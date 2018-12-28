-module(capi_handler_payments).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('CreatePayment', Req, Context, _) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
    Result =
        try
            Params =  #payproc_InvoicePaymentParams{
                'payer' = encode_payer_params(genlib_map:get(<<"payer">>, PaymentParams)),
                'flow' = encode_flow(Flow),
                'make_recurrent' = genlib_map:get(<<"makeRecurrent">>, PaymentParams, false)
            },
            Call = {invoicing, 'StartPayment', [InvoiceID, Params]},
            service_call_with([user_info], Call, Context)
        catch
            throw:Error when
                Error =:= invalid_token orelse
                Error =:= invalid_payment_session
            ->
                {error, Error}
        end,

    case Result of
        {ok, Payment} ->
            {ok, {201, [], decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvoicePaymentPending{} ->
                    {ok, {400, [], logic_error(invoicePaymentPending, <<"Invoice payment pending">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidContractStatus{} ->
                    {ok, {400, [], logic_error(invalidContractStatus, <<"Invalid contract status">>)}};
                #payproc_InvalidRecurrentParentPayment{} ->
                    {ok, {400, [], logic_error(invalidRecurrentParent, <<"Specified recurrent parent is invalid">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end;
        {error, invalid_token} ->
            {ok, {400, [], logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            )}};
        {error, invalid_payment_session} ->
            {ok, {400, [], logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            )}}
    end;

process_request('GetPayments', Req, Context, _) ->
    InvoiceID = maps:get(invoiceID, Req),
    case get_invoice_by_id(InvoiceID, Context) of
        {ok, #'payproc_Invoice'{payments = Payments}} ->
            {ok, {200, [], [decode_invoice_payment(InvoiceID, P, Context) || P <- Payments]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetPaymentByID', Req, Context, _) ->
    InvoiceID = maps:get(invoiceID, Req),
    case get_payment_by_id(InvoiceID, maps:get(paymentID, Req), Context) of
        {ok, Payment} ->
            {ok, {200, [], decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('CancelPayment', Req, Context, _) ->
    CallArgs = [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))],
    Call = {invoicing, 'CancelPayment', CallArgs},
    case service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, [], logic_error(invalidPaymentStatus, <<"Invalid payment status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, [], logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CapturePayment', Req, Context, _) ->
    CallArgs = [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))],
    Call = {invoicing, 'CapturePayment', CallArgs},
    case service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, [], logic_error(invalidPaymentStatus, <<"Invalid payment status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, [], logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CreateRefund', Req, Context, _) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    RefundParams = maps:get('RefundParams', Req),
    Params = #payproc_InvoicePaymentRefundParams{
        reason = genlib_map:get(<<"reason">>, RefundParams),
        cash = encode_optional_refund_cash(RefundParams, InvoiceID, PaymentID, Context)
    },
    Call = {invoicing, 'RefundPayment', [InvoiceID, PaymentID, Params]},
    case service_call_with([user_info], Call, Context) of
        {ok, Refund} ->
            {ok, {201, [], decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidContractStatus{} ->
                    {ok, {400, [], logic_error(invalidContractStatus, <<"Invalid contract status">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, [], logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, [], logic_error(invalidPaymentStatus, <<"Invalid invoice payment status">>)}};
                #payproc_InsufficientAccountBalance{} ->
                    {ok, {400, [], logic_error(
                        insufficentAccountBalance,
                        <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                    )}};
                #payproc_InvoicePaymentAmountExceeded{} ->
                    {ok, {400, [], logic_error(invoicePaymentAmountExceeded, <<"Payment amount exceeded">>)}};
                #payproc_InconsistentRefundCurrency{} ->
                    {ok, {400, [], logic_error(inconsistentRefundCurrency, <<"Inconsistent refund currency">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetRefunds', Req, Context, _) ->
    case get_payment_by_id(maps:get(invoiceID, Req), maps:get(paymentID, Req), Context) of
        {ok, #payproc_InvoicePayment{refunds = Refunds}} ->
            {ok, {200, [], [decode_refund(R, Context) || R <- Refunds]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetRefundByID', Req, Context, _) ->
    Call =
        {invoicing, 'GetPaymentRefund', [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(refundID, Req)]},
    case service_call_with([user_info], Call, Context) of
        {ok, Refund} ->
            {ok, {200, [], decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentRefundNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice payment refund not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
