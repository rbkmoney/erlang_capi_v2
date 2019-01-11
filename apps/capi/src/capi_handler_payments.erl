-module(capi_handler_payments).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

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
            capi_handler_utils:service_call_with([user_info], Call, Context)
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
                    {ok, {400, [], capi_handler_utils:logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvoicePaymentPending{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        invoicePaymentPending,
                        <<"Invoice payment pending">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidContractStatus{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        invalidContractStatus,
                        <<"Invalid contract status">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InvalidRecurrentParentPayment{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        invalidRecurrentParent,
                        <<"Specified recurrent parent is invalid">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}}
            end;
        {error, invalid_token} ->
            {ok, {400, [], capi_handler_utils:logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            )}};
        {error, invalid_payment_session} ->
            {ok, {400, [], capi_handler_utils:logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            )}}
    end;

process_request('GetPayments', Req, Context, _) ->
    InvoiceID = maps:get(invoiceID, Req),
    case capi_handler_utils:get_invoice_by_id(InvoiceID, Context) of
        {ok, #'payproc_Invoice'{payments = Payments}} ->
            {ok, {200, [], [decode_invoice_payment(InvoiceID, P, Context) || P <- Payments]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetPaymentByID', Req, Context, _) ->
    InvoiceID = maps:get(invoiceID, Req),
    case capi_handler_utils:get_payment_by_id(InvoiceID, maps:get(paymentID, Req), Context) of
        {ok, Payment} ->
            {ok, {200, [], decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payment not found">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('CancelPayment', Req, Context, _) ->
    CallArgs = [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))],
    Call = {invoicing, 'CancelPayment', CallArgs},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payment not found">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPaymentStatus, <<"Invalid payment status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
                #payproc_OperationNotPermitted{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CapturePayment', Req, Context, _) ->
    CallArgs = [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))],
    Call = {invoicing, 'CapturePayment', CallArgs},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payment not found">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPaymentStatus, <<"Invalid payment status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
                #payproc_OperationNotPermitted{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
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
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, Refund} ->
            {ok, {201, [], capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidContractStatus{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        invalidContractStatus,
                         <<"Invalid contract status">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_OperationNotPermitted{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InvalidPaymentStatus{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        invalidPaymentStatus,
                        <<"Invalid invoice payment status">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InsufficientAccountBalance{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(
                        insufficentAccountBalance,
                        <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                    )}};
                #payproc_InvoicePaymentAmountExceeded{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        invoicePaymentAmountExceeded,
                        <<"Payment amount exceeded">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #payproc_InconsistentRefundCurrency{} ->
                    ErrorMsg = capi_handler_utils:logic_error(
                        inconsistentRefundCurrency,
                        <<"Inconsistent refund currency">>
                    ),
                    {ok, {400, [], ErrorMsg}};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}}
            end
    end;

process_request('GetRefunds', Req, Context, _) ->
    case capi_handler_utils:get_payment_by_id(maps:get(invoiceID, Req), maps:get(paymentID, Req), Context) of
        {ok, #payproc_InvoicePayment{refunds = Refunds}} ->
            {ok, {200, [], [capi_handler_decoder_invoicing:decode_refund(R, Context) || R <- Refunds]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetRefundByID', Req, Context, _) ->
    Call =
        {invoicing, 'GetPaymentRefund', [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(refundID, Req)]},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, Refund} ->
            {ok, {200, [], capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentRefundNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice payment refund not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Invoice not found">>)}}
            end
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

%%

encode_payer_params(#{
    <<"payerType" >> := <<"CustomerPayer">>,
    <<"customerID">> := ID
}) ->
    {customer, #payproc_CustomerPayerParams{customer_id = ID}};

encode_payer_params(#{
    <<"payerType"       >> := <<"PaymentResourcePayer">>,
    <<"paymentToolToken">> := Token,
    <<"paymentSession"  >> := EncodedSession,
    <<"contactInfo"     >> := ContactInfo
}) ->
    PaymentTool = capi_handler_encoder:encode_payment_tool_token(Token),
    {ClientInfo, PaymentSession} = capi_handler_utils:unwrap_payment_session(EncodedSession),
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = capi_handler_encoder:encode_client_info(ClientInfo)
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }};

encode_payer_params(#{
    <<"payerType"             >> := <<"RecurrentPayer">>,
    <<"recurrentParentPayment">> := RecurrentParent,
    <<"contactInfo"           >> := ContactInfo
}) ->
    #{
        <<"invoiceID">> := InvoiceID,
        <<"paymentID">> := PaymentID
    } = RecurrentParent,
    {recurrent, #payproc_RecurrentPayerParams{
        recurrent_parent = #domain_RecurrentParentPayment{
            invoice_id = InvoiceID,
            payment_id = PaymentID
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }}.

encode_flow(#{<<"type">> := <<"PaymentFlowInstant">>}) ->
    {instant, #payproc_InvoicePaymentParamsFlowInstant{}};

encode_flow(#{<<"type">> := <<"PaymentFlowHold">>} = Entity) ->
    OnHoldExpiration = maps:get(<<"onHoldExpiration">>, Entity, <<"cancel">>),
    {hold, #payproc_InvoicePaymentParamsFlowHold{
        on_hold_expiration = binary_to_existing_atom(OnHoldExpiration, utf8)
    }}.

encode_optional_refund_cash(Params = #{<<"amount">> := _, <<"currency">> := _}, _, _, _) ->
    capi_handler_encoder:encode_cash(Params);
encode_optional_refund_cash(Params = #{<<"amount">> := _}, InvoiceID, PaymentID, Context) ->
    {ok, #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{
            cost = #domain_Cash{currency = Currency}
        }
    }} = capi_handler_utils:get_payment_by_id(InvoiceID, PaymentID, Context),
    capi_handler_encoder:encode_cash(Params#{<<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)});
encode_optional_refund_cash(_, _, _, _) ->
    undefined.

%%

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{payment = Payment}, Context) ->
    capi_handler_decoder_invoicing:decode_payment(InvoiceID, Payment, Context).

