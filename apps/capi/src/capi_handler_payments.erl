-module(capi_handler_payments).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [general_error/2, logic_error/2]).

-define(DEFAULT_PROCESSING_DEADLINE, <<"30m">>).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('CreatePayment' = OperationID, Req, Context) ->
    InvoiceID     = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    PartyID       = capi_handler_utils:get_party_id(Context),
    Result =
        try
            create_payment(InvoiceID, PartyID, PaymentParams, Context, OperationID)
        catch
            throw:Error when
                Error =:= invalid_token orelse
                Error =:= invalid_payment_session orelse
                Error =:= invalid_processing_deadline
            ->
                {error, Error}
        end,
    case Result of
        {ok, Payment} ->
            {ok, {201, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)};
                #payproc_InvoicePaymentPending{} ->
                    ErrorResp = logic_error(
                        invoicePaymentPending,
                        <<"Invoice payment pending">>
                    ),
                    {ok, ErrorResp};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvalidContractStatus{} ->
                    ErrorResp = logic_error(
                        invalidContractStatus,
                        <<"Invalid contract status">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidRecurrentParentPayment{} ->
                    ErrorResp = logic_error(
                        invalidRecurrentParent,
                        <<"Specified recurrent parent is invalid">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end;
        {error, invalid_token} ->
            {ok, logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            )};
        {error, invalid_payment_session} ->
            {ok, logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            )};
        {error, invalid_processing_deadline} ->
            {ok, logic_error(
                invalidProcessingDeadline,
                <<"Specified processing deadline is invalid">>
            )};
        {error, {external_id_conflict, PaymentID, ExternalID}} ->
            {ok, logic_error(externalIDConflict, {PaymentID, ExternalID})}
    end;

process_request('GetPayments', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    case capi_handler_utils:get_invoice_by_id(InvoiceID, Context) of
        {ok, #'payproc_Invoice'{payments = Payments}} ->
            {ok, {200, #{}, [decode_invoice_payment(InvoiceID, P, Context) || P <- Payments]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetPaymentByID', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    case capi_handler_utils:get_payment_by_id(InvoiceID, maps:get(paymentID, Req), Context) of
        {ok, Payment} ->
            {ok, {200, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetRefundByExternalID', Req, Context) ->
    ExternalID = maps:get(externalID, Req),
    case get_refund_by_external_id(ExternalID, Context) of
        {ok, Refund} ->
            {ok, {200, #{}, capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {error, internal_id_not_found} ->
             {ok, general_error(404, <<"Refund not found">>)};
        {error, payment_not_found} ->
            {ok, general_error(404, <<"Payment not found">>)};
        {error, invoice_not_found} ->
            {ok, general_error(404, <<"Invoice not found">>)};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentRefundNotFound{} ->
                    {ok, general_error(404, <<"Refund not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetPaymentByExternalID', Req, Context) ->
    ExternalID = maps:get(externalID, Req),
    case get_payment_by_external_id(ExternalID, Context) of
        {ok, InvoiceID, Payment} ->
            {ok, {200, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
        {error, internal_id_not_found} ->
            {ok, general_error(404, <<"Payment not found">>)};
        {error, invoice_not_found} ->
            {ok, general_error(404, <<"Invoice not found">>)};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('CancelPayment', Req, Context) ->
    CallArgs = [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))],
    Call = {invoicing, 'CancelPayment', CallArgs},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, logic_error(invalidPaymentStatus, <<"Invalid payment status">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)}
            end
    end;

process_request('CapturePayment', Req, Context) ->
    CaptureParams = maps:get('CaptureParams', Req),
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    try
        CallArgs = [
            InvoiceID,
            PaymentID,
            #payproc_InvoicePaymentCaptureParams{
                reason = maps:get(<<"reason">>, CaptureParams),
                cash = encode_optional_cash(CaptureParams, InvoiceID, PaymentID, Context),
                cart = capi_handler_encoder:encode_invoice_cart(CaptureParams)
            }
        ],
        Call = {invoicing, 'CapturePayment', CallArgs},
        capi_handler_utils:service_call_with([user_info], Call, Context)
    of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, logic_error(invalidPaymentStatus, <<"Invalid payment status">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InconsistentCaptureCurrency{payment_currency = PaymentCurrency} ->
                    {ok, logic_error(
                        inconsistentCaptureCurrency,
                        io_lib:format("Correct currency: ~p", [PaymentCurrency])
                    )};
                #payproc_AmountExceededCaptureBalance{payment_amount = PaymentAmount} ->
                    {ok, logic_error(
                        amountExceededCaptureBalance,
                        io_lib:format("Max amount: ~p", [PaymentAmount])
                    )}
            end
    catch
        throw:invoice_cart_empty ->
            {ok, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}
    end;

process_request('CreateRefund' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    RefundParams = maps:get('RefundParams', Req),
    try create_refund(InvoiceID, PaymentID, RefundParams, Context, OperationID) of
        {ok, Refund} ->
            {ok, {201, #{}, capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvalidContractStatus{} ->
                    ErrorResp = logic_error(
                        invalidContractStatus,
                         <<"Invalid contract status">>
                    ),
                    {ok, ErrorResp};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidPaymentStatus{} ->
                    ErrorResp = logic_error(
                        invalidPaymentStatus,
                        <<"Invalid invoice payment status">>
                    ),
                    {ok, ErrorResp};
                #payproc_InsufficientAccountBalance{} ->
                    {ok, logic_error(
                        insufficentAccountBalance,
                        <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                    )};
                #payproc_InvoicePaymentAmountExceeded{} ->
                    ErrorResp = logic_error(
                        invoicePaymentAmountExceeded,
                        <<"Payment amount exceeded">>
                    ),
                    {ok, ErrorResp};
                #payproc_InconsistentRefundCurrency{} ->
                    ErrorResp = logic_error(
                        inconsistentRefundCurrency,
                        <<"Inconsistent refund currency">>
                    ),
                    {ok, ErrorResp};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)}
            end;
        {error, {external_id_conflict, RefundID, ExternalID}} ->
            {ok, logic_error(externalIDConflict, {RefundID, ExternalID})}
    catch
        throw:invoice_cart_empty ->
            {ok, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}
    end;

process_request('GetRefunds', Req, Context) ->
    case capi_handler_utils:get_payment_by_id(maps:get(invoiceID, Req), maps:get(paymentID, Req), Context) of
        {ok, #payproc_InvoicePayment{refunds = Refunds}} ->
            {ok, {200, #{}, [
                capi_handler_decoder_invoicing:decode_refund(R, Context)
                || #payproc_InvoicePaymentRefund{refund =  R} <- Refunds
            ]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetRefundByID', Req, Context) ->
    case capi_handler_utils:get_refund_by_id(
        maps:get(invoiceID, Req),
        maps:get(paymentID, Req),
        maps:get(refundID, Req),
        Context
    ) of
        {ok, Refund} ->
            {ok, {200, #{}, capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentRefundNotFound{} ->
                    {ok, general_error(404, <<"Invoice payment refund not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetChargebacks', Req, Context) ->
    DecodeChargebackFun = fun(C) ->
        capi_handler_decoder_invoicing:decode_chargeback(C#payproc_InvoicePaymentChargeback.chargeback, Context)
    end,
    case capi_handler_utils:get_payment_by_id(maps:get(invoiceID, Req), maps:get(paymentID, Req), Context) of
        {ok, #payproc_InvoicePayment{chargebacks = Chargebacks}} ->
            {ok, {200, #{}, [DecodeChargebackFun(C) || C <- Chargebacks]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetChargebackByID', Req, Context) ->
    Call = {
        invoicing,
        'GetPaymentChargeback',
        [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(chargebackID, Req)]
    },
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, Chargeback} ->
            {ok, {200, #{}, capi_handler_decoder_invoicing:decode_chargeback(Chargeback, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentChargebackNotFound{} ->
                    {ok, general_error(404, <<"Invoice payment chargeback not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

create_payment(InvoiceID, PartyID, #{<<"externalID">> := ExternalID} = PaymentParams, Context, BenderPrefix) ->
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, ExternalID),
    {Payer, PaymentToolThrift} = decrypt_payer(maps:get(<<"payer">>, PaymentParams)),
    PaymentParamsDecrypted = PaymentParams#{<<"payer">> => Payer},
    Hash = erlang:phash2(PaymentParams),
    Schema = capi_feature_schemas:payment(),
    {Features, _} = capi_idemp_features:read(Schema, PaymentParamsDecrypted),
    Params = {Hash, Features},
    #{woody_context := WoodyCtx} = Context,
    CtxData = #{<<"invoice_id">> => InvoiceID},
    case capi_bender:gen_by_sequence(IdempotentKey, InvoiceID, Params, WoodyCtx, CtxData) of
        {ok, ID} ->
            start_payment(ID, InvoiceID, ExternalID, PaymentParamsDecrypted, PaymentToolThrift, Context);
        {error, {external_id_conflict, ID, undefined}} ->
            {error, {external_id_conflict, ID, ExternalID}};
        {error, {external_id_conflict, ID, Difference}} ->
            ReadableDiff = capi_idemp_features:list_diff_fields(Schema, Difference),
            logger:warning("This externalID: ~p, used in another request.~nDifference: ~p", [ID, ReadableDiff]),
            {error, {external_id_conflict, ID, ExternalID}}
    end;
create_payment(InvoiceID, _PartyID, PaymentParams, #{woody_context := WoodyCtx} = Context, _) ->
    ExternalID = undefined,
    {Payer, PaymentToolThrift} = decrypt_payer(maps:get(<<"payer">>, PaymentParams)),
    PaymentParamsDecrypted = PaymentParams#{<<"payer">> => Payer},
    {ok, {ID, _}} = bender_generator_client:gen_sequence(InvoiceID, WoodyCtx, #{}),
    PaymentID = <<InvoiceID/binary, ".", ID/binary>>,
    start_payment(PaymentID, InvoiceID, ExternalID, PaymentParamsDecrypted, PaymentToolThrift, Context).

start_payment(ID, InvoiceID, ExternalID, PaymentParamsDecrypted, PaymentToolThrift, Context) ->
    InvoicePaymentParams = encode_invoice_payment_params(ID, ExternalID, PaymentParamsDecrypted, PaymentToolThrift),
    Call = {invoicing, 'StartPayment', [InvoiceID, InvoicePaymentParams]},
    capi_handler_utils:service_call_with([user_info], Call, Context).

decrypt_payer(#{<<"payerType">> := <<"PaymentResourcePayer">>} = Payer) ->
    #{<<"paymentToolToken">> := Token} = Payer,
    case capi_crypto:decrypt_payment_tool_token(Token) of
        {ok, PaymentToolThrift} ->
            PaymentTool = capi_handler_decoder_party:decode_payment_tool(PaymentToolThrift),
            {Payer#{<<"paymentTool">> => PaymentTool}, PaymentToolThrift};
        {error, {decryption_failed, Error}} ->
            logger:warning("Payment tool token decryption failed: ~p", [Error]),
            erlang:throw(invalid_token)
    end;
decrypt_payer(CustomerOrRecurrentPayer) ->
    {CustomerOrRecurrentPayer, undefined}.

encode_invoice_payment_params(ID, ExternalID, PaymentParams, PaymentToolThrift) ->
    Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
    Payer = genlib_map:get(<<"payer">>, PaymentParams),
    #payproc_InvoicePaymentParams{
        id                  = ID,
        external_id         = ExternalID,
        payer               = encode_payer_params({Payer, PaymentToolThrift}),
        flow                = encode_flow(Flow),
        make_recurrent      = genlib_map:get(<<"makeRecurrent">>, PaymentParams, false),
        context             = capi_handler_encoder:encode_payment_context(PaymentParams),
        processing_deadline = encode_processing_deadline(
            genlib_map:get(<<"processingDeadline">>, PaymentParams, default_processing_deadline())
        )
    }.

encode_payer_params({#{
    <<"payerType" >> := <<"CustomerPayer">>,
    <<"customerID">> := ID
}, _}) ->
    {customer, #payproc_CustomerPayerParams{customer_id = ID}};

encode_payer_params({#{
    <<"payerType"       >>  := <<"PaymentResourcePayer">>,
    <<"paymentSession"  >>  := EncodedSession,
    <<"contactInfo"     >>  := ContactInfo
}, PaymentToolThrift}) ->
    {ClientInfo, PaymentSession} = capi_handler_utils:unwrap_payment_session(EncodedSession),
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentToolThrift,
            payment_session_id = PaymentSession,
            client_info = capi_handler_encoder:encode_client_info(ClientInfo)
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }};

encode_payer_params({#{
    <<"payerType"             >> := <<"RecurrentPayer">>,
    <<"recurrentParentPayment">> := RecurrentParent,
    <<"contactInfo"           >> := ContactInfo
}, _}) ->
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

encode_optional_cash(Params = #{<<"amount">> := _, <<"currency">> := _}, _, _, _) ->
    capi_handler_encoder:encode_cash(Params);
encode_optional_cash(Params = #{<<"amount">> := _}, InvoiceID, PaymentID, Context) ->
    {ok, #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{
            cost = #domain_Cash{currency = Currency}
        }
    }} = capi_handler_utils:get_payment_by_id(InvoiceID, PaymentID, Context),
    capi_handler_encoder:encode_cash(Params#{<<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)});
encode_optional_cash(_, _, _, _) ->
    undefined.

%%

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{payment = Payment}, Context) ->
    capi_handler_decoder_invoicing:decode_payment(InvoiceID, Payment, Context).

get_refund_by_external_id(ExternalID, #{woody_context := WoodyContext} = Context) ->
    PartyID   = capi_handler_utils:get_party_id(Context),
    RefundKey = capi_bender:get_idempotent_key('CreateRefund', PartyID, ExternalID),
    case capi_bender:get_internal_id(RefundKey, WoodyContext) of
        {ok, RefundID, CtxData} ->
            InvoiceID = maps:get(<<"invoice_id">>, CtxData, undefined),
            PaymentID = maps:get(<<"payment_id">>, CtxData, undefined),
            get_refund(InvoiceID, PaymentID, RefundID, Context);
        Error ->
            Error
    end.

get_refund(undefined, _, _, _) ->
    {error, invoice_not_found};
get_refund(_, undefined, _, _) ->
    {error, payment_not_found};
get_refund(InvoiceID, PaymentID, RefundID, Context) ->
    capi_handler_utils:get_refund_by_id(InvoiceID, PaymentID, RefundID, Context).

-spec get_payment_by_external_id(binary(), capi_handler:processing_context()) ->
    woody:result().

get_payment_by_external_id(ExternalID, #{woody_context := WoodyContext} = Context) ->
    PartyID    = capi_handler_utils:get_party_id(Context),
    PaymentKey = capi_bender:get_idempotent_key('CreatePayment', PartyID, ExternalID),
    case capi_bender:get_internal_id(PaymentKey, WoodyContext) of
        {ok, PaymentID, CtxData} ->
            InvoiceID = maps:get(<<"invoice_id">>, CtxData, undefined),
            get_payment(InvoiceID, PaymentID, Context);
        Error ->
            Error
    end.

get_payment(undefined, _, _) ->
    {error, invoice_not_found};
get_payment(InvoiceID, PaymentID, Context) ->
    case capi_handler_utils:get_payment_by_id(InvoiceID, PaymentID, Context) of
        {ok, Payment} ->
            {ok, InvoiceID, Payment};
        Error ->
            Error
    end.

encode_processing_deadline(Deadline) ->
    case capi_utils:parse_deadline(Deadline) of
        {ok, undefined} ->
            undefined;
        {error, bad_deadline} ->
            throw(invalid_processing_deadline);
        {ok, ProcessingDeadline} ->
            woody_deadline:to_binary(ProcessingDeadline)
    end.

default_processing_deadline() ->
    genlib_app:env(capi, default_processing_deadline, ?DEFAULT_PROCESSING_DEADLINE).

create_refund(InvoiceID, PaymentID, #{<<"externalID">> := ExternalID} = RefundParams, Context, BenderPrefix) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    #{woody_context := WoodyCtx} = Context,
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, ExternalID),
    SequenceID = create_sequence_id([InvoiceID, PaymentID], BenderPrefix),
    SequenceParams = #{minimum => 100},
    Hash = erlang:phash2(RefundParams),
    RefundParamsFull = RefundParams#{<<"invoiceID">> => invoiceID, <<"paymentID">> => PaymentID},
    Schema = capi_feature_schemas:refund(),
    {Features, _} = capi_idemp_features:read_features(Schema, RefundParamsFull),
    Params = {Hash, Features},
    case capi_bender:gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyCtx, #{}, SequenceParams) of
        {ok, ID} ->
            refund_payment(ID, InvoiceID, PaymentID, RefundParams, Context);
        {error, {external_id_conflict, ID, undefined}} ->
            {error, {external_id_conflict, ID, ExternalID}};
        {error, {external_id_conflict, ID, Difference}} ->
            ReadableDiff = capi_idemp_features:list_diff_fields(Schema, Difference),
            logger:warning("This externalID: ~p, used in another request.~nDifference: ~p", [ID, ReadableDiff]),
            {error, {external_id_conflict, ID, ExternalID}}
    end;
create_refund(InvoiceID, PaymentID, RefundParams, Context, BenderPrefix) ->
    #{woody_context := WoodyCtx} = Context,
    SequenceID = create_sequence_id([InvoiceID, PaymentID], BenderPrefix),
    SequenceParams = #{minimum => 100},
    {ok, {ID, _}} = bender_generator_client:gen_sequence(SequenceID, WoodyCtx, SequenceParams),
    refund_payment(ID, InvoiceID, PaymentID, RefundParams, Context).

refund_payment(RefundID, InvoiceID, PaymentID, RefundParams, Context) ->
    ExternalID = maps:get(<<"externalID">>, RefundParams, undefined),
    Params = #payproc_InvoicePaymentRefundParams{
        external_id = ExternalID,
        reason = genlib_map:get(<<"reason">>, RefundParams),
        cash = encode_optional_cash(RefundParams, InvoiceID, PaymentID, Context),
        cart = capi_handler_encoder:encode_invoice_cart(RefundParams)
    },
    Call = {invoicing, 'RefundPayment', [
        InvoiceID,
        PaymentID,
        Params#payproc_InvoicePaymentRefundParams{id = RefundID}
    ]},
    capi_handler_utils:service_call_with([user_info], Call, Context).

%%

create_sequence_id([Identifier | Rest], BenderPrefix) ->
    Next = create_sequence_id(Rest, BenderPrefix),
    <<Identifier/binary, ".", Next/binary>>;
create_sequence_id([], BenderPrefix) ->
    genlib:to_binary(BenderPrefix).
