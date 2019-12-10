-module(capi_real_handler).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_cds_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").
-include_lib("damsel/include/dmsl_geo_ip_thrift.hrl").
-include_lib("reporter_proto/include/reporter_base_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_provider_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").

-behaviour(swag_server_logic_handler).

%% API callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-define(DEFAULT_INVOICE_META, #{}).
-define(DEFAULT_INVOICE_TPL_META, #{}).
-define(DEFAULT_URL_LIFETIME, 60). % seconds

-define(payment_institution_ref(PaymentInstitutionID),
    #domain_PaymentInstitutionRef{id = PaymentInstitutionID}).

-define(CAPI_NS, <<"com.rbkmoney.capi">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key(), handler_opts()) ->
    Result :: false | {true, capi_auth:context()}.

authorize_api_key(OperationID, ApiKey, _HandlerOpts) ->
    _ = capi_utils:logtag_process(operation_id, OperationID),
    capi_auth:authorize_api_key(OperationID, ApiKey).

-type request_data() :: #{atom() | binary() => term()}.
-type handler_opts()        :: swag_server:handler_opts(_).

-spec handle_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: swag_server:request_context(),
    handler_opts()
) ->
    {ok | error, swag_server:response()}.

handle_request(OperationID, Req, Context, _HandlerOpts) ->
    _ = logger:info("Processing request ~p", [OperationID]),
    try
        case capi_auth:authorize_operation(OperationID, Req, get_auth_context(Context)) of
            ok ->
                ReqContext = create_context(Req, get_auth_context(Context)),
                process_request(OperationID, Req, Context, ReqContext);
            {error, _} = Error ->
                _ = logger:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {error, {401, #{}, general_error(<<"Unauthorized operation">>)}}
        end
    catch
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    end.

-spec process_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: swag_server:request_context(),
    ReqCtx :: woody_context:ctx()
) ->
    {Code :: non_neg_integer(), Headers :: #{}, Response :: #{}}.

process_request('CreateInvoice' = OperationID, Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    try
        create_invoice(PartyID, maps:get('InvoiceParams', Req), Context, ReqCtx, OperationID)
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, #{}, make_invoice_and_token(Invoice, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, #{}, logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    catch
        invoice_cart_empty ->
            {ok, {400, #{}, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        invalid_invoice_cost ->
            {ok, {400, #{}, logic_error(invalidInvoiceCost, <<"Invalid invoice amount">>)}}
    end;

process_request('CreatePayment' = OperationID, Req, Context, ReqCtx) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    PartyID = get_party_id(Context),
    Result = try
        create_payment(PartyID, InvoiceID, PaymentParams, Context, ReqCtx, OperationID)
    catch
        throw:Error when Error =:= invalid_token orelse Error =:= invalid_payment_session ->
            {error, Error}
    end,

    case Result of
        {ok, Payment} ->
            {ok, {201, #{}, decode_invoice_payment(InvoiceID, Payment)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, #{}, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvoicePaymentPending{} ->
                    {ok, {400, #{}, logic_error(invoicePaymentPending, <<"Invoice payment pending">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end;
        {error, invalid_token} ->
            {ok, {400, #{}, logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            )}};
        {error, invalid_payment_session} ->
            {ok, {400, #{}, logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            )}}
    end;

process_request('CreateInvoiceAccessToken', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{}} ->
            Token = make_invoice_access_token(InvoiceID, PartyID, Context),
            {ok, {201, #{}, Token}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetInvoiceByID', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            Resp = decode_invoice(Invoice),
            {ok, {200, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('FulfillInvoice', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),

    Params = maps:get('Reason', Req),
    Reason = maps:get(<<"reason">>, Params),

    UserInfo = get_user_info(Context),

    Result = service_call(
        invoicing,
        'Fulfill',
        [UserInfo, InvoiceID, Reason],
        ReqCtx
    ),
    case Result of
        {ok, _} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, #{}, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('RescindInvoice', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),

    Params = maps:get('Reason', Req),
    Reason = maps:get(<<"reason">>, Params),

    UserInfo = get_user_info(Context),
    Result = service_call(
        invoicing,
        'Rescind',
        [UserInfo, InvoiceID, Reason],
        ReqCtx
    ),
    case Result of
        {ok, _} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, #{}, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvoicePaymentPending{} ->
                    {ok, {400, #{}, logic_error(invoicePaymentPending, <<"Invoice payment pending">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetInvoiceEvents', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    GetterFun = fun(Range) ->
        service_call(
            invoicing,
            'GetEvents',
            [UserInfo, InvoiceID, Range],
            ReqCtx
        )
    end,
    DecodingContext = #{
        user_info => UserInfo,
        req_ctx => ReqCtx
    },
    Result  = collect_events(
        maps:get(limit, Req),
        genlib_map:get(eventID, Req),
        GetterFun,
        fun decode_invoice_event/2,
        DecodingContext
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, #{}, Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{},  general_error(<<"Invoice not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Event not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetInvoicePaymentMethods', Req, Context, ReqCtx) ->
    Result = construct_payment_methods(
        invoicing,
        [
            get_user_info(Context),
            maps:get(invoiceID, Req)
        ],
        ReqCtx
    ),
    case Result of
        {ok, PaymentMethods} when is_list(PaymentMethods) ->
            {ok, {200, #{}, PaymentMethods}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetPayments', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{payments = Payments}} ->
            Resp = [decode_invoice_payment(InvoiceID, P) || P <- Payments],
            {ok, {200, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetPaymentByID', Req, Context, ReqCtx) ->
    PaymentID = maps:get(paymentID, Req),
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID),
    case Result of
        {ok, Payment} ->
            Resp = decode_invoice_payment(InvoiceID, Payment),
            {ok, {200, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Payment not found">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('CancelPayment', Req, Context, ReqCtx) ->
    PaymentID = maps:get(paymentID, Req),
    InvoiceID = maps:get(invoiceID, Req),
    Params = maps:get('Reason', Req),
    Reason = maps:get(<<"reason">>, Params),
    UserInfo = get_user_info(Context),

    Result = service_call(
        invoicing,
        'CancelPayment',
        [UserInfo, InvoiceID, PaymentID, Reason],
        ReqCtx
    ),
    case Result of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Payment not found">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPaymentStatus, <<"Invalid payment status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, #{}, logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CapturePayment', Req, Context, ReqCtx) ->
    PaymentID = maps:get(paymentID, Req),
    InvoiceID = maps:get(invoiceID, Req),
    Params = maps:get('Reason', Req),
    Reason = maps:get(<<"reason">>, Params),
    UserInfo = get_user_info(Context),
    CaptureParams = #payproc_InvoicePaymentCaptureParams{
        reason = Reason
    },

    Result = service_call(
        invoicing,
        'CapturePaymentNew',
        [UserInfo, InvoiceID, PaymentID, CaptureParams],
        ReqCtx
    ),
    case Result of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Payment not found">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPaymentStatus, <<"Invalid payment status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, #{}, logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('SearchInvoices', Req, Context, ReqCtx) ->
    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"invoice_id">> =>  genlib_map:get('invoiceID', Req),
        <<"from_time">> => get_time('fromTime', Req),
        <<"to_time">> => get_time('toTime', Req),
        <<"invoice_status">> => genlib_map:get('invoiceStatus', Req),
        <<"payment_status">> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow">> => genlib_map:get('paymentFlow', Req),
        <<"payment_method">> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id">> => genlib_map:get('customerID', Req),
        <<"payment_id">> => genlib_map:get('paymentID', Req),
        <<"payment_email">> => genlib_map:get('payerEmail', Req),
        <<"payment_ip">> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint">> => genlib_map:get('payerFingerprint', Req),
        <<"payment_pan_mask">> => genlib_map:get('cardNumberMask', Req),
        <<"payment_amount">> => genlib_map:get('paymentAmount', Req),
        <<"invoice_amount">> => genlib_map:get('invoiceAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_bin"              >> => genlib_map:get('bin', Req)
    },
    Opts = #{
        thrift_fun => 'GetInvoices',
        decode_fun => fun decode_stat_invoice/1
    },
    process_search_request(invoices, Query, Req, ReqCtx, Opts);

process_request('SearchPayments', Req, Context, ReqCtx) ->
    Query = #{
        <<"merchant_id">>   => get_party_id(Context),
        <<"shop_id">>       => genlib_map:get('shopID', Req),
        <<"invoice_id">>    => genlib_map:get('invoiceID', Req),
        <<"from_time">>     => get_time('fromTime', Req),
        <<"to_time">>       => get_time('toTime', Req),
        <<"payment_status">>            => genlib_map:get('paymentStatus', Req),
        <<"payment_flow">>              => genlib_map:get('paymentFlow', Req),
        <<"payment_method">> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id">>       => genlib_map:get('customerID', Req),
        <<"payment_id">>                => genlib_map:get('paymentID', Req),
        <<"payment_email">>             => genlib_map:get('payerEmail', Req),
        <<"payment_ip">>                => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint">>       => genlib_map:get('payerFingerprint', Req),
        <<"payment_pan_mask">>          => genlib_map:get('cardNumberMask', Req),
        <<"payment_amount">>            => genlib_map:get('paymentAmount', Req),
        <<"payment_token_provider">>    => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system">>            => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_bin">>               => genlib_map:get('bin', Req)
    },
    Opts = #{
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_payment/1
    },
    process_search_request(payments, Query, Req, ReqCtx, Opts);

process_request('SearchPayouts', Req, Context, ReqCtx) ->
    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"from_time">> => get_time('fromTime', Req),
        <<"to_time">> => get_time('toTime', Req),
        <<"payout_status">> => genlib_map:get('payoutStatus', Req),
        <<"payout_id">> => genlib_map:get('payoutID', Req),
        <<"payout_type">> => encode_payout_type(genlib_map:get('payoutToolType', Req))
    },
    Opts = #{
        thrift_fun => 'GetPayouts',
        decode_fun => fun decode_stat_payout/1
    },
    process_search_request(payouts, Query, Req, ReqCtx, Opts);

process_request('GetPaymentConversionStats', Req, Context, ReqCtx) ->
    process_merchant_stat(payments_conversion_stat, Req, Context, ReqCtx);

process_request('GetPaymentRevenueStats', Req, Context, ReqCtx) ->
    process_merchant_stat(payments_turnover, Req, Context, ReqCtx);

process_request('GetPaymentGeoStats', Req, Context, ReqCtx) ->
    process_merchant_stat(payments_geo_stat, Req, Context, ReqCtx);

process_request('GetPaymentRateStats', Req, Context, ReqCtx) ->
    process_merchant_stat(customers_rate_stat, Req, Context, ReqCtx);

process_request('GetPaymentMethodStats', Req, Context, ReqCtx) ->
    bankCard =  maps:get(paymentMethod, Req),
    StatType = payments_pmt_cards_stat,
    process_merchant_stat(StatType, Req, Context, ReqCtx);

process_request('GetLocationsNames', Req, _Context, ReqCtx) ->
    Language = maps:get('language', Req),
    GeoIDs = ordsets:from_list(maps:get('geoIDs', Req)),

    Result = service_call(
        geo_ip_service,
        'GetLocationName',
        [GeoIDs, Language],
        ReqCtx
    ),

    case Result of
        {ok, LocationNames = #{}} ->
            PreparedLocationNames = maps:fold(
                fun(GeoID, Name, Acc) -> [decode_location_name(GeoID, Name) | Acc] end,
                [],
                LocationNames
            ),
            {ok, {200, #{}, PreparedLocationNames}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}}
    end;

process_request('CreateRefund' = OperationID, Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    RefundParams = maps:get('RefundParams', Req),
    UserInfo = get_user_info(Context),
    EncodingContext = #{
        req_ctx => ReqCtx,
        user_info => UserInfo,
        invoice_id => InvoiceID,
        payment_id => PaymentID
    },
    PartyID = get_party_id(Context),
    IdempotentKey = capi_bender:get_idempotent_key(OperationID, PartyID, undefined),
    Hash = erlang:phash2(RefundParams),
    SequenceID = create_sequence_id([InvoiceID, PaymentID], OperationID),
    {ok, RefundID} = capi_bender:gen_by_sequence(IdempotentKey, SequenceID, Hash, ReqCtx, #{minimum => 100}),
    Params = #payproc_InvoicePaymentRefundParams{
        id = RefundID,
        reason = genlib_map:get(<<"reason">>, RefundParams),
        cash = encode_optional_refund_cash(RefundParams, EncodingContext)
    },
    Result = service_call(
        invoicing,
        'RefundPayment',
        [UserInfo, InvoiceID, PaymentID, Params],
        ReqCtx
    ),
    case Result of
        {ok, Refund} ->
            Resp = decode_refund(Refund),
            {ok, {201, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, #{}, logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPaymentStatus, <<"Invalid invoice payment status">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidContractStatus{} ->
                    {ok, {400, #{}, logic_error(invalidContractStatus, <<"Invalid contract status">>)}};
                #payproc_InsufficientAccountBalance{} ->
                    {ok, {400, #{}, logic_error(
                        insufficentAccountBalance,
                        <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                    )}};
                #payproc_InvoicePaymentAmountExceeded{} ->
                    {ok, {400, #{}, logic_error(invoicePaymentAmountExceeded, <<"Payment amount exceeded">>)}};
                #payproc_InconsistentRefundCurrency{} ->
                    {ok, {400, #{}, logic_error(inconsistentRefundCurrency, <<"Inconsistent refund currency">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetRefunds', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    UserInfo = get_user_info(Context),
    Result = get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID),
    case Result of
        {ok, #payproc_InvoicePayment{refunds = Refunds}} ->
            Resp = [decode_refund(R) || R <- Refunds],
            {ok, {200, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetRefundByID', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    RefundID = maps:get(refundID, Req),
    UserInfo = get_user_info(Context),
    Result = service_call(
        invoicing,
        'GetPaymentRefund',
        [UserInfo, InvoiceID, PaymentID, RefundID],
        ReqCtx
    ),
    case Result of
        {ok, Refund} ->
            Resp = decode_refund(Refund),
            {ok, {200, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentRefundNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice payment refund not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('CreateInvoiceTemplate', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    UserInfo = get_user_info(Context),
    try
        Params = encode_invoice_tpl_create_params(PartyID, maps:get('InvoiceTemplateCreateParams', Req)),
        prepare_party(
            Context,
            ReqCtx,
            fun () ->
                service_call(
                    invoice_templating,
                    'Create',
                    [UserInfo, Params],
                    ReqCtx
                )
            end
        )
    of
        {ok, InvoiceTpl} ->
            {ok, {201, #{}, make_invoice_tpl_and_token(InvoiceTpl, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, #{}, logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    catch
        throw:zero_invoice_lifetime ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}}
    end;

process_request('GetInvoiceTemplateByID', Req, Context, ReqCtx) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    UserInfo = get_user_info(Context),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            get_invoice_tpl_by_id(ReqCtx, UserInfo, InvoiceTplID)
        end
    ),
    case Result of
        {ok, InvoiceTpl} ->
            {ok, {200, #{}, decode_invoice_tpl(InvoiceTpl)}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, {404, #{}, general_error(<<"Invoice template not found">>)}}
    end;

process_request('UpdateInvoiceTemplate', Req, Context, ReqCtx) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    UserInfo = get_user_info(Context),
    try
        Params = encode_invoice_tpl_update_params(
            maps:get('InvoiceTemplateUpdateParams', Req),
            fun() ->
                get_invoice_tpl(InvoiceTplID, UserInfo, ReqCtx, Context)
            end
        ),
        prepare_party(
            Context,
            ReqCtx,
            fun () ->
                service_call(
                    invoice_templating,
                    'Update',
                    [UserInfo, InvoiceTplID, Params],
                    ReqCtx
                )
            end
        )
    of
        {ok, InvoiceTpl} ->
            {ok, {200, #{}, decode_invoice_tpl(InvoiceTpl)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}}
            end
    catch
        throw:#payproc_InvalidUser{} ->
            {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
        throw:#payproc_InvoiceTemplateNotFound{} ->
            {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
        throw:#payproc_InvoiceTemplateRemoved{} ->
            {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
        throw:zero_invoice_lifetime ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}}
    end;

process_request('DeleteInvoiceTemplate', Req, Context, ReqCtx) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    UserInfo = get_user_info(Context),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                invoice_templating,
                'Delete',
                [UserInfo, InvoiceTplID],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}}
            end
    end;

process_request('CreateInvoiceWithTemplate' = OperationID, Req, Context, ReqCtx) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    InvoiceParams = maps:get('InvoiceParamsWithTemplate', Req),
    PartyID = get_party_id(Context),
    try
        create_invoice_with_template(PartyID, InvoiceTplID, InvoiceParams, Context, ReqCtx, OperationID)
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, #{}, make_invoice_and_token(Invoice, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, #{}, general_error(<<"Invoice Template not found">>)}}
            end
    catch
        throw:{bad_invoice_params, currency_no_amount} ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Amount is required for the currency">>)}};
        throw:{bad_invoice_params, amount_no_currency} ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Currency is required for the amount">>)}}
    end;

process_request('GetInvoicePaymentMethodsByTemplateID', Req, Context, ReqCtx) ->
    {ok, Timestamp} = rfc3339:format(erlang:system_time()),
    Result = construct_payment_methods(
        invoice_templating,
        [
            get_user_info(Context),
            maps:get('invoiceTemplateID', Req),
            Timestamp
        ],
        ReqCtx
    ),
    case Result of
        {ok, PaymentMethods} when is_list(PaymentMethods) ->
            {ok, {200, #{}, PaymentMethods}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, {404, #{}, general_error(<<"Invoice template not found">>)}}
    end;

process_request('ActivateShop', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ShopID = maps:get(shopID, Req),

    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'ActivateShop',
                [UserInfo, PartyID, ShopID],
                ReqCtx
            )
        end
    ),

    case Result of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{
                    status = {suspension, {active, _}}
                } ->
                    {ok, {204, #{}, undefined}}
            end
    end;

process_request('SuspendShop', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ShopID = maps:get(shopID, Req),

    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'SuspendShop',
                [UserInfo, PartyID, ShopID],
                ReqCtx
            )
        end
    ),

    case Result of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{
                    status = {suspension, {suspended, _}}
                } ->
                    {ok, {204, #{}, undefined}}
            end
    end;

process_request('GetShops', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    {ok, #domain_Party{shops = Shops}} = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    Resp = decode_shops_map(Shops),
    {ok, {200, #{}, Resp}};

process_request('GetShopByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetShop',
                [UserInfo, PartyID, ShopID],
                ReqCtx
            )
        end
    ),

    case Result of
        {ok, Shop} ->
            Resp = decode_shop(Shop),
            {ok, {200, #{}, Resp}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Shop not found">>)}}
    end;

process_request('GetReports', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    ReportRequest = #reports_ReportRequest{
        party_id = PartyID,
        shop_id = maps:get(shopID, Req),
        time_range = #reports_ReportTimeRange{
            from_time = get_time('fromTime', Req),
            to_time = get_time('toTime', Req)
        }
    },
    StatReportRequest = #reports_StatReportRequest{
        request = ReportRequest
    },
    Result = service_call(reporting, 'GetReports', [StatReportRequest], ReqCtx),
    case Result of
        {ok, #reports_StatReportResponse{reports = Reports}} ->
            Resp = [decode_report(R) || #reports_Report{status = created} = R <- Reports],
            {ok, {200, #{}, Resp}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, {400, #{}, limit_exceeded_error(Limit)}}
            end
    end;

process_request('DownloadFile', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    ReportID = maps:get(reportID, Req),
    FileID = maps:get(fileID, Req),
    Result = service_call(reporting, 'GetReport', [ReportID], ReqCtx),
    case Result of
        {ok, #reports_Report{status = created, files = Files, party_id = PartyID, shop_id = ShopID}} ->
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, ReqCtx);
                false ->
                    {ok, {404, #{}, general_error(<<"File not found">>)}}
            end;
        {ok, _WrongReport} ->
            {ok, {404, #{}, general_error(<<"Report not found">>)}};
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Report not found">>)}}
    end;

process_request('GetContracts', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    {ok, Party} = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    {ok, {200, #{}, decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};

process_request('GetContractByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    {ok, Party} = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    case genlib_map:get(ContractID, Party#domain_Party.contracts) of
        undefined ->
            {ok, {404, #{}, general_error(<<"Contract not found">>)}};
        Contract ->
            {ok, {200, #{}, decode_contract(Contract, Party#domain_Party.contractors)}}
    end;

process_request('GetPayoutTools', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            Resp = [decode_payout_tool(P) || P <- PayoutTools],
            {ok, {200, #{}, Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Contract not found">>)}}
    end;

process_request('GetPayoutToolByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),
    PayoutToolID = maps:get('payoutToolID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                #domain_PayoutTool{} = P ->
                    {ok, {200, #{}, decode_payout_tool(P)}};
                false ->
                    {ok, {404, #{}, general_error(<<"PayoutTool not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Contract not found">>)}}
    end;

process_request('GetContractAdjustments', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, #{}, Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Contract not found">>)}}
    end;

process_request('GetContractAdjustmentByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),
    AdjustmentID = maps:get('adjustmentID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
                #domain_ContractAdjustment{} = A ->
                    {ok, {200, #{}, decode_contract_adjustment(A)}};
                false ->
                    {ok, {404, #{}, general_error(<<"Adjustment not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Contract not found">>)}}
    end;

process_request('GetMyParty', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    {ok, Party} = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    Resp = decode_party(Party),
    {ok, {200, #{}, Resp}};

process_request('SuspendMyParty', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'Suspend',
                [UserInfo, PartyID],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;

process_request('ActivateMyParty', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'Activate',
                [UserInfo, PartyID],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, #{}, undefined}}
    end;

process_request('GetCategories', _Req, Context, ReqCtx) ->
    _ = get_user_info(Context),
    _ = get_party_id(Context),
    {ok, Categories} = capi_domain:get_categories(ReqCtx),
    Resp = [decode_category(C) || C <- Categories],
    {ok, {200, #{}, Resp}};

process_request('GetCategoryByRef', Req, Context0, ReqCtx) ->
    _ = get_user_info(Context0),
    _ = get_party_id(Context0),
    CategoryID = maps:get(categoryID, Req),
    case get_category_by_id(genlib:to_int(CategoryID), ReqCtx) of
        {ok, Category} ->
            Resp = decode_category(Category),
            {ok, {200, #{}, Resp}};
        {error, not_found} ->
            {404, #{}, general_error(<<"Category not found">>)}
    end;

process_request('GetScheduleByRef', Req, Context, ReqCtx) ->
    _ = get_user_info(Context),
    _ = get_party_id(Context),
    ScheduleID = maps:get(scheduleID, Req),
    case get_schedule_by_id(genlib:to_int(ScheduleID), ReqCtx) of
        {ok, Schedule} ->
            {ok, {200, #{}, decode_business_schedule(Schedule)}};
        {error, not_found} ->
            {404, #{}, general_error(<<"Schedule not found">>)}
    end;

process_request('GetPaymentInstitutions', Req, _Context, ReqCtx) ->
    try
        Residence = encode_residence(genlib_map:get(residence, Req)),
        Realm = genlib_map:get(realm, Req),
        {ok, PaymentInstObjects} = capi_domain:get_payment_institutions(ReqCtx),
        Resp =
            lists:filtermap(
                fun(P) ->
                    case check_payment_institution(Realm, Residence, P) of
                        true ->
                            {true, decode_payment_institution_obj(P)};
                        false ->
                            false
                    end
                end,
                PaymentInstObjects
            ),
        {ok, {200, #{}, Resp}}
    catch
        throw:{encode_residence, invalid_residence} ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Invalid residence">>)}}
    end;

process_request('GetPaymentInstitutionByRef', Req, _Context, ReqCtx) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case capi_domain:get({payment_institution, ?payment_institution_ref(PaymentInstitutionID)}, ReqCtx) of
        {ok, PaymentInstitution} ->
            Resp = decode_payment_institution_obj(PaymentInstitution),
            {ok, {200, #{}, Resp}};
        {error, not_found} ->
            {404, #{}, general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPaymentTerms', Req, Context, ReqCtx) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    VS = #payproc_Varset{},
    case compute_payment_institution_terms(PaymentInstitutionID, VS, Context, ReqCtx) of
        {ok, #domain_TermSet{payments = PaymentTerms}} ->
            Resp = decode_payment_terms(PaymentTerms),
            {ok, {200, #{}, Resp}};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, #{}, general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutMethods', Req, Context, ReqCtx) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    VS = prepare_varset(Req),
    case compute_payment_institution_terms(PaymentInstitutionID, VS, Context, ReqCtx) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{
            payout_methods = PayoutMethods
        }}} ->
            Resp = decode_payout_methods_selector(PayoutMethods),
            {ok, {200, #{}, Resp}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, #{}, general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, #{}, general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutSchedules', Req, Context, ReqCtx) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    VS = prepare_varset(Req),
    case compute_payment_institution_terms(PaymentInstitutionID, VS, Context, ReqCtx) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_schedules = Schedules}}} ->
            {ok, {200, #{}, decode_business_schedules_selector(Schedules)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, #{}, general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, #{}, general_error(<<"Payment institution not found">>)}
    end;

process_request('GetAccountByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    AccountID = maps:get('accountID', Req),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetAccountState',
                [UserInfo, PartyID, genlib:to_int(AccountID)],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, S} ->
            Resp = decode_account_state(S),
            {ok, {200, #{}, Resp}};
        {exception, #payproc_AccountNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Account not found">>)}}
    end;

process_request('GetClaims', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimStatus = maps:get('claimStatus', Req),
    {ok, Claims} = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetClaims',
                [UserInfo, PartyID],
                ReqCtx
            )
        end
    ),
    Resp = decode_claims(filter_claims(ClaimStatus, Claims)),
    {ok, {200, #{}, Resp}};

process_request('GetClaimByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimID = maps:get('claimID', Req),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetClaim',
                [UserInfo, PartyID, genlib:to_int(ClaimID)],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, Claim} ->
            case is_wallet_claim(Claim) of
                true ->
                    %% filter this out
                    {ok, {404, #{}, general_error(<<"Claim not found">>)}};
                false ->
                    {ok, {200, #{}, decode_claim(Claim)}}
            end;
        {exception, #payproc_ClaimNotFound{}} ->
            {ok, {404, #{}, general_error(<<"Claim not found">>)}}
    end;

process_request('CreateClaim', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    try
        Changeset = encode_claim_changeset(maps:get('ClaimChangeset', Req), ReqCtx),
        Result = prepare_party(
            Context,
            ReqCtx,
            fun () ->
                service_call(
                    party_management,
                    'CreateClaim',
                    [UserInfo, PartyID, Changeset],
                    ReqCtx
                )
            end
        ),
        case Result of
            {ok, Claim} ->
                Resp = decode_claim(Claim),
                {ok, {201, #{}, Resp}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidPartyStatus{} ->
                        {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                    #payproc_ChangesetConflict{} ->
                        {ok, {400, #{}, logic_error(changesetConflict, <<"Changeset conflict">>)}};
                    #payproc_InvalidChangeset{} ->
                        {ok, {400, #{}, logic_error(invalidChangeset, <<"Invalid changeset">>)}};
                    #'InvalidRequest'{errors = Errors} ->
                        {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}}
                end
        end
    catch
        throw:{encode_contract_modification, adjustment_creation_not_supported} ->
            {ok, {400, #{}, logic_error(invalidChangeset, <<"Contract adjustment creation not supported">>)}};
        throw:{encode_residence, invalid_residence} ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Invalid residence">>)}}
    end;

% TODO disabled temporary, exception handling must be fixed befor enabling
% process_request('UpdateClaimByID', Req, Context, ReqCtx) ->
%     UserInfo = get_user_info(Context),
%     PartyID = get_party_id(Context),
%     ClaimID = genlib:to_int(maps:get('claimID', Req)),
%     ClaimRevision = genlib:to_int(maps:get('claimRevision', Req)),
%     Changeset = encode_claim_changeset(maps:get('claimChangeset', Req)),
%     {ok, Party} = prepare_party(
%         Context,
%         ReqCtx,
%         fun () ->
%             service_call(
%                 party_management,
%                 'UpdateClaim',
%                 [UserInfo, PartyID, ClaimID, ClaimRevision, Changeset],
%                 ReqCtx
%             )
%         end
%     ),
%     Resp = decode_party(Party),
%     {ok, {200, #{}, Resp}};

process_request('RevokeClaimByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimID = genlib:to_int(maps:get('claimID', Req)),
    ClaimRevision = genlib:to_int(maps:get('claimRevision', Req)),
    Reason = encode_reason(maps:get('Reason', Req)),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'RevokeClaim',
                [UserInfo, PartyID, ClaimID, ClaimRevision, Reason],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, _} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_ClaimNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Claim not found">>)}};
                #payproc_InvalidClaimStatus{} ->
                    {ok, {400, #{}, logic_error(invalidClaimStatus, <<"Invalid claim status">>)}};
                #payproc_InvalidClaimRevision{} ->
                    {ok, {400, #{}, logic_error(invalidClaimRevision, <<"Invalid claim revision">>)}}
            end
    end;

process_request('CreateWebhook', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    WebhookParams = encode_webhook_params(PartyID, maps:get('Webhook', Req)),
    case validate_webhook_params(WebhookParams, Context, ReqCtx) of
        {ok, _} ->
            {ok, Webhook} = service_call(webhook_manager, 'Create', [WebhookParams], ReqCtx),
            Resp = decode_webhook(Webhook),
            {ok, {201, #{}, Resp}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {400, #{}, logic_error(invalidShopID, <<"Shop not found">>)}}
    end;

process_request('GetWebhooks', _Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    {ok, Webhooks} = service_call(webhook_manager, 'GetList', [PartyID], ReqCtx),
    {ok, {200, #{}, [decode_webhook(V) || V <- Webhooks]}};

process_request('GetWebhookByID', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case get_webhook(PartyID, WebhookID, ReqCtx) of
                {ok, Webhook} ->
                    {ok, {200, #{}, decode_webhook(Webhook)}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {404, #{}, general_error(<<"Webhook not found">>)}}
            end;
        error ->
            {ok, {404, #{}, general_error(<<"Webhook not found">>)}}
    end;

process_request('DeleteWebhookByID', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case delete_webhook(PartyID, WebhookID, ReqCtx) of
                {ok, _} ->
                    {ok, {204, #{}, undefined}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {204, #{}, undefined}}
            end;
        error ->
            {ok, {404, #{}, general_error(<<"Webhook not found">>)}}
    end;

process_request('CreateCustomer', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    Params = encode_customer_params(PartyID, maps:get('Customer', Req)),
    Result = prepare_party(
       Context,
       ReqCtx,
       fun () ->
           service_call(
               customer_management,
               'Create',
               [Params],
               ReqCtx
           )
       end
    ),
    case Result of
        {ok, Customer} ->
            {ok, {201, #{}, make_customer_and_token(Customer, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, #{}, logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, #{}, logic_error(operationNotPermitted, <<"Operation not permitted">>)}}
            end
    end;

process_request('GetCustomerById', Req, _Context, ReqCtx) ->
    CustomerID = maps:get('customerID', Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, Customer} ->
            {ok, {200, #{}, decode_customer(Customer)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}}
            end
    end;

process_request('DeleteCustomer', Req, _Context, ReqCtx) ->
    CustomerID = maps:get(customerID, Req),
    Result = service_call(
        customer_management,
        'Delete',
        [CustomerID],
        ReqCtx
    ),
    case Result of
        {ok, _} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CreateCustomerAccessToken', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    CustomerID = maps:get(customerID, Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, #payproc_Customer{}} ->
            Token = make_customer_access_token(CustomerID, PartyID, Context),
            {ok, {201, #{}, Token}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}}
            end
    end;

process_request('CreateBinding', Req, _Context, ReqCtx) ->
    CustomerID = maps:get(customerID, Req),
    BindingParams = maps:get('CustomerBindingParams', Req),
    Result = try
        Params =  encode_customer_binding_params(BindingParams),
        service_call(
            customer_management,
            'StartBinding',
            [CustomerID, Params],
            ReqCtx
        )
    catch
        throw:Error when Error =:= invalid_token orelse Error =:= invalid_payment_session ->
            {error, Error}
    end,

    case Result of
        {ok, CustomerBinding} ->
            {ok, {201, #{}, decode_customer_binding(CustomerBinding)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, #{}, logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, #{}, logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidPaymentTool{} ->
                    {ok, {400, #{}, logic_error(invalidPaymentResource, <<"Invalid payment resource">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, #{}, logic_error(operationNotPermitted, <<"Operation not permitted">>)}}
            end;
        {error, invalid_token} ->
            {ok, {400, #{}, logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            )}};
        {error, invalid_payment_session} ->
            {ok, {400, #{}, logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            )}}
    end;

process_request('GetBindings', Req, _Context, ReqCtx) ->
    CustomerID = maps:get(customerID, Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            {ok, {200, #{}, [decode_customer_binding(B) || B <- Bindings]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}}
            end
    end;

process_request('GetBinding', Req, _Context, ReqCtx) ->
    CustomerID = maps:get(customerID, Req),
    BindingID = maps:get(customerBindingID, Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            case lists:keyfind(BindingID, #payproc_CustomerBinding.id, Bindings) of
                #payproc_CustomerBinding{} = B ->
                    {ok, {200, #{}, decode_customer_binding(B)}};
                false ->
                    {ok, {404, #{}, general_error(<<"Customer binding not found">>)}}
            end;
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}}
            end
    end;

process_request('GetCustomerEvents', Req, _Context, ReqCtx) ->
    CustomerID = maps:get(customerID, Req),
    GetterFun = fun(Range) ->
        service_call(
            customer_management,
            'GetEvents',
            [CustomerID, Range],
            ReqCtx
        )
    end,
    Result  = collect_events(
        maps:get(limit, Req),
        genlib_map:get(eventID, Req),
        GetterFun,
        fun decode_customer_event/2,
        undefined
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, #{}, Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Customer not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, #{}, general_error(<<"Event not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end.

create_invoice(PartyID, InvoiceParams, Context, ReqCtx, BenderPrefix) ->
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, undefined),
    Hash = erlang:phash2(InvoiceParams),
    {ok, ID} = capi_bender:gen_by_snowflake(IdempotentKey, Hash, ReqCtx),
    UserInfo = get_user_info(Context),
    Params = encode_invoice_params(ID, PartyID, InvoiceParams),
    prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(invoicing, 'Create', [UserInfo, Params], ReqCtx)
        end).

create_invoice_with_template(PartyID, InvoiceTplID, InvoiceParams, Context, ReqCtx, BenderPrefix) ->
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, undefined),
    UserInfo = get_user_info(Context),
    Hash = erlang:phash2({InvoiceTplID, InvoiceParams}),
    {ok, ID} = capi_bender:gen_by_snowflake(IdempotentKey, Hash, ReqCtx),
    Params = encode_invoice_params_with_tpl(ID, InvoiceTplID, InvoiceParams),
    prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(invoicing, 'CreateWithTemplate', [UserInfo, Params], ReqCtx)
        end
    ).

create_payment(PartyID, InvoiceID, PaymentParams, Context, ReqCtx, BenderPrefix) ->
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, undefined),
    Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
    Payer = genlib_map:get(<<"payer">>, PaymentParams),
    UserInfo = get_user_info(Context),
    Hash = erlang:phash2(PaymentParams),
    {ok, ID} = capi_bender:gen_by_sequence(IdempotentKey, InvoiceID, Hash, ReqCtx),
    Params =  #payproc_InvoicePaymentParams{
        id = ID,
        payer = encode_payer_params(Payer),
        flow = encode_flow(Flow)
    },
    service_call(invoicing, 'StartPayment', [UserInfo, InvoiceID, Params], ReqCtx).

check_payment_institution(Realm, Residence, PaymentInstitution) ->
    check_payment_institution_realm(Realm, PaymentInstitution) andalso
        check_payment_institution_residence(Residence, PaymentInstitution).

check_payment_institution_realm(undefined, _) ->
    true;
check_payment_institution_realm(Realm1, #domain_PaymentInstitutionObject{
    data = #domain_PaymentInstitution{realm = Realm2}
}) ->
    Realm1 =:= Realm2.

check_payment_institution_residence(undefined, _) ->
    true;
check_payment_institution_residence(Residence, #domain_PaymentInstitutionObject{
    data = #domain_PaymentInstitution{residences = Residences}
}) ->
    ordsets:is_element(Residence, Residences).

generate_report_presigned_url(FileID, ReqCtx) ->
    ExpiresAt = get_default_url_lifetime(),
    Result = service_call(reporting, 'GeneratePresignedUrl', [FileID, ExpiresAt], ReqCtx),
    case Result of
        {ok, URL} ->
            {ok, {303, #{<<"Location">> => URL}, undefined}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_FileNotFound{}->
                    {ok, {404, #{}, general_error(<<"File not found">>)}}
            end
    end.

validate_webhook_params(#webhooker_WebhookParams{event_filter = EventFilter}, Context, ReqCtx) ->
    validate_event_filter(EventFilter, Context, ReqCtx).

validate_event_filter({invoice, #webhooker_InvoiceEventFilter{shop_id = ShopID}}, Context, ReqCtx) ->
    validate_event_filter_shop(ShopID, Context, ReqCtx);

validate_event_filter({customer, #webhooker_CustomerEventFilter{shop_id = ShopID}}, Context, ReqCtx) ->
    validate_event_filter_shop(ShopID, Context, ReqCtx).

validate_event_filter_shop(ShopID, Context, ReqCtx) when ShopID /= undefined ->
    PartyID = get_party_id(Context),
    UserInfo = get_user_info(Context),
    prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetShop',
                [UserInfo, PartyID, ShopID],
                ReqCtx
            )
        end
    ).

get_webhook(PartyID, WebhookID, ReqCtx) ->
    Result = service_call(webhook_manager, 'Get', [WebhookID], ReqCtx),
    case Result of
        {ok, Webhook = #webhooker_Webhook{party_id = PartyID}} ->
            {ok, Webhook};
        {ok, _Webhook} ->
            {exception, #webhooker_WebhookNotFound{}};
        {exception, Exception} ->
            {exception, Exception}
    end.

encode_webhook_id(WebhookID) ->
    try
        ID = binary_to_integer(WebhookID),
        {ok, ID}
    catch
        error:badarg ->
            error
    end.

decode_webhook_id(WebhookID) when is_integer(WebhookID) ->
    {ok, integer_to_binary(WebhookID)}.

encode_webhook_params(PartyID, #{
    <<"scope">> := Scope,
    <<"url">>   := URL
}) ->
    #webhooker_WebhookParams{
        party_id     = PartyID,
        url          = URL,
        event_filter = encode_webhook_scope(Scope)
    }.

encode_webhook_scope(#{
    <<"topic">>      := <<"InvoicesTopic">>,
    <<"shopID">>     := ShopID,
    <<"eventTypes">> := EventTypes
}) ->
    {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ShopID,
        types   = ordsets:from_list([
            encode_invoice_event_type(V) || V <- EventTypes
        ])
    }};
encode_webhook_scope(#{
    <<"topic">>      := <<"CustomersTopic">>,
    <<"shopID">>     := ShopID,
    <<"eventTypes">> := EventTypes
}) ->
    {customer, #webhooker_CustomerEventFilter{
        shop_id = ShopID,
        types   = ordsets:from_list([
            encode_customer_event_type(V) || V <- EventTypes
        ])
    }}.

%%%

service_call(ServiceName, Function, Args, Context) ->
    capi_woody_client:call_service(ServiceName, Function, Args, Context).

create_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    _ = logger:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
    WoodyContext = woody_context:new(RpcID),
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyContext).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => capi_auth:get_subject_id(AuthContext),
        realm => ?REALM,
        email => capi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => capi_auth:get_claim(<<"name">>, AuthContext, undefined)
    }).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

limit_exceeded_error(Limit) ->
    logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit])).

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

get_user_info(Context) ->
    #payproc_UserInfo{
        id = get_party_id(Context),
        type = {external_user, #payproc_ExternalUser{}}
    }.

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

get_party_id(Context) ->
    capi_auth:get_subject_id(get_auth_context(Context)).

get_party_params(Context) ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = capi_auth:get_claim(<<"email">>, get_auth_context(Context))
        }
    }.

encode_invoice_params(ID, PartyID, InvoiceParams) ->
    Amount = genlib_map:get(<<"amount">>, InvoiceParams),
    Currency = genlib_map:get(<<"currency">>, InvoiceParams),
    Cart = genlib_map:get(<<"cart">>, InvoiceParams),
    #payproc_InvoiceParams{
        id       = ID,
        party_id = PartyID,
        details  = encode_invoice_details(InvoiceParams),
        cost     = encode_invoice_cost(Amount, Currency, Cart),
        due      = get_time(<<"dueDate">>, InvoiceParams),
        context  = encode_invoice_context(InvoiceParams),
        shop_id  = genlib_map:get(<<"shopID">>, InvoiceParams)
    }.

encode_invoice_cost(Amount, Currency, Cart) when Amount =/= undefined, Cart =/= undefined ->
    case get_invoice_cart_amount(Cart) of
        Amount ->
            encode_cash(Amount, Currency);
        _ ->
            throw(invalid_invoice_cost)
    end;
encode_invoice_cost(undefined, Currency, Cart) when Cart =/= undefined ->
    encode_cash(get_invoice_cart_amount(Cart), Currency);
encode_invoice_cost(Amount, Currency, undefined) when Amount =/= undefined ->
    encode_cash(Amount, Currency);
encode_invoice_cost(_, _, _) ->
    throw(invalid_invoice_cost).

get_invoice_cart_amount(Cart) ->
    lists:foldl(
        fun(Line, Acc) ->
            P = genlib_map:get(<<"price">>, Line),
            Q = genlib_map:get(<<"quantity">>, Line),
            Acc + (P * Q)
        end,
        0,
        Cart
    ).

encode_invoice_params_with_tpl(InvoiceID, InvoiceTplID, InvoiceParams) ->
    #payproc_InvoiceWithTemplateParams{
        id          = InvoiceID,
        template_id = InvoiceTplID,
        cost        = encode_optional_invoice_cost(InvoiceParams),
        context     = encode_optional_context(InvoiceParams)
    }.

encode_invoice_details(Params) ->
    #domain_InvoiceDetails{
        product = genlib_map:get(<<"product">>, Params),
        description = genlib_map:get(<<"description">>, Params),
        cart = encode_invoice_cart(Params)
    }.

encode_invoice_cart(Params) ->
    Cart = genlib_map:get(<<"cart">>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_invoice_cart(Cart, Currency).

encode_invoice_cart(Cart, Currency) when Cart =/= undefined, Cart =/= [] ->
    #domain_InvoiceCart{
        lines = [encode_invoice_line(Line, Currency) || Line <- Cart]
    };
encode_invoice_cart([], _) ->
    throw(invoice_cart_empty);
encode_invoice_cart(undefined, _) ->
    undefined.

encode_invoice_line(Line, Currency) ->
    Metadata = case genlib_map:get(<<"taxMode">>, Line) of
        TaxMode when TaxMode =/= undefined ->
            TM = encode_invoice_line_tax_mode(TaxMode),
            #{<<"TaxMode">> => {str, TM}};
        undefined ->
            #{}
    end,
    Price = encode_cash(genlib_map:get(<<"price">>, Line), Currency),
    #domain_InvoiceLine{
        product = genlib_map:get(<<"product">>, Line),
        quantity = genlib_map:get(<<"quantity">>, Line),
        price = Price,
        metadata = Metadata
    }.

encode_invoice_line_tax_mode(#{<<"type">> := <<"InvoiceLineTaxVAT">>} = TaxMode)  ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    genlib_map:get(<<"rate">>, TaxMode).

encode_cash(Params) ->
    Amount = genlib_map:get(<<"amount">>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_cash(Amount, Currency).

encode_cash(Amount, Currency) ->
    #domain_Cash{
        amount = Amount,
        currency = encode_currency(Currency)
    }.

encode_payer_params(#{
    <<"payerType">> := <<"CustomerPayer">>,
    <<"customerID">> := ID
}) ->
    {customer, #payproc_CustomerPayerParams{customer_id = ID}};

encode_payer_params(#{
    <<"payerType">> := <<"PaymentResourcePayer">>,
    <<"paymentToolToken">> := Token,
    <<"paymentSession">> := EncodedSession,
    <<"contactInfo">> := ContactInfo
}) ->
    PaymentTool = encode_payment_tool_token(Token),
    {ClientInfo, PaymentSession} = unwrap_payment_session(EncodedSession),
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = encode_client_info(ClientInfo)
        },
        contact_info = encode_contact_info(ContactInfo)
    }}.

encode_payment_tool_token(Token) ->
    try capi_utils:base64url_to_map(Token) of
        <<"v1", PaymentToolToken/binary>> ->
            encode_payment_tool_token(v1, PaymentToolToken);
        PaymentToolTokenOld ->
            encode_payment_tool_token(depricated, PaymentToolTokenOld)
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end.

encode_payment_tool_token(depricated, PaymentToolToken) ->
    case PaymentToolToken of
        #{<<"type">> := <<"bank_card">>} = Encoded ->
            encode_bank_card(Encoded);
        #{<<"type">> := <<"payment_terminal">>} = Encoded ->
            encode_payment_terminal(Encoded);
        #{<<"type">> := <<"digital_wallet">>} = Encoded ->
            encode_digital_wallet(Encoded);
        #{<<"type">> := <<"crypto_wallet">>} = Encoded ->
            encode_crypto_wallet(Encoded)
    end;
encode_payment_tool_token(v1, EncryptedToken) ->
    ThriftType = {struct, union, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
    {ok, PaymentToolToken} = lechiffre:decode(ThriftType, EncryptedToken),
    case PaymentToolToken of
        {bank_card_payload, #ptt_BankCardPayload{bank_card = BankCard}} ->
            {bank_card, BankCard};
        {payment_terminal_payload, #ptt_PaymentTerminalPayload{payment_terminal = PaymentTerminal}} ->
            {payment_terminal, PaymentTerminal};
        {digital_wallet_payload, #ptt_DigitalWalletPayload{digital_wallet = DigitalWallet}} ->
            {digital_wallet, DigitalWallet};
        {crypto_currency_payload, #ptt_CryptoCurrencyPayload{crypto_currency = CryptoCurrency}} ->
            {crypto_wallet, CryptoCurrency}
    end.

decode_bank_card(#domain_BankCard{
    'token'  = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan,
    'token_provider' = TokenProvider,
    'issuer_country' = IssuerCountry,
    'bank_name'      = BankName,
    'metadata'       = Metadata
}) ->
    capi_utils:map_to_base64url(genlib_map:compact(#{
        <<"type">> => <<"bank_card">>,
        <<"token">> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin">> => Bin,
        <<"masked_pan">> => MaskedPan,
        <<"token_provider">> => TokenProvider,
        <<"issuer_country">> => IssuerCountry,
        <<"bank_name"     >> => BankName,
        <<"metadata"      >> => decode_bank_card_metadata(Metadata)
    })).

decode_bank_card_metadata(undefined) ->
    undefined;
decode_bank_card_metadata(Meta) ->
    maps:map(fun(_, Data) -> capi_msgp_marshalling:unmarshal(Data) end, Meta).

decode_payment_terminal(#domain_PaymentTerminal{
    terminal_type = Type
}) ->
    capi_utils:map_to_base64url(#{
        <<"type">> => <<"payment_terminal">>,
        <<"terminal_type">> => Type
    }).

decode_digital_wallet(#domain_DigitalWallet{
    provider = Provider,
    id = ID
}) ->
    capi_utils:map_to_base64url(#{
        <<"type">> => <<"digital_wallet">>,
        <<"provider">> => atom_to_binary(Provider, utf8),
        <<"id">> => ID
    }).

decode_crypto_wallet(CryptoCurrency) ->
    capi_utils:map_to_base64url(#{
        <<"type"           >> => <<"crypto_wallet">>,
        <<"crypto_currency">> => convert_crypto_currency_to_swag(CryptoCurrency)
    }).

decode_client_info(ClientInfo) ->
    #{
        <<"fingerprint">> => ClientInfo#domain_ClientInfo.fingerprint,
        <<"ip">> => ClientInfo#domain_ClientInfo.ip_address
    }.

encode_client_info(ClientInfo) ->
    #domain_ClientInfo{
        fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
        ip_address = maps:get(<<"ip">>, ClientInfo)
    }.

encode_invoice_tpl_create_params(PartyID, Params) ->
    #payproc_InvoiceTemplateCreateParams{
        party_id         = PartyID,
        shop_id          = genlib_map:get(<<"shopID">>, Params),
        invoice_lifetime = encode_lifetime(Params),
        product          = genlib_map:get(<<"product">>, Params),
        description      = genlib_map:get(<<"description">>, Params),
        details          = encode_invoice_tpl_details(Params),
        context          = encode_invoice_context(Params)
    }.

encode_invoice_tpl_update_params(Params, InvoiceTplGetter) ->
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = encode_optional_invoice_tpl_lifetime(Params),
        product          = genlib_map:get(<<"product">>, Params),
        description      = genlib_map:get(<<"description">>, Params),
        context          = encode_optional_context(Params),
        details          = encode_optional_details(Params, InvoiceTplGetter)
    }.

encode_invoice_tpl_details(#{<<"product">> := Product, <<"cost">> := Cost}) ->
    {product, encode_invoice_tpl_product(Product, Cost)}.

encode_invoice_tpl_product(Product, Cost) ->
    #domain_InvoiceTemplateProduct{
        product = Product,
        price = encode_invoice_tpl_cost(Cost),
        metadata = ?DEFAULT_INVOICE_TPL_META
    }.

encode_optional_invoice_tpl_lifetime(Params = #{<<"lifetime">> := _}) ->
    encode_lifetime(Params);
encode_optional_invoice_tpl_lifetime(_) ->
    undefined.

encode_optional_invoice_cost(Params = #{<<"amount">> := _, <<"currency">> := _}) ->
    encode_cash(Params);
encode_optional_invoice_cost(#{<<"amount">> := _}) ->
    throw({bad_invoice_params, amount_no_currency});
encode_optional_invoice_cost(#{<<"currency">> := _}) ->
    throw({bad_invoice_params, currency_no_amount});
encode_optional_invoice_cost(_) ->
    undefined.

encode_optional_context(Params = #{<<"metadata">> := _}) ->
    encode_invoice_context(Params);
encode_optional_context(#{}) ->
    undefined.

encode_optional_details(#{<<"product">> := Product, <<"cost">> := Cost}, _) ->
    {product, encode_invoice_tpl_product(Product, Cost)};
encode_optional_details(#{<<"cost">> := Cost}, InvoiceTplGetter) ->
    #domain_InvoiceTemplate{
        product = Product
    } = InvoiceTplGetter(),
    {product, encode_invoice_tpl_product(Product, Cost)};
encode_optional_details(#{<<"product">> := Product}, InvoiceTplGetter) ->
    #domain_InvoiceTemplate{
        details = {product, #domain_InvoiceTemplateProduct{price = Price}}
    } = InvoiceTplGetter(),
    {product, #domain_InvoiceTemplateProduct{
        product = Product,
        price = Price,
        metadata = ?DEFAULT_INVOICE_TPL_META
    }};
encode_optional_details(_, _) ->
    undefined.

encode_invoice_context(Params) ->
    encode_invoice_context(Params, ?DEFAULT_INVOICE_META).

encode_invoice_context(Params, DefaultMeta) ->
    Context = genlib_map:get(<<"metadata">>, Params, DefaultMeta),
    encode_content(json, Context).

encode_content(json, Data) ->
    #'Content'{
        type = <<"application/json">>,
        data = jsx:encode(Data)
    }.

encode_invoice_tpl_cost(#{<<"invoiceTemplateCostType">> := CostType} = Cost) ->
    encode_invoice_tpl_cost(CostType, Cost);
encode_invoice_tpl_cost(_) ->
    undefined.

encode_invoice_tpl_cost(<<"InvoiceTemplateCostUnlim">>, _Cost) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}};
encode_invoice_tpl_cost(<<"InvoiceTemplateCostFixed">>, Cost) ->
    {fixed, encode_cash(Cost)};
encode_invoice_tpl_cost(<<"InvoiceTemplateCostRange">>, Cost) ->
    Range = genlib_map:get(<<"range">>, Cost),
    {range, #domain_CashRange{
        lower = {inclusive, encode_cash(Cost#{<<"amount">> => genlib_map:get(<<"lowerBound">>, Range)})},
        upper = {inclusive, encode_cash(Cost#{<<"amount">> => genlib_map:get(<<"upperBound">>, Range)})}
    }}.

encode_lifetime(Params) ->
    Lifetime = genlib_map:get(<<"lifetime">>, Params),
    encode_lifetime(
        genlib_map:get(<<"days">>, Lifetime),
        genlib_map:get(<<"months">>, Lifetime),
        genlib_map:get(<<"years">>, Lifetime)
    ).

encode_lifetime(0, 0, 0) ->
    throw(zero_invoice_lifetime);
encode_lifetime(DD, MM, YY) ->
    #domain_LifetimeInterval{
        days   = DD,
        months = MM,
        years  = YY
      }.

encode_shop_params(Params) ->
    #payproc_ShopParams{
        location =  encode_shop_location(genlib_map:get(<<"location">>, Params)),
        details = encode_shop_details(genlib_map:get(<<"details">>, Params)),
        contract_id = genlib_map:get(<<"contractID">>, Params),
        payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params)
    }.

encode_shop_details(undefined) ->
    undefined;

encode_shop_details(Details = #{
    <<"name">> := Name
}) ->
    #domain_ShopDetails{
        name = Name,
        description = genlib_map:get(<<"description">>, Details)
    }.

encode_shop_location(#{
    <<"locationType">> := <<"ShopLocationUrl">>,
    <<"url">> := Url
}) ->
    {url, Url}.

encode_category_ref(undefined) ->
    undefined;

encode_category_ref(Ref) ->
    #domain_CategoryRef{
        id = Ref
    }.

encode_claim_changeset(Changeset, ReqCtx) when is_list(Changeset)->
    lists:map(fun (C) -> encode_party_modification(C, ReqCtx) end, Changeset).

encode_party_modification(#{<<"partyModificationType">> := Type} = Modification, ReqCtx) ->
    case Type of
        <<"ContractModification">> ->
            {contract_modification, encode_contract_modification(Modification, ReqCtx)};
        <<"ShopModification">> ->
            {shop_modification, encode_shop_modification(Modification, ReqCtx)}
    end.

encode_contract_modification(#{<<"contractID">> := ContractID} = Modification, ReqCtx) ->
    EncodedMod = case maps:get(<<"contractModificationType">>, Modification) of
        <<"ContractCreation">> ->
            PaymentInstitutionRef = genlib_map:get(<<"paymentInstitutionID">>, Modification),
            {creation, #payproc_ContractParams{
                contractor = encode_contractor(maps:get(<<"contractor">>, Modification)),
                payment_institution = ensure_payment_institution(PaymentInstitutionRef, ReqCtx)
            }};
        <<"ContractTermination">> ->
            {termination, #payproc_ContractTermination{
                reason = encode_reason(maps:get(<<"reason">>, Modification))
            }};
        <<"ContractLegalAgreementBinding">> ->
            {legal_agreement_binding, encode_legal_agreement(maps:get(<<"legalAgreement">>, Modification))};
        <<"ContractAdjustmentCreation">> ->
        % FIXME need swag supprot for template ref
        %     {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
        %         adjustment_id = maps:get(<<"adjustmentID">>, Modification),
        %         modification = {creation, #payproc_ContractAdjustmentParams{
        %             template = NOT_SUPPORTED
        %         }}
        %     }};
            erlang:throw({encode_contract_modification, adjustment_creation_not_supported});
        <<"ContractPayoutToolCreation">> ->
            {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                payout_tool_id = maps:get(<<"payoutToolID">>, Modification),
                modification   = {creation, encode_payout_tool_params(Modification)}
            }};
        <<"ContractReportingPreferencesChange">> ->
            {report_preferences_modification, encode_report_preferences(Modification)}
    end,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = EncodedMod
    }.

ensure_payment_institution(Ref, _ReqCtx) when Ref /= undefined ->
    encode_payment_institution_ref(Ref);
ensure_payment_institution(undefined, ReqCtx) ->
    capi_domain:get_default_payment_institution_ref(live, ReqCtx).

encode_shop_modification(#{<<"shopID">> := ShopID} = Modification, _ReqCtx) ->
    EncodedMod = case maps:get(<<"shopModificationType">>, Modification) of
        <<"ShopCreation">> ->
            {creation, encode_shop_params(Modification)};
        <<"ShopAccountCreation">> ->
            {shop_account_creation, #payproc_ShopAccountParams{
                currency = encode_currency(maps:get(<<"currency">>, Modification))
            }};
        <<"ShopCategoryChange">> ->
            {category_modification, encode_category_ref(maps:get(<<"categoryID">>, Modification))};
        <<"ShopLocationChange">> ->
            {location_modification, encode_shop_location(maps:get(<<"location">>, Modification))};
        <<"ShopDetailsChange">> ->
            {details_modification, encode_shop_details(maps:get(<<"details">>, Modification))};
        <<"ShopContractBinding">> ->
            {contract_modification, #payproc_ShopContractModification{
                contract_id = maps:get(<<"contractID">>, Modification),
                payout_tool_id = maps:get(<<"payoutToolID">>, Modification)
            }};
        <<"ShopPayoutToolChange">> ->
            {payout_tool_modification, maps:get(<<"payoutToolID">>, Modification)};
        <<"ShopPayoutScheduleChange">> ->
            {payout_schedule_modification, #payproc_ScheduleModification{
                schedule = encode_schedule_ref(genlib_map:get(<<"scheduleID">>, Modification))
            }}
    end,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = EncodedMod
    }.

encode_reason(undefined) ->
    undefined;
encode_reason(#{<<"reason">> := Reason}) ->
    Reason.

encode_legal_agreement(LegalAgreement) ->
    #domain_LegalAgreement{
        signed_at = maps:get(<<"signedAt">>, LegalAgreement),
        legal_agreement_id = maps:get(<<"id">>, LegalAgreement),
        valid_until = genlib_map:get(<<"validUntil">>, LegalAgreement)
    }.

encode_payout_tool_params(#{
    <<"currency">> := Currency,
    <<"details">> := Details
}) ->
    #payproc_PayoutToolParams{
        currency = encode_currency(Currency),
        tool_info = encode_payout_tool_info(Details)
    }.

encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsBankAccount">>} = Tool) ->
   {russian_bank_account, encode_russian_bank_account(Tool)};
encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsInternationalBankAccount">>} = Tool) ->
   {international_bank_account, encode_international_bank_account(Tool)}.

encode_russian_bank_account(#{
    <<"account">> := Account,
    <<"bankName">> := BankName,
    <<"bankPostAccount">> := BankPostAccount,
    <<"bankBik">> := BankBik
}) ->
    #domain_RussianBankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
    }.

encode_international_bank_account(Acc) ->
    #domain_InternationalBankAccount{
        iban           = genlib_map:get(<<"iban">>, Acc),
        bank           = encode_international_bank_details(Acc),
        account_holder = genlib_map:get(<<"accountHolder">>, Acc)
     }.

encode_international_bank_details(Acc) ->
    #domain_InternationalBankDetails{
        bic     = genlib_map:get(<<"bic">>, Acc),
        name    = genlib_map:get(<<"bankName">>, Acc),
        address = genlib_map:get(<<"bankAddress">>, Acc)
    }.

encode_contractor(#{<<"contractorType">> := <<"PrivateEntity">>} = Contractor) ->
    {private_entity, encode_private_entity(Contractor)};

encode_contractor(#{<<"contractorType">> := <<"LegalEntity">>} = Contractor) ->
    {legal_entity, encode_legal_entity(Contractor)};

encode_contractor(#{<<"contractorType">> := <<"RegisteredUser">>} = Contractor) ->
    {registered_user, encode_registered_user(Contractor)}.

encode_private_entity(#{<<"entityType">> := <<"RussianPrivateEntity">>} = Entity) ->
    {russian_private_entity, #domain_RussianPrivateEntity{
        first_name = maps:get(<<"firstName">>, Entity),
        second_name = maps:get(<<"secondName">>, Entity),
        middle_name = maps:get(<<"middleName">>, Entity),
        contact_info = encode_contact_info(maps:get(<<"contactInfo">>, Entity))
    }}.

encode_legal_entity(#{<<"entityType">> := <<"RussianLegalEntity">>} = Entity) ->
    {russian_legal_entity , #domain_RussianLegalEntity{
        registered_name = maps:get(<<"registeredName">>, Entity),
        registered_number = maps:get(<<"registeredNumber">>, Entity),
        inn = maps:get(<<"inn">>, Entity),
        actual_address = maps:get(<<"actualAddress">>, Entity),
        post_address = maps:get(<<"postAddress">>, Entity),
        representative_position = maps:get(<<"representativePosition">>, Entity),
        representative_full_name = maps:get(<<"representativeFullName">>, Entity),
        representative_document = maps:get(<<"representativeDocument">>, Entity),
        russian_bank_account = encode_russian_bank_account(maps:get(<<"bankAccount">>, Entity))
    }};
encode_legal_entity(#{
    <<"entityType">> := <<"InternationalLegalEntity">>
} = Entity) ->
    {international_legal_entity, #domain_InternationalLegalEntity{
        legal_name = genlib_map:get(<<"legalName">>, Entity),
        trading_name = genlib_map:get(<<"tradingName">>, Entity),
        registered_address = genlib_map:get(<<"registeredOffice">>, Entity),
        actual_address = genlib_map:get(<<"principalPlaceOfBusiness">>, Entity),
        registered_number = genlib_map:get(<<"registeredNumber">>, Entity)
    }}.

encode_registered_user(#{<<"email">> := Email}) ->
    #domain_RegisteredUser{email = Email}.

encode_payment_institution_ref(Ref) ->
    #domain_PaymentInstitutionRef{
        id = Ref
    }.

encode_residence(undefined) ->
    undefined;
encode_residence(Residence) when is_binary(Residence) ->
    try
        list_to_existing_atom(string:to_lower(binary_to_list(Residence)))
    catch
        error:badarg ->
            throw({encode_residence, invalid_residence})
    end.

decode_residence(Residence) when is_atom(Residence) ->
    list_to_binary(string:to_upper(atom_to_list(Residence))).

encode_flow(#{<<"type">> := <<"PaymentFlowInstant">>}) ->
    {instant, #payproc_InvoicePaymentParamsFlowInstant{}};

encode_flow(#{<<"type">> := <<"PaymentFlowHold">>} = Entity) ->
    OnHoldExpiration = maps:get(<<"onHoldExpiration">>, Entity, <<"cancel">>),
    {hold, #payproc_InvoicePaymentParamsFlowHold{
        on_hold_expiration = binary_to_existing_atom(OnHoldExpiration, utf8)
    }}.

encode_report_preferences(#{<<"serviceAcceptanceActPreferences">> := #{
    <<"scheduleID">> := ScheduleID,
    <<"signer">> := Signer
}}) ->
    #domain_ReportPreferences{
        service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
            schedule = encode_schedule_ref(ScheduleID),
            signer = encode_representative(Signer)
        }
    };
encode_report_preferences(_) ->
    #domain_ReportPreferences{}.

encode_representative(Representative) ->
    #domain_Representative{
        position  = maps:get(<<"position">>, Representative),
        full_name = maps:get(<<"fullName">>, Representative),
        document  = encode_representative_document(maps:get(<<"document">>, Representative))
    }.

encode_representative_document(#{<<"representativeDocumentType">> := <<"ArticlesOfAssociation">>}) ->
    {articles_of_association, #domain_ArticlesOfAssociation{}};
encode_representative_document(#{<<"representativeDocumentType">> := <<"PowerOfAttorney">>} = Document) ->
    {power_of_attorney, encode_legal_agreement(Document)}.

make_invoice_and_token(Invoice, PartyID, Context) ->
    #{
        <<"invoice">> => decode_invoice(Invoice),
        <<"invoiceAccessToken">> => make_invoice_access_token(
            Invoice#domain_Invoice.id,
            PartyID,
            Context
        )
    }.

make_invoice_tpl_and_token(InvoiceTpl, PartyID, Context) ->
    #{
        <<"invoiceTemplate">> => decode_invoice_tpl(InvoiceTpl),
        <<"invoiceTemplateAccessToken">> => make_invoice_tpl_access_token(
            InvoiceTpl#domain_InvoiceTemplate.id,
            PartyID,
            Context
        )
    }.

make_customer_and_token(Customer, PartyID, Context) ->
    #{
        <<"customer">> => decode_customer(Customer),
        <<"customerAccessToken">> => make_customer_access_token(
            Customer#payproc_Customer.id,
            PartyID,
            Context
        )
    }.

make_invoice_access_token(InvoiceID, PartyID, Context) ->
    Fun = fun capi_auth:issue_invoice_access_token/3,
    make_access_token(Fun, InvoiceID, PartyID, Context).

make_invoice_tpl_access_token(InvoiceTplID, PartyID, Context) ->
    Fun = fun capi_auth:issue_invoice_template_access_token/3,
    make_access_token(Fun, InvoiceTplID, PartyID, Context).

make_customer_access_token(CustomerID, PartyID, Context) ->
    Fun = fun capi_auth:issue_customer_access_token/3,
    make_access_token(Fun, CustomerID, PartyID, Context).

make_access_token(Fun, ID, PartyID, _Context) ->
    AdditionalClaims = #{},
    {ok, Token} = Fun(PartyID, ID, AdditionalClaims),
    #{<<"payload">> => Token}.

encode_bank_card(#{
    <<"token">> := Token,
    <<"payment_system">> := PaymentSystem,
    <<"masked_pan">> := MaskedPan
} = BankCard) ->
    {bank_card, #domain_BankCard{
        'token'  = Token,
        'payment_system' = encode_payment_system(PaymentSystem),
        'bin'            = maps:get(<<"bin">>, BankCard, <<>>),
        'masked_pan'     = MaskedPan,
        'token_provider' = encode_token_provider(genlib_map:get(<<"token_provider">>, BankCard)),
        'issuer_country' = encode_residence(genlib_map:get(<<"issuer_country">>, BankCard)),
        'bank_name'      = genlib_map:get(<<"bank_name">>, BankCard),
        'metadata'       = encode_bank_card_metadata(genlib_map:get(<<"metadata">>, BankCard))
    }}.

encode_payment_system(PaymentSystem) ->
    binary_to_existing_atom(PaymentSystem, utf8).

encode_bank_card_metadata(undefined) ->
    undefined;
encode_bank_card_metadata(Meta) ->
    maps:map(fun(_, Data) -> capi_msgp_marshalling:marshal(Data) end, Meta).

encode_payment_terminal(#{<<"terminal_type">> := Type}) ->
    {payment_terminal, #domain_PaymentTerminal{
        terminal_type = binary_to_existing_atom(Type, utf8)
    }}.

encode_digital_wallet(#{<<"provider">> := Provider, <<"id">> := ID}) ->
    {digital_wallet, #domain_DigitalWallet{
        provider = binary_to_existing_atom(Provider, utf8),
        id = ID
    }}.

encode_crypto_wallet(#{<<"crypto_currency">> := CryptoCurrency}) ->
    {crypto_wallet, convert_crypto_currency_from_swag(CryptoCurrency)}.

encode_token_provider(TokenProvider) when TokenProvider /= undefined ->
    binary_to_existing_atom(TokenProvider, utf8);
encode_token_provider(undefined) ->
    undefined.

encode_customer_params(PartyID, Params) ->
    #payproc_CustomerParams{
        party_id = PartyID,
        shop_id = genlib_map:get(<<"shopID">>, Params),
        contact_info = encode_contact_info(genlib_map:get(<<"contactInfo">>, Params)),
        metadata = encode_customer_metadata(genlib_map:get(<<"metadata">>, Params))
    }.

encode_contact_info(ContactInfo) ->
    #domain_ContactInfo{
        phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
        email = genlib_map:get(<<"email">>, ContactInfo)
    }.

encode_customer_metadata(Meta) ->
    capi_json_marshalling:marshal(Meta).

encode_customer_binding_params(#{
    <<"paymentResource">> := #{
        <<"paymentToolToken">> := Token,
        <<"paymentSession">> := EncodedSession
    }
}) ->
    PaymentTool = encode_payment_tool_token(Token),
    {ClientInfo, PaymentSession} = unwrap_payment_session(EncodedSession),
    #payproc_CustomerBindingParams{
        payment_resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = encode_client_info(ClientInfo)
        }
    }.

encode_optional_refund_cash(Params = #{<<"amount">> := _, <<"currency">> := _}, _) ->
    encode_cash(Params);
encode_optional_refund_cash(
    Params = #{<<"amount">> := _},
    #{
        req_ctx := ReqCtx,
        user_info := UserInfo,
        invoice_id := InvoiceID,
        payment_id := PaymentID
    }
) ->
    {ok, #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{
            cost = #domain_Cash{currency = Currency}
        }
    }} = get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID),
    encode_cash(Params#{<<"currency">> => decode_currency(Currency)});
encode_optional_refund_cash(_, _) ->
    undefined.

decode_invoice_event(#payproc_Event{
    id = EventID,
    created_at = CreatedAt,
    payload =  {invoice_changes, InvoiceChanges},
    source =  {invoice_id, InvoiceID} %%@TODO deal with Party source
}, Context) ->
    Changes = decode_invoice_changes(InvoiceID, InvoiceChanges, Context),
    case Changes of
        [_Something | _] ->
            {true, #{
                <<"id">> => EventID,
                <<"createdAt">> => CreatedAt,
                <<"changes">> => Changes
            }};
        [] ->
            false
    end.

decode_invoice_changes(InvoiceID, InvoiceChanges, Context) when is_list(InvoiceChanges) ->
    lists:foldl(
        fun(Change, Acc) ->
            case decode_invoice_change(InvoiceID, Change, Context) of
                #{} = Decoded ->
                    Acc ++ [Decoded];
                undefined ->
                    Acc
            end
        end,
        [],
        InvoiceChanges
    ).

decode_invoice_change(
    _,
    {invoice_created, #payproc_InvoiceCreated{
        invoice = Invoice
    }},
    _Context
) ->
    #{
        <<"changeType">> => <<"InvoiceCreated">>,
        <<"invoice">> => decode_invoice(Invoice)
    };

decode_invoice_change(
    _,
    {invoice_status_changed, #payproc_InvoiceStatusChanged{
        status = {Status, _}
    }},
    _Context
) ->
    #{
        <<"changeType">> => <<"InvoiceStatusChanged">>,
        <<"status">> => genlib:to_binary(Status)
    };

decode_invoice_change(
    InvoiceID,
    {invoice_payment_change, #payproc_InvoicePaymentChange{
        id = PaymentID,
        payload = Change
    }},
    Context
) ->
    decode_payment_change(InvoiceID, PaymentID, Change, Context);

decode_invoice_change(_, _, _) ->
    undefined.

decode_payment_change(
    InvoiceID,
    _PaymentID,
    {invoice_payment_started, #payproc_InvoicePaymentStarted{
        payment = Payment
    }},
    _Context
) ->
    #{
        <<"changeType">> => <<"PaymentStarted">>,
        <<"payment">> => decode_payment(InvoiceID, Payment)
    };

decode_payment_change(
    _InvoiceID,
    PaymentID,
    {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
        payload = {session_interaction_requested,
            #payproc_SessionInteractionRequested{
                interaction = Interaction
            }
        }
    }},
    _Context
) ->
    #{
        <<"changeType">> => <<"PaymentInteractionRequested">>,
        <<"paymentID">> => PaymentID,
        <<"userInteraction">> => decode_user_interaction(Interaction)
    };

decode_payment_change(
    _InvoiceID,
    PaymentID,
    {invoice_payment_status_changed, #payproc_InvoicePaymentStatusChanged{
        status = Status
    }},
    _Context
) ->
    genlib_map:compact(maps:merge(
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID
        },
        decode_payment_status(Status)
    ));

decode_payment_change(
    InvoiceID,
    PaymentID,
    {invoice_payment_refund_change, #payproc_InvoicePaymentRefundChange{
        id = RefundID,
        payload = Change
    }},
    Context
) ->
    decode_refund_change(InvoiceID, PaymentID, RefundID, Change, Context);

decode_payment_change(_, _, _, _) ->
    undefined.

decode_refund_change(
    InvoiceID,
    PaymentID,
    _RefundID,
    {invoice_payment_refund_created, #payproc_InvoicePaymentRefundCreated{
        refund = Refund
    }},
    Context
) ->
    #{
        <<"changeType">> => <<"RefundStarted">>,
        <<"paymentID">> => PaymentID,
        <<"refund">> => decode_refund_for_event(Refund, InvoiceID, PaymentID, Context)
    };

decode_refund_change(
    _InvoiceID,
    PaymentID,
    RefundID,
    {invoice_payment_refund_status_changed, #payproc_InvoicePaymentRefundStatusChanged{
        status = Status
    }},
    _Context
) ->
    genlib_map:compact(maps:merge(
        #{
            <<"changeType">> => <<"RefundStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"refundID">> => RefundID
        },
        decode_refund_status(Status)
    ));

decode_refund_change(_, _, _, _, _) ->
    undefined.

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{
    payment = Payment
}) ->
    decode_payment(InvoiceID, Payment).

decode_payment(InvoiceID, #domain_InvoicePayment{
    id = PaymentID,
    created_at = CreatedAt,
    status = Status,
    payer = Payer,
    cost = #domain_Cash{
        amount = Amount,
        currency = Currency
    },
    flow = Flow
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"createdAt">> => CreatedAt,
        % TODO whoops, nothing to get it from yet
        <<"flow">> => decode_flow(Flow),
        <<"amount">> => Amount,
        <<"currency">> => decode_currency(Currency),
        <<"payer">> => decode_payer(Payer)
    }, decode_payment_status(Status))).

decode_payer({customer, #domain_CustomerPayer{
    customer_id = ID
}}) ->
    #{
        <<"payerType">> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_payer({payment_resource, #domain_PaymentResourcePayer{
    resource = DisposablePaymentResource,
    contact_info = ContactInfo
}}) ->
    maps:merge(
        #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"contactInfo">> => decode_contact_info(ContactInfo)
        },
        decode_disposable_payment_resource(DisposablePaymentResource)
    ).

decode_payment_tool_token({bank_card, BankCard}) ->
    decode_bank_card(BankCard);
decode_payment_tool_token({payment_terminal, PaymentTerminal}) ->
    decode_payment_terminal(PaymentTerminal);
decode_payment_tool_token({digital_wallet, DigitalWallet}) ->
    decode_digital_wallet(DigitalWallet);
decode_payment_tool_token({crypto_currency, CryptoCurrency}) ->
    decode_crypto_wallet(CryptoCurrency).

decode_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>});
decode_payment_tool_details({crypto_currency, CryptoCurrency}) ->
    #{
        <<"detailsType">> => <<"PaymentToolDetailsCryptoWallet">>,
        <<"cryptoCurrency">> => convert_crypto_currency_to_swag(CryptoCurrency)
    }.

decode_bank_card_details(BankCard, V) ->
    LastDigits = decode_last_digits(BankCard#domain_BankCard.masked_pan),
    Bin = decode_bank_card_bin(BankCard#domain_BankCard.bin),
    merge_and_compact(V, #{
        <<"lastDigits">>     => LastDigits,
        <<"bin">>            => Bin,
        <<"cardNumberMask">> => decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem" >> => genlib:to_binary(BankCard#domain_BankCard.payment_system),
        <<"tokenProvider" >> => decode_token_provider(BankCard#domain_BankCard.token_provider)
    }).

decode_bank_card_bin(<<>>) ->
    undefined;
decode_bank_card_bin(Bin) when is_binary(Bin) ->
    Bin.

decode_token_provider(Provider) when Provider /= undefined ->
    genlib:to_binary(Provider);
decode_token_provider(undefined) ->
    undefined.

decode_payment_terminal_details(#domain_PaymentTerminal{
    terminal_type = Type
}, V) ->
    V#{
        <<"provider">> => genlib:to_binary(Type)
    }.

decode_digital_wallet_details(#domain_DigitalWallet{
    provider = qiwi,
    id = ID
}, V) ->
    V#{
        <<"digitalWalletDetailsType">> => <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask">> => mask_phone_number(ID)
    }.

-define(MASKED_PAN_MAX_LENGTH, 4).

decode_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_last_digits(MaskedPan) ->
    MaskedPan.

-define(PAN_LENGTH, 16).

decode_masked_pan(Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, ?PAN_LENGTH - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

mask_phone_number(PhoneNumber) ->
    capi_utils:redact(PhoneNumber, <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>).

decode_contact_info(#domain_ContactInfo{
    phone_number = PhoneNumber,
    email = Email
}) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => PhoneNumber,
        <<"email">> => Email
    }).

decode_payment_status({Status, StatusInfo}) ->
    Error = case StatusInfo of
        #domain_InvoicePaymentFailed{failure = OperationFailure} ->
            decode_operation_failure(OperationFailure);
        _ ->
            undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_operation_failure({operation_timeout, _}) ->
    logic_error(timeout, <<"timeout">>);
decode_operation_failure({failure, #domain_Failure{
    code = Code,
    reason = Reason
}}) ->
    logic_error(Code, Reason).

decode_stat_payment(#merchstat_StatPayment{
    id = PaymentID,
    short_id = ShortID,
    invoice_id = InvoiceID,
    shop_id = ShopID,
    created_at = CreatedAt,
    status = Status,
    amount = Amount,
    flow = Flow,
    fee = Fee,
    currency_symbolic_code = Currency,
    payer = Payer,
    context = RawContext,
    location_info = Location
}) ->
    merge_and_compact(#{
        <<"id">> =>  PaymentID,
        <<"shortID">> => ShortID,
        <<"invoiceID">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"amount">> => Amount,
        <<"flow">> => decode_stat_payment_flow(Flow),
        <<"fee">> => Fee,
        <<"currency">> => Currency,
        <<"payer">> => decode_stat_payer(Payer),
        <<"geoLocationInfo">> => decode_geo_location_info(Location),
        <<"metadata">> =>  decode_context(RawContext)
    }, decode_stat_payment_status(Status)).

decode_stat_payer({customer, #merchstat_CustomerPayer{
    customer_id = ID
}}) ->
    #{
        <<"payerType">> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_stat_payer({payment_resource, #merchstat_PaymentResourcePayer{
    payment_tool = PaymentTool,
    session_id = PaymentSession,
    fingerprint = Fingerprint,
    ip_address = IP,
    phone_number = PhoneNumber,
    email = Email
}}) ->
    #{
        <<"payerType">> => <<"PaymentResourcePayer">>,
        <<"paymentToolToken">> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"clientInfo">> => #{
            <<"ip">> => IP,
            <<"fingerprint">> => Fingerprint
        },
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email">> => Email
        })
    }.

decode_stat_payment_tool_token(PaymentTool) ->
    decode_payment_tool_token(merchstat_to_domain(PaymentTool)).

decode_stat_payment_tool_details(PaymentTool) ->
    decode_payment_tool_details(merchstat_to_domain(PaymentTool)).

decode_stat_payment_status(PaymentStatus) ->
    decode_payment_status(merchstat_to_domain(PaymentStatus)).

decode_stat_payment_flow(Flow) ->
    decode_flow(merchstat_to_domain(Flow)).

decode_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};

decode_flow({hold, #domain_InvoicePaymentFlowHold{
    'on_hold_expiration' = OnHoldExpiration,
    'held_until' = HeldUntil
}}) ->
    #{
        <<"type">> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil">> => HeldUntil
    }.

merchstat_to_domain({bank_card, #merchstat_BankCard{
    'token' = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan,
    'token_provider' = TokenProvider
}}) ->
    {bank_card, #domain_BankCard{
        'token' = Token,
        'payment_system' = PaymentSystem,
        'bin' = Bin,
        'masked_pan' = MaskedPan,
        'token_provider' = TokenProvider
    }};

merchstat_to_domain({payment_terminal, #merchstat_PaymentTerminal{
    terminal_type = Type}}
) ->
    {payment_terminal, #domain_PaymentTerminal{terminal_type = Type}};
merchstat_to_domain({digital_wallet, #merchstat_DigitalWallet{
    provider = Provider,
    id       = ID
}}) ->
    {digital_wallet, #domain_DigitalWallet{
        provider = Provider,
        id       = ID
    }};
merchstat_to_domain({bank_card, #merchstat_PayoutCard{card = BankCard}}) ->
    merchstat_to_domain({bank_card, BankCard});
merchstat_to_domain({bank_account,
    {russian_payout_account, #merchstat_RussianPayoutAccount{
        bank_account = #merchstat_RussianBankAccount{
            account = Account,
            bank_name = BankName,
            bank_post_account = BankPostAccount,
            bank_bik = BankBik
        }
    }}
}) ->
    {russian_bank_account, #domain_RussianBankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
    }};

merchstat_to_domain({bank_account, {international_payout_account, PayoutAccount}}) ->
    #merchstat_InternationalPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {international_bank_account, merchstat_to_domain({international_bank_account, BankAccount})};
merchstat_to_domain({international_bank_account, undefined}) ->
    undefined;
merchstat_to_domain({international_bank_account, BankAccount = #merchstat_InternationalBankAccount{}}) ->
    #domain_InternationalBankAccount{
        number         = BankAccount#merchstat_InternationalBankAccount.number,
        iban           = BankAccount#merchstat_InternationalBankAccount.iban,
        account_holder = BankAccount#merchstat_InternationalBankAccount.account_holder,
        bank           = merchstat_to_domain(
            {international_bank_details, BankAccount#merchstat_InternationalBankAccount.bank}
        ),
        correspondent_account = merchstat_to_domain(
            {international_bank_account, BankAccount#merchstat_InternationalBankAccount.correspondent_account}
        )
    };
merchstat_to_domain({international_bank_details, undefined}) ->
    undefined;
merchstat_to_domain({international_bank_details, Bank = #merchstat_InternationalBankDetails{}}) ->
    #domain_InternationalBankDetails{
            bic     = Bank#merchstat_InternationalBankDetails.bic,
            name    = Bank#merchstat_InternationalBankDetails.name,
            address = Bank#merchstat_InternationalBankDetails.address,
            country = Bank#merchstat_InternationalBankDetails.country,
            aba_rtn = Bank#merchstat_InternationalBankDetails.aba_rtn
    };

merchstat_to_domain({Status, #merchstat_InvoicePaymentPending{}}) ->
    {Status, #domain_InvoicePaymentPending{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentProcessed{}}) ->
    {Status, #domain_InvoicePaymentProcessed{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentCaptured{}}) ->
    {Status, #domain_InvoicePaymentCaptured{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentCancelled{}}) ->
    {Status, #domain_InvoicePaymentCancelled{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentRefunded{}}) ->
    {Status, #domain_InvoicePaymentRefunded{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentFailed{
    failure = {failure, Failure}
}}) ->
    {Status, #domain_InvoicePaymentFailed{
        failure = {failure, Failure}
    }};
merchstat_to_domain({Status, #merchstat_InvoicePaymentFailed{
    failure = {operation_timeout, #merchstat_OperationTimeout{}}
}}) ->
    {Status, #domain_InvoicePaymentFailed{
        failure = {operation_timeout, #domain_OperationTimeout{}}
    }};
merchstat_to_domain({Status, #merchstat_InvoiceUnpaid{}}) ->
    {Status, #domain_InvoiceUnpaid{}};
merchstat_to_domain({Status, #merchstat_InvoicePaid{}}) ->
    {Status, #domain_InvoicePaid{}};
merchstat_to_domain({Status, #merchstat_InvoiceCancelled{details = Details}}) ->
    {Status, #domain_InvoiceCancelled{details = Details}};
merchstat_to_domain({Status, #merchstat_InvoiceFulfilled{details = Details}}) ->
    {Status, #domain_InvoiceFulfilled{details = Details}};
merchstat_to_domain({instant, #merchstat_InvoicePaymentFlowInstant{}}) ->
    {instant, #domain_InvoicePaymentFlowInstant{}};
merchstat_to_domain({hold, #merchstat_InvoicePaymentFlowHold{
    on_hold_expiration = OnHoldExpiration,
    held_until         = HeldUntil
}}) ->
    {hold, #domain_InvoicePaymentFlowHold{
        on_hold_expiration = OnHoldExpiration,
        held_until         = HeldUntil
    }}.

decode_invoice(#domain_Invoice{
    id = InvoiceID,
    created_at = CreatedAt,
    status = InvoiceStatus,
    due  = DueDate,
    details = #domain_InvoiceDetails{
        product = Product,
        description = Description,
        cart = Cart
    },
    cost = #domain_Cash{
        amount = Amount,
        currency = Currency
    },
    context = RawContext,
    shop_id = ShopID
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"dueDate">> => DueDate,
        <<"amount">> => Amount,
        <<"currency">> =>  decode_currency(Currency),
        <<"metadata">> =>  decode_context(RawContext),
        <<"product">> => Product,
        <<"description">> => Description,
        <<"cart">> => decode_invoice_cart(Cart)
    }, decode_invoice_status(InvoiceStatus))).

decode_invoice_status({Status, StatusInfo}) ->
    Reason = case StatusInfo of
        #domain_InvoiceCancelled{details = Details} -> Details;
        #domain_InvoiceFulfilled{details = Details} -> Details;
        _ -> undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"reason">> => Reason
    }.

decode_invoice_cart(#domain_InvoiceCart{lines = Lines}) ->
    [decode_invoice_line(L) || L <- Lines];
decode_invoice_cart(undefined) ->
    undefined.

decode_invoice_line(#domain_InvoiceLine{
    product = Product,
    quantity = Quantity,
    price = #domain_Cash{amount = Price},
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        <<"product">> => Product,
        <<"quantity">> => Quantity,
        <<"price">> => Price,
        <<"cost">> => Price * Quantity,
        <<"taxMode">> => decode_invoice_line_tax_mode(Metadata)
    }).

decode_invoice_line_tax_mode(#{<<"TaxMode">> := {str, TM}}) ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    #{
       <<"type">> => <<"InvoiceLineTaxVAT">>,
       <<"rate">> => TM
    };
decode_invoice_line_tax_mode(_) ->
    undefined.

decode_invoice_tpl(#domain_InvoiceTemplate{
    id = InvoiceTplID,
    shop_id = ShopID,
    product = Product,
    description = Description,
    invoice_lifetime = #domain_LifetimeInterval{
        days = DD,
        months = MM,
        years = YY
    },
    details = {product, #domain_InvoiceTemplateProduct{
        product = Product,
        price = Cost
    }},
    context = RawContext
}) ->
    genlib_map:compact(#{
        <<"id">> => InvoiceTplID,
        <<"shopID">> => ShopID,
        <<"product">> => Product,
        <<"description">> => Description,
        <<"lifetime">> => #{
            <<"days">>   => undef_to_zero(DD),
            <<"months">> => undef_to_zero(MM),
            <<"years">>  => undef_to_zero(YY)
        },
        <<"cost">> => decode_invoice_tpl_cost(Cost),
        <<"metadata">> => decode_context(RawContext)
    }).

undef_to_zero(undefined) ->
    0;
undef_to_zero(Int) ->
    Int.

decode_context(#'Content'{
    type = <<"application/json">>,
    data = InvoiceContext
}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);

decode_context(undefined) ->
    undefined.

decode_invoice_tpl_cost({unlim, _}) ->
    #{
        <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostUnlim">>
    };

decode_invoice_tpl_cost({fixed, #domain_Cash{amount = Amount, currency = Currency}}) ->
    #{
        <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostFixed">>,
        <<"currency">> => decode_currency(Currency),
        <<"amount">> => Amount
    };

decode_invoice_tpl_cost({range, #domain_CashRange{
    upper = {_, #domain_Cash{amount = UpperBound, currency = Currency}},
    lower = {_, #domain_Cash{amount = LowerBound, currency = Currency}}
}}) ->
    #{
        <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostRange">>,
        <<"currency">> => decode_currency(Currency),
        <<"range">> => #{
            <<"upperBound">> => UpperBound,
            <<"lowerBound">> => LowerBound
        }
    }.

decode_party(#domain_Party{
    id = PartyID,
    blocking = Blocking,
    suspension = Suspension
}) ->
    #{
        <<"id">> => PartyID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension)
    }.

decode_contracts_map(Contracts, Contractors) ->
    decode_map(Contracts, fun(C) -> decode_contract(C, Contractors) end).

decode_shops_map(Shops) ->
    decode_map(Shops, fun (S) -> decode_shop(S) end).

decode_map(Items, Fun) ->
    lists:map(Fun, maps:values(Items)).

decode_contract(Contract, Contractors) ->
    merge_and_compact(#{
        <<"id"                  >> => Contract#domain_Contract.id,
        <<"createdAt"           >> => Contract#domain_Contract.created_at,
        <<"contractor"          >> => decode_contractor(get_contractor(Contract, Contractors)),
        <<"paymentInstitutionID">> => decode_payment_institution_ref(Contract#domain_Contract.payment_institution),
        <<"validSince"          >> => Contract#domain_Contract.valid_since,
        <<"validUntil"          >> => Contract#domain_Contract.valid_until,
        <<"legalAgreement"      >> => decode_optional(
            Contract#domain_Contract.legal_agreement,
            fun decode_legal_agreement/1
        ),
        <<"reportingPreferences">> => decode_optional(
            Contract#domain_Contract.report_preferences,
            fun decode_reporting_preferences/1
        )
    }, decode_contract_status(Contract#domain_Contract.status)).

decode_contract_status({active, _}) ->
    #{
        <<"status">> => <<"active">>
    };

decode_contract_status({terminated, #domain_ContractTerminated{
    terminated_at = TerminatedAt
}}) ->
    #{
        <<"status">> => <<"terminated">>,
        <<"terminatedAt">> => TerminatedAt
    }.

get_contractor(#domain_Contract{contractor = Contractor}, _) when Contractor =/= undefined ->
    Contractor;
get_contractor(#domain_Contract{contractor_id = ContractorID}, Contractors) ->
    #domain_PartyContractor{
        contractor = Contractor
    } = maps:get(ContractorID, Contractors),
    Contractor.

decode_payout_tool(#domain_PayoutTool{id = ID, currency = Currency, payout_tool_info = Info}) ->
    maps:merge(
        #{<<"id">> => ID},
        decode_payout_tool_params(Currency, Info)
    ).

decode_payout_tool_params(#payproc_PayoutToolParams{
    currency = Currency,
    tool_info = ToolInfo
}) ->
    decode_payout_tool_params(Currency, ToolInfo).

decode_payout_tool_params(Currency, Info) ->
    #{
        <<"currency">> => decode_currency(Currency),
        <<"details">> => decode_payout_tool_details(Info)
    }.

decode_russian_bank_account(#domain_RussianBankAccount{
    account = Account,
    bank_name = BankName,
    bank_post_account = BankPostAccount,
    bank_bik = BankBik
}, V) ->
    V#{
        <<"account">> => Account,
        <<"bankName">> => BankName,
        <<"bankPostAccount">> => BankPostAccount,
        <<"bankBik">> => BankBik
    }.

decode_international_bank_account(BankAccount = #domain_InternationalBankAccount{bank = Bank}, V) ->
    genlib_map:compact(V#{
        <<"accountHolder">> => BankAccount#domain_InternationalBankAccount.account_holder,
        <<"iban">> => BankAccount#domain_InternationalBankAccount.iban,
        <<"bankName">> => Bank#domain_InternationalBankDetails.name,
        <<"bankAddress">> => Bank#domain_InternationalBankDetails.address,
        <<"bic">> => Bank#domain_InternationalBankDetails.bic
    }).

decode_contract_adjustment(#domain_ContractAdjustment{
    id = ID,
    created_at = CreatedAt,
    valid_since = ValidSince,
    valid_until = ValidUntil
}) ->
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"createdAt">> => CreatedAt,
        <<"validSince">> => ValidSince,
        <<"validUntil">> => ValidUntil
    }).

decode_payment_institution_ref(#domain_PaymentInstitutionRef{id = Ref}) ->
    Ref.

decode_shop(Shop) ->
    genlib_map:compact(#{
        <<"id"          >> => Shop#domain_Shop.id,
        <<"createdAt"   >> => Shop#domain_Shop.created_at,
        <<"isBlocked"   >> => is_blocked(Shop#domain_Shop.blocking),
        <<"isSuspended" >> => is_suspended(Shop#domain_Shop.suspension),
        <<"categoryID"  >> => decode_category_ref(Shop#domain_Shop.category),
        <<"details"     >> => decode_shop_details(Shop#domain_Shop.details),
        <<"location"    >> => decode_shop_location(Shop#domain_Shop.location),
        <<"contractID"  >> => Shop#domain_Shop.contract_id,
        <<"payoutToolID">> => Shop#domain_Shop.payout_tool_id,
        <<"scheduleID"  >> => decode_business_schedule_ref(Shop#domain_Shop.payout_schedule),
        <<"account"     >> => decode_shop_account(Shop#domain_Shop.account)
    }).

decode_shop_details(#domain_ShopDetails{
    name = Name,
    description = Description
}) ->
    genlib_map:compact(#{
      <<"name">> => Name,
      <<"description">> => Description
    }).

decode_shop_location({url, Location}) ->
    #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => Location
    }.

decode_contractor({legal_entity, LegalEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"LegalEntity">>}, decode_legal_entity(LegalEntity));

decode_contractor({private_entity, PrivateEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"PrivateEntity">>}, decode_private_entity(PrivateEntity));

decode_contractor({registered_user, RegisteredUser}) ->
    maps:merge(#{<<"contractorType">> => <<"RegisteredUser">>}, decode_registered_user(RegisteredUser)).

decode_legal_entity({
    russian_legal_entity,
    #domain_RussianLegalEntity{
        registered_name = RegisteredName,
        registered_number = RegisteredNumber,
        inn = Inn,
        actual_address = ActualAddress,
        post_address = PostAddress,
        representative_position = RepresentativePosition,
        representative_full_name = RepresentativeFullName,
        representative_document = RepresentativeDocument,
        russian_bank_account = BankAccount
    }
}) ->
    #{
        <<"entityType">> => <<"RussianLegalEntity">>,
        <<"registeredName">> => RegisteredName,
        <<"registeredNumber">> => RegisteredNumber,
        <<"inn">> => Inn,
        <<"actualAddress">> => ActualAddress,
        <<"postAddress">> => PostAddress,
        <<"representativePosition">> => RepresentativePosition,
        <<"representativeFullName">> => RepresentativeFullName,
        <<"representativeDocument">> => RepresentativeDocument,
        <<"bankAccount">> => decode_russian_bank_account(BankAccount, #{})
    };
decode_legal_entity({
    international_legal_entity,
    #domain_InternationalLegalEntity{
        legal_name = LegalName,
        trading_name = TradingName,
        registered_address = RegisteredOffice,
        actual_address = PrincipalPlaceOfBusiness,
        registered_number = RegisteredNumber
    }
}) ->
    genlib_map:compact(#{
        <<"entityType">> => <<"InternationalLegalEntity">>,
        <<"legalName">> => LegalName,
        <<"tradingName">> => TradingName,
        <<"registeredOffice">> => RegisteredOffice,
        <<"principalPlaceOfBusiness">> => PrincipalPlaceOfBusiness,
        <<"registeredNumber">> => RegisteredNumber
    }).

decode_private_entity({russian_private_entity, PrivateEntity}) ->
    #{
        <<"entityType">>    => <<"RussianPrivateEntity">>,
        <<"firstName">>     => PrivateEntity#domain_RussianPrivateEntity.first_name,
        <<"secondName">>    => PrivateEntity#domain_RussianPrivateEntity.second_name,
        <<"middleName">>    => PrivateEntity#domain_RussianPrivateEntity.middle_name,
        <<"contactInfo">>   => decode_contact_info(PrivateEntity#domain_RussianPrivateEntity.contact_info)
    }.

decode_registered_user(#domain_RegisteredUser{email = Email}) ->
    #{<<"email">> => Email}.

decode_payment_institution_obj(#domain_PaymentInstitutionObject{
    ref = #domain_PaymentInstitutionRef{id = ID},
    data = #domain_PaymentInstitution{
        name = Name,
        description = Description,
        realm = Realm,
        residences = Residences
    }
}) ->
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"realm">> => genlib:to_binary(Realm),
        <<"residences">> => [decode_residence(R) || R <- ordsets:to_list(Residences)]
    }).

is_blocked({blocked, _}) ->
    true;
is_blocked({unblocked, _}) ->
    false.

is_suspended({suspended, _}) ->
    true;
is_suspended({active, _}) ->
    false.

decode_stat_info(payments_conversion_stat, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"successfulCount">> => genlib:to_int(maps:get(<<"successful_count">>, Response)),
        <<"totalCount">> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"conversion">> => genlib:to_float(maps:get(<<"conversion">>, Response))
    };

decode_stat_info(payments_geo_stat, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"geoID">> => genlib:to_int(maps:get(<<"city_id">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_info(payments_turnover, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_info(customers_rate_stat, Response) ->
    #{
        <<"uniqueCount">> => genlib:to_int(maps:get(<<"unic_count">>, Response))
    };

decode_stat_info(payments_pmt_cards_stat, Response) ->
    #{
        <<"statType">> => <<"PaymentMethodBankCardStat">>, %% @TODO deal with nested responses decoding
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"totalCount">> =>  genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"paymentSystem">> =>  maps:get(<<"payment_system">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> =>  genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    }.

decode_stat_invoice(#merchstat_StatInvoice{
    id = InvoiceID,
    shop_id = ShopID,
    created_at = CreatedAt,
    status = InvoiceStatus,
    product = Product,
    description = Description,
    due = DueDate,
    amount = Amount,
    currency_symbolic_code = Currency,
    context = RawContext,
    cart = Cart
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"dueDate">> => DueDate,
        <<"amount">> => Amount,
        <<"currency">> =>  Currency,
        <<"metadata">> =>  decode_context(RawContext),
        <<"product">> => Product,
        <<"description">> => Description,
        <<"cart">> => decode_invoice_cart(Cart)
    }, decode_stat_invoice_status(InvoiceStatus))).

decode_stat_invoice_status(Status) ->
    decode_invoice_status(merchstat_to_domain(Status)).

decode_refund_for_event(
    #domain_InvoicePaymentRefund{cash = #domain_Cash{}} = Refund,
    _InvoiceID,
    _PaymentID,
    _Context
) ->
    decode_refund(Refund);
decode_refund_for_event(
    #domain_InvoicePaymentRefund{cash = undefined} = Refund,
    InvoiceID,
    PaymentID,
    #{req_ctx := ReqCtx, user_info := UserInfo}
) ->
    % Need to fix it!
    {ok, #payproc_InvoicePayment{payment = #domain_InvoicePayment{cost = Cash}}} =
        get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID),
    decode_refund(Refund#domain_InvoicePaymentRefund{cash = Cash}).

decode_refund(#domain_InvoicePaymentRefund{
    id = ID,
    status = Status,
    created_at = CreatedAt,
    reason = Reason,
    cash = #domain_Cash{
        amount = Amount,
        currency = Currency
    }
}) ->
    genlib_map:compact(maps:merge(
        #{
            <<"id">> => ID,
            <<"createdAt">> => CreatedAt,
            <<"reason">> => Reason,
            <<"amount">> => Amount,
            <<"currency">> => decode_currency(Currency)
        },
        decode_refund_status(Status)
    )).

decode_refund_status({Status, StatusInfo}) ->
    Error = case StatusInfo of
        #domain_InvoicePaymentRefundFailed{failure = OperationFailure} ->
            decode_operation_failure(OperationFailure);
        _ ->
            undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_stat_payout(#merchstat_StatPayout{
    id = PayoutID,
    shop_id = ShopID,
    created_at = CreatedAt,
    status = PayoutStatus,
    amount = Amount,
    fee = Fee,
    currency_symbolic_code = Currency,
    type = PayoutType,
    summary = PayoutSummary
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> => PayoutID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"amount">> => Amount,
        <<"fee">> => Fee,
        <<"currency">> => Currency,
        <<"payoutToolDetails">> => decode_stat_payout_tool_details(PayoutType),
        <<"payoutSummary">> => decode_stat_payout_summary(PayoutSummary)
    }, decode_stat_payout_status(PayoutStatus))).

decode_stat_payout_status({cancelled, #merchstat_PayoutCancelled{details = Details}}) ->
    #{
        <<"status">> => <<"cancelled">>,
        <<"cancellationDetails">> => genlib:to_binary(Details)
    };
decode_stat_payout_status({Status, _}) ->
    #{
        <<"status">> => genlib:to_binary(Status)
    }.

decode_stat_payout_tool_details(PayoutType) ->
    decode_payout_tool_details(merchstat_to_domain(PayoutType)).

decode_payout_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankCard">>});
decode_payout_tool_details({russian_bank_account, V}) ->
    decode_russian_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankAccount">>});
decode_payout_tool_details({international_bank_account, V}) ->
    decode_international_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>}).

decode_stat_payout_summary(PayoutSummary) when is_list(PayoutSummary) ->
    [decode_stat_payout_summary_item(PayoutSummaryItem) || PayoutSummaryItem <- PayoutSummary];
decode_stat_payout_summary(undefined) ->
    undefined.

decode_stat_payout_summary_item(#merchstat_PayoutSummaryItem{
    amount = Amount,
    fee = Fee,
    currency_symbolic_code = Currency,
    from_time = FromTime,
    to_time = ToTime,
    operation_type = OperationType,
    count = Count
}) ->
    genlib_map:compact(#{
        <<"amount">> => Amount,
        <<"fee">> => Fee,
        <<"currency">> => Currency,
        <<"count">> => Count,
        <<"fromTime">> => FromTime,
        <<"toTime">> => ToTime,
        <<"type">> => genlib:to_binary(OperationType)
    }).

encode_payout_type('PayoutCard') ->
    <<"bank_card">>;
encode_payout_type('PayoutAccount') ->
    <<"bank_account">>;
encode_payout_type(undefined) ->
    undefined.

create_dsl(QueryType, QueryBody, QueryParams) when
    is_atom(QueryType),
    is_map(QueryBody),
    is_map(QueryParams) ->
    Query = maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{}),
    Basic = #{
        <<"query">> => Query
    },
    maps:merge(Basic, genlib_map:compact(QueryParams)).

encode_payment_method('bankCard') ->
    <<"bank_card">>;
encode_payment_method('paymentTerminal') ->
    <<"payment_terminal">>;
encode_payment_method(undefined) ->
    undefined.

filter_claims(ClaimStatus, Claims) ->
    lists:filter(
        fun(C) ->
            is_claim_status_equals(ClaimStatus, C)
            andalso
            (not is_wallet_claim(C))
        end,
        Claims
    ).

is_claim_status_equals(undefined, _) ->
    true;
is_claim_status_equals(ClaimStatus, #payproc_Claim{status = {Status, _}}) ->
    Status =:= ClaimStatus.

decode_claims(Claims) ->
    lists:map(fun decode_claim/1, Claims).

decode_claim(#payproc_Claim{
    id = ID,
    revision = Revision,
    created_at = CreatedAt,
    updated_at = UpdatedAt,
    status = Status,
    changeset = ChangeSet
}) ->
    merge_and_compact(
        #{
            <<"id">> => ID,
            <<"revision">> => Revision,
            <<"createdAt">> => CreatedAt,
            <<"updatedAt">> => UpdatedAt,
            <<"changeset">> => decode_party_changeset(ChangeSet)
        },
        decode_claim_status(Status)
    ).

is_wallet_claim(#payproc_Claim{changeset = Changeset}) ->
    lists:any(fun is_wallet_change/1, Changeset).

is_wallet_change({contractor_modification, _}) ->
    true;
is_wallet_change({wallet_modification, _}) ->
    true;
is_wallet_change({contract_modification, #payproc_ContractModificationUnit{
    modification = {creation, #payproc_ContractParams{
        contractor = undefined
    }}
}}) ->
    true;
is_wallet_change({contract_modification, #payproc_ContractModificationUnit{
    modification = {contractor_modification, _}
}}) ->
    true;
is_wallet_change(_) ->
    false.

decode_claim_status({'pending', _}) ->
    #{
        <<"status">> => <<"ClaimPending">>
    };

decode_claim_status({'accepted', #payproc_ClaimAccepted{}}) ->
    #{
        <<"status">> => <<"ClaimAccepted">>
    };

decode_claim_status({'denied', #payproc_ClaimDenied{
    reason = Reason
}}) ->
    #{
        <<"status">> => <<"ClaimDenied">>,
        <<"reason">> => Reason
    };

decode_claim_status({'revoked', #payproc_ClaimRevoked{
    reason = Reason
}}) ->
    #{
        <<"status">> => <<"ClaimRevoked">>,
        <<"reason">> => Reason
    }.

decode_party_changeset(PartyChangeset) ->
    lists:filtermap(fun decode_party_modification/1, PartyChangeset).

decode_party_modification({
    contract_modification,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = Modification
    }
}) ->
    {true, maps:merge(
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ContractID
        },
        decode_contract_modification(Modification)
    )};

decode_party_modification({
    shop_modification,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = ShopModification
    }
}) ->
    {true, maps:merge(
        #{
            <<"partyModificationType">> => <<"ShopModification">>,
            <<"shopID">> => ShopID
        },
        decode_shop_modification(ShopModification)
    )}.

decode_contract_modification({creation, #payproc_ContractParams{
    contractor = Contractor,
    payment_institution = PaymentInstitutionRef
}}) ->
    #{
        <<"contractModificationType">> => <<"ContractCreation">>,
        <<"contractor">> => decode_contractor(Contractor),
        <<"paymentInstitutionID">> => decode_payment_institution_ref(PaymentInstitutionRef)
    };

decode_contract_modification({legal_agreement_binding, LegalAgreement}) ->
    #{
        <<"contractModificationType">> => <<"ContractLegalAgreementBinding">>,
        <<"legalAgreement">> => decode_legal_agreement(LegalAgreement)
    };

decode_contract_modification({adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
    adjustment_id = AdjustmentID,
    modification = {creation, #payproc_ContractAdjustmentParams{
        % FIXME need swag support for this
        template = _Template
    }}
}}) ->
    #{
        <<"contractModificationType">> => <<"ContractAdjustmentCreation">>,
        <<"adjustmentID">> => AdjustmentID
    };

decode_contract_modification({termination, #payproc_ContractTermination{
    reason = Reason
}}) ->
    genlib_map:compact(#{
        <<"contractModificationType">> => <<"ContractTermination">>,
        <<"reason">> => Reason
    });

decode_contract_modification({payout_tool_modification, #payproc_PayoutToolModificationUnit{
    payout_tool_id = PayoutToolID,
    modification = {creation, PayoutToolParams}
}}) ->
    maps:merge(#{
        <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
        <<"payoutToolID"            >> => PayoutToolID
    }, decode_payout_tool_params(PayoutToolParams));

decode_contract_modification({report_preferences_modification, ReportPreferences}) ->
    maps:merge(
        #{<<"contractModificationType">> => <<"ContractReportingPreferencesChange">>},
        decode_reporting_preferences(ReportPreferences)
    ).

decode_legal_agreement(
    #domain_LegalAgreement{
        signed_at = SignedAt,
        legal_agreement_id = ID,
        valid_until = ValidUntil
    }
) ->
    genlib_map:compact(#{
        <<"id"      >> => ID,
        <<"signedAt">> => SignedAt,
        <<"validUntil">> => ValidUntil
    }).

decode_reporting_preferences(#domain_ReportPreferences{
    service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
        schedule = ScheduleRef,
        signer = Signer
    }
}) ->
    #{
        <<"serviceAcceptanceActPreferences">> => #{
            <<"scheduleID">> => decode_business_schedule_ref(ScheduleRef),
            <<"signer">> => decode_representative(Signer)
        }
    };
decode_reporting_preferences(#domain_ReportPreferences{service_acceptance_act_preferences = undefined}) ->
    #{}.

decode_representative(#domain_Representative{
    position  = Position,
    full_name = Name,
    document  = Document
}) ->
    #{
        <<"position">> => Position,
        <<"fullName">> => Name,
        <<"document">> => decode_representative_document(Document)
    }.

decode_representative_document({articles_of_association, #domain_ArticlesOfAssociation{}}) ->
    #{
        <<"representativeDocumentType">> => <<"ArticlesOfAssociation">>
    };
decode_representative_document({power_of_attorney, LegalAgreement}) ->
    maps:merge(
        #{<<"representativeDocumentType">> => <<"PowerOfAttorney">>},
        decode_legal_agreement(LegalAgreement)
    ).

decode_shop_modification({creation, ShopParams}) ->
    maps:merge(
        #{<<"shopModificationType">> => <<"ShopCreation">>},
        decode_shop_params(ShopParams)
    );

decode_shop_modification({shop_account_creation, #payproc_ShopAccountParams{
    currency = Currency
}}) ->
    #{
        <<"shopModificationType">> => <<"ShopAccountCreation">>,
        <<"currency">> => decode_currency(Currency)
    };

decode_shop_modification({category_modification, CategoryRef}) ->
    #{
        <<"shopModificationType">> => <<"ShopCategoryChange">>,
        <<"categoryID">> => decode_category_ref(CategoryRef)
    };

decode_shop_modification({location_modification, Location}) ->
    #{
        <<"shopModificationType">> => <<"ShopLocationChange">>,
        <<"location">> => decode_shop_location(Location)
    };

decode_shop_modification({details_modification, Details}) ->
    #{
        <<"shopModificationType">> => <<"ShopDetailsChange">>,
        <<"details">> => decode_shop_details(Details)
    };

decode_shop_modification({contract_modification, #payproc_ShopContractModification{
    contract_id = ContractID,
    payout_tool_id = PayoutToolID
}}) ->
    #{
        <<"shopModificationType">> => <<"ShopContractBinding">>,
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID
    };

decode_shop_modification({payout_tool_modification, PayoutToolID}) ->
    #{
        <<"shopModificationType">> => <<"ShopPayoutToolChange">>,
        <<"payoutToolID">> => PayoutToolID
    };

decode_shop_modification({payout_schedule_modification, #payproc_ScheduleModification{
    schedule = ScheduleRef
}}) ->
    genlib_map:compact(#{
        <<"shopModificationType">> => <<"ShopPayoutScheduleChange">>,
        <<"scheduleID"          >> => decode_business_schedule_ref(ScheduleRef)
    }).

decode_shop_params(#payproc_ShopParams{
    location =  Location,
    details = Details,
    contract_id = ContractID,
    payout_tool_id = PayoutToolID
}) ->
    #{
        <<"location">> => decode_shop_location(Location),
        <<"details">> => decode_shop_details(Details),
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID
    }.

decode_category(#domain_CategoryObject{
    ref = #domain_CategoryRef{
        id = CategoryID
    },
    data = #domain_Category{
        name = Name,
        description = Description
    }
}) ->
    genlib_map:compact(#{
        <<"name">> => Name,
        <<"categoryID">> => CategoryID,
        <<"description">> => Description
    }).

decode_category_ref(#domain_CategoryRef{
    id = CategoryRef
}) ->
    CategoryRef.

decode_shop_account(undefined) ->
    undefined;

decode_shop_account(#domain_ShopAccount{
    currency = Currency,
    settlement = SettlementID,
    guarantee = GuaranteeID
}) ->
    #{
        <<"guaranteeID">> => GuaranteeID,
        <<"settlementID">> => SettlementID,
        <<"currency">> => decode_currency(Currency)
    }.

decode_account_state(#payproc_AccountState{
    account_id = AccountID,
    own_amount = OwnAmount,
    available_amount = AvailableAmount,
    currency = Currency
}) ->
    #{
        <<"id">> => AccountID,
        <<"ownAmount">> => OwnAmount,
        <<"availableAmount">> => AvailableAmount,
        <<"currency">> => decode_currency(Currency)
    }.

decode_payment_terms(#domain_PaymentsServiceTerms{
    currencies = Currencies,
    categories = Categories
}) ->
    genlib_map:compact(#{
        <<"currencies">> => decode_payment_terms(fun decode_currency/1, Currencies),
        <<"categories">> => decode_payment_terms(fun decode_category_ref/1, Categories)
    });
decode_payment_terms(undefined) ->
    #{}.

decode_payment_terms(Fun, {value, Val}) when is_list(Val) ->
    [Fun(V) || V <- Val];
decode_payment_terms(_, _) ->
    undefined.

decode_user_interaction({payment_terminal_reciept, #'PaymentTerminalReceipt'{
    short_payment_id = SPID,
    due = DueDate
}}) ->
    #{
        <<"interactionType">> => <<"PaymentTerminalReceipt">>,
        <<"shortPaymentID">> => SPID,
        <<"dueDate">> => DueDate
    };

decode_user_interaction({redirect, BrowserRequest}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => decode_browser_request(BrowserRequest)
    }.

decode_browser_request({get_request, #'BrowserGetRequest'{
    uri = UriTemplate
}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => UriTemplate
    };

decode_browser_request({post_request, #'BrowserPostRequest'{
    uri = UriTemplate,
    form = UserInteractionForm
}}) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => UriTemplate,
        <<"form">> => decode_user_interaction_form(UserInteractionForm)
    }.

decode_user_interaction_form(Form) ->
    maps:fold(
        fun(K, V, Acc) ->
            F = #{
                <<"key">> => K,
                <<"template">> => V
            },
            [F | Acc]
        end,
        [],
        Form
    ).

decode_geo_location_info(#geo_ip_LocationInfo{
    city_geo_id = CityID,
    country_geo_id = CountryID
}) ->
    #{
        <<"cityGeoID">> => CityID,
        <<"countryGeoID">> => CountryID
    };

decode_geo_location_info(undefined) ->
    undefined.

decode_location_name(GeoID, Name) ->
    #{
        <<"geoID">> => GeoID,
        <<"name">> => Name
    }.

decode_currency(#domain_Currency{
    symbolic_code = SymbolicCode
}) ->
    SymbolicCode;

decode_currency(#domain_CurrencyRef{
    symbolic_code = SymbolicCode
}) ->
    SymbolicCode.

encode_optional_currency(SymbolicCode) when is_binary(SymbolicCode) ->
    encode_currency(SymbolicCode);
encode_optional_currency(undefined) ->
    undefined.

encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{
        symbolic_code = SymbolicCode
    }.

decode_report(#reports_Report{
    report_id = ReportID,
    time_range = #reports_ReportTimeRange{
        from_time = FromTime,
        to_time = ToTime
    },
    created_at = CreatedAt,
    report_type = Type,
    files = Files
}) ->
    #{
        <<"id">> => ReportID,
        <<"createdAt">> => CreatedAt,
        <<"fromTime">> => FromTime,
        <<"toTime">> => ToTime,
        <<"type">> => decode_report_type(Type),
        <<"files">> => [decode_report_file(F) || F <- Files]
    }.

decode_report_type(<<"provision_of_service">>) -> <<"provisionOfService">>;
decode_report_type(<<"payment_registry">>) -> <<"paymentRegistry">>.

decode_report_file(#reports_FileMeta{
    file_id = ID,
    filename = Filename,
    signature = #reports_Signature{md5 = MD5, sha256 = SHA256}
}) ->
    #{
        <<"id">> => ID,
        <<"filename">> => Filename,
        <<"signatures">> => #{<<"md5">> => MD5, <<"sha256">> => SHA256}
    }.

decode_customer_event(#payproc_Event{
    id = EventID,
    created_at = CreatedAt,
    source =  {customer_id, _},
    payload =  {customer_changes, CustomerChanges}
}, _) ->
    Changes = decode_customer_changes(CustomerChanges),
    case Changes of
        [_Something | _] ->
            {true, #{
                <<"id">> => EventID,
                <<"createdAt">> => CreatedAt,
                <<"changes">> => Changes
            }};
        [] ->
            false
    end.

decode_customer_changes(CustomerChanges) when is_list(CustomerChanges) ->
    lists:filtermap(
        fun decode_customer_change/1,
        CustomerChanges
    ).

decode_customer_change({customer_binding_changed, #payproc_CustomerBindingChanged{
    id = BindingID,
    payload = Payload
}}) ->
    decode_customer_binding_change(BindingID, Payload);
decode_customer_change(_) ->
    false.

decode_customer_binding_change(_, {started, #payproc_CustomerBindingStarted{
    binding = CustomerBinding
}}) ->
    {true, #{
        <<"changeType">> => <<"CustomerBindingStarted">>,
        <<"customerBinding">> => decode_customer_binding(CustomerBinding)
    }};
decode_customer_binding_change(BindingID, {status_changed, #payproc_CustomerBindingStatusChanged{
    status = Status
}}) ->
    {true, genlib_map:compact(maps:merge(
        #{
            <<"changeType">> => <<"CustomerBindingStatusChanged">>,
            <<"customerBindingID">> => BindingID
        },
        decode_customer_binding_status(Status)
    ))};
decode_customer_binding_change(BindingID, {interaction_requested, #payproc_CustomerBindingInteractionRequested{
    interaction = UserInteraction
}}) ->
    {true, #{
        <<"changeType">> => <<"CustomerBindingInteractionRequested">>,
        <<"customerBindingID">> => BindingID,
        <<"userInteraction">> => decode_user_interaction(UserInteraction)
    }}.

decode_customer(#payproc_Customer{
    id = ID,
    shop_id = ShopID,
    status = Status,
    contact_info = ContactInfo,
    metadata = Metadata
}) ->
    #{
        <<"id">> => ID,
        <<"shopID">> => ShopID,
        <<"status">> => decode_customer_status(Status),
        <<"contactInfo">> => decode_contact_info(ContactInfo),
        <<"metadata">> => decode_customer_metadata(Metadata)
    }.

decode_customer_status({Status, _}) ->
    atom_to_binary(Status, utf8).

decode_customer_metadata(Meta) ->
    capi_json_marshalling:unmarshal(Meta).

decode_customer_binding(#payproc_CustomerBinding{
    id = ID,
    payment_resource = DisposablePaymentResource,
    status = Status
}) ->
    genlib_map:compact(maps:merge(
        #{
            <<"id">> => ID,
            <<"paymentResource">> => decode_disposable_payment_resource(DisposablePaymentResource)
        },
        decode_customer_binding_status(Status)
    )).

decode_disposable_payment_resource(#domain_DisposablePaymentResource{
    payment_tool = PaymentTool,
    payment_session_id = PaymentSessionID,
    client_info = ClientInfo0
}) ->
    ClientInfo = decode_client_info(ClientInfo0),
    #{
        <<"paymentToolToken">> => decode_payment_tool_token(PaymentTool),
        <<"paymentSession">> => wrap_payment_session(ClientInfo, PaymentSessionID),
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"clientInfo">> => ClientInfo
    }.

decode_customer_binding_status({Status, StatusInfo}) ->
    Error = case StatusInfo of
        #payproc_CustomerBindingFailed{failure = OperationFailure} ->
            decode_operation_failure(OperationFailure);
        _ ->
            undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

encode_optional_payout_method('BankAccount') ->
    #domain_PayoutMethodRef{
        id = russian_bank_account
    };
encode_optional_payout_method('InternationalBankAccount') ->
    #domain_PayoutMethodRef{
        id = international_bank_account
    };
encode_optional_payout_method(undefined) ->
    undefined.

decode_payout_method(#domain_PayoutMethodRef{id = russian_bank_account}) ->
    <<"BankAccount">>;
decode_payout_method(#domain_PayoutMethodRef{id = international_bank_account}) ->
    <<"InternationalBankAccount">>.

decode_payout_methods_selector({value, Val}) when is_list(Val) ->
    lists:map(fun decode_payout_method/1, Val);
decode_payout_methods_selector(_) ->
    [].

decode_business_schedules_selector({value, Val}) when is_list(Val) ->
    lists:map(fun decode_business_schedule_ref/1, Val);
decode_business_schedules_selector(_) ->
    [].

decode_business_schedule(#domain_BusinessScheduleObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"scheduleID" >> => Ref#domain_BusinessScheduleRef.id,
        <<"name"       >> => Data#domain_BusinessSchedule.name,
        <<"description">> => Data#domain_BusinessSchedule.description
    }).

encode_schedule_ref(ID) when ID /= undefined ->
    #domain_BusinessScheduleRef{id = ID};
encode_schedule_ref(undefined) ->
    undefined.

decode_business_schedule_ref(#domain_BusinessScheduleRef{id = ID}) when ID /= undefined ->
    ID;
decode_business_schedule_ref(undefined) ->
    undefined.

-define(invpaid()      , {paid, #webhooker_InvoicePaid{}}).
-define(invcancelled() , {cancelled, #webhooker_InvoiceCancelled{}}).
-define(invfulfilled() , {fulfilled, #webhooker_InvoiceFulfilled{}}).

-define(pmtprocessed() , {processed, #webhooker_InvoicePaymentProcessed{}}).
-define(pmtcaptured()  , {captured, #webhooker_InvoicePaymentCaptured{}}).
-define(pmtcancelled() , {cancelled, #webhooker_InvoicePaymentCancelled{}}).
-define(pmtrefunded()  , {refunded, #webhooker_InvoicePaymentRefunded{}}).
-define(pmtfailed()    , {failed, #webhooker_InvoicePaymentFailed{}}).

encode_invoice_event_type(<<"InvoiceCreated">>) ->
    {created, #webhooker_InvoiceCreated{}};
encode_invoice_event_type(<<"InvoicePaid">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invpaid()}};
encode_invoice_event_type(<<"InvoiceCancelled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invcancelled()}};
encode_invoice_event_type(<<"InvoiceFulfilled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invfulfilled()}};
encode_invoice_event_type(<<"PaymentStarted">>) ->
    {payment, {created, #webhooker_InvoicePaymentCreated{}}};
encode_invoice_event_type(<<"PaymentProcessed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtprocessed()}}};
encode_invoice_event_type(<<"PaymentCaptured">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcaptured()}}};
encode_invoice_event_type(<<"PaymentCancelled">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcancelled()}}};
encode_invoice_event_type(<<"PaymentRefunded">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtrefunded()}}};
encode_invoice_event_type(<<"PaymentFailed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtfailed()}}}.

encode_customer_event_type(<<"CustomerCreated">>) ->
    {created, #webhooker_CustomerCreated{}};
encode_customer_event_type(<<"CustomerDeleted">>) ->
    {deleted, #webhooker_CustomerDeleted{}};
encode_customer_event_type(<<"CustomerReady">>) ->
    {ready, #webhooker_CustomerStatusReady{}};
encode_customer_event_type(<<"CustomerBindingStarted">>) ->
    {binding,
        {started, #webhooker_CustomerBindingStarted{}}
    };
encode_customer_event_type(<<"CustomerBindingSucceeded">>) ->
    {binding,
        {succeeded, #webhooker_CustomerBindingSucceeded{}}
    };
encode_customer_event_type(<<"CustomerBindingFailed">>) ->
    {binding,
        {failed, #webhooker_CustomerBindingFailed{}}
    }.

decode_event_filter({invoice, #webhooker_InvoiceEventFilter{
    shop_id = ShopID,
    types   = EventTypes
}}) ->
    genlib_map:compact(#{
        <<"topic">>      => <<"InvoicesTopic">>,
        <<"shopID">>     => ShopID,
        <<"eventTypes">> => lists:flatmap(
            fun (V) -> decode_invoice_event_type(V) end, ordsets:to_list(EventTypes)
        )
    });
decode_event_filter({customer, #webhooker_CustomerEventFilter{
    shop_id = ShopID,
    types   = EventTypes
}}) ->
    genlib_map:compact(#{
        <<"topic">>      => <<"CustomersTopic">>,
        <<"shopID">>     => ShopID,
        <<"eventTypes">> => lists:map(
            fun decode_customer_event_type/1,
            ordsets:to_list(EventTypes)
        )
    }).

decode_invoice_event_type(
    {created, #webhooker_InvoiceCreated{}}
) ->
    [<<"InvoiceCreated">>];
decode_invoice_event_type(
    {status_changed, #webhooker_InvoiceStatusChanged{value = undefined}}
) ->
    % TODO seems unmaintainable
    [decode_invoice_status_event_type(V) || V <- [
        ?invpaid(),
        ?invcancelled(),
        ?invfulfilled()
    ]];
decode_invoice_event_type(
    {status_changed, #webhooker_InvoiceStatusChanged{value = Value}}
) ->
    [decode_invoice_status_event_type(Value)];
decode_invoice_event_type(
    {payment, {created, #webhooker_InvoicePaymentCreated{}}}
) ->
    [<<"PaymentStarted">>];
decode_invoice_event_type(
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = undefined}}}
) ->
    % TODO seems unmaintainable
    [decode_payment_status_event_type(V) || V <- [
        ?pmtprocessed(),
        ?pmtcaptured(),
        ?pmtcancelled(),
        ?pmtrefunded(),
        ?pmtfailed()
    ]];
decode_invoice_event_type(
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = Value}}}
) ->
    [decode_payment_status_event_type(Value)].

decode_invoice_status_event_type(?invpaid())      -> <<"InvoicePaid">>;
decode_invoice_status_event_type(?invcancelled()) -> <<"InvoiceCancelled">>;
decode_invoice_status_event_type(?invfulfilled()) -> <<"InvoiceFulfilled">>.

decode_payment_status_event_type(?pmtprocessed()) -> <<"PaymentProcessed">>;
decode_payment_status_event_type(?pmtcaptured())  -> <<"PaymentCaptured">>;
decode_payment_status_event_type(?pmtcancelled()) -> <<"PaymentCancelled">>;
decode_payment_status_event_type(?pmtrefunded())  -> <<"PaymentRefunded">>;
decode_payment_status_event_type(?pmtfailed())    -> <<"PaymentFailed">>.

decode_customer_event_type({created, #webhooker_CustomerCreated{}}) ->
    <<"CustomerCreated">>;
decode_customer_event_type({deleted, #webhooker_CustomerDeleted{}}) ->
    <<"CustomerDeleted">>;
decode_customer_event_type({ready, #webhooker_CustomerStatusReady{}}) ->
    <<"CustomerReady">>;
decode_customer_event_type({binding, {started, #webhooker_CustomerBindingStarted{}}}) ->
    <<"CustomerBindingStarted">>;
decode_customer_event_type({binding, {succeeded, #webhooker_CustomerBindingSucceeded{}}}) ->
    <<"CustomerBindingSucceeded">>;
decode_customer_event_type({binding, {failed, #webhooker_CustomerBindingFailed{}}}) ->
    <<"CustomerBindingFailed">>.

decode_webhook(#webhooker_Webhook{
    id           = ID,
    party_id     = _PartyID,
    event_filter = EventFilter,
    url          = URL,
    pub_key      = PubKey,
    enabled      = Enabled
}) ->
    {ok, WebhookID} = decode_webhook_id(ID),
    #{
        <<"id">>        => WebhookID,
        <<"active">>    => Enabled,
        <<"scope">>     => decode_event_filter(EventFilter),
        <<"url">>       => URL,
        <<"publicKey">> => PubKey
    }.

encode_stat_request(Dsl) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl));

encode_stat_request(Dsl) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl
    }.

create_stat_dsl(StatType, Req, Context) ->
    FromTime = get_time('fromTime', Req),
    ToTime  = get_time('toTime', Req),
    SplitInterval = case StatType of
        customers_rate_stat ->
            get_time_diff(FromTime, ToTime);
        _ ->
            SplitUnit = genlib_map:get('splitUnit', Req),
            SplitSize = genlib_map:get('splitSize', Req),
            get_split_interval(SplitSize, SplitUnit)
    end,

    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"from_time">> => FromTime,
        <<"to_time">> => ToTime,
        <<"split_interval">> => SplitInterval
    },
    create_dsl(StatType, Query, #{}).

process_merchant_stat(StatType, Req, Context, ReqCtx) ->
    Dsl = create_stat_dsl(StatType, Req, Context),
    Result = service_call(
        merchant_stat,
        'GetStatistics',
        [encode_stat_request(Dsl)],
        ReqCtx
    ),

    process_merchant_stat_result(StatType, Result).

process_merchant_stat_result(
    customers_rate_stat = StatType,
    {ok, #merchstat_StatResponse{data = {records, Stats}}}
) ->
    Resp = case Stats of
        [] ->
            #{
                <<"uniqueCount">> => 0
            };
        [StatResponse] ->
            decode_stat_info(StatType, StatResponse)
    end,
    {ok, {200, #{}, Resp}};

process_merchant_stat_result(StatType, Result) ->
    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_info(StatType, S) || S <- Stats],
            {ok, {200, #{}, Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
        {exception, #merchstat_BadToken{}} ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Invalid token">>)}}
    end.

process_search_request(QueryType, Query, Req, ReqCtx, Opts = #{thrift_fun := ThriftFun}) ->
    Limit = genlib_map:get('limit', Req),
    Offset = genlib_map:get('offset', Req),
    QueryParams = #{
        <<"size">> => Limit,
        <<"from">> => Offset
    },
    Dsl = create_dsl(QueryType, Query, QueryParams),
    Result = service_call(
        merchant_stat,
        ThriftFun,
        [encode_stat_request(Dsl)],
        ReqCtx
    ),
    process_search_request_result(QueryType, Result, Opts).

process_search_request_result(QueryType, Result, #{decode_fun := DecodeFun}) ->
    case Result of
        {ok, #merchstat_StatResponse{data = {QueryType, Data}, total_count = TotalCount}} ->
            DecodedData = [DecodeFun(D) || D <- Data],
            Resp = genlib_map:compact(#{
                <<"result">> => DecodedData,
                <<"totalCount">> => TotalCount
            }),
            {ok, {200, #{}, Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, #{}, logic_error(invalidRequest, format_request_errors(Errors))}};
        {exception, #merchstat_BadToken{}} ->
            {ok, {400, #{}, logic_error(invalidRequest, <<"Invalid token">>)}}
    end.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            capi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

get_split_interval(SplitSize, minute) ->
    SplitSize * 60;

get_split_interval(SplitSize, hour) ->
    SplitSize * 60 * 60;

get_split_interval(SplitSize, day) ->
    SplitSize * 60 * 60 * 24;

get_split_interval(SplitSize, week) ->
    SplitSize * 60 * 60 * 24 * 7;

get_split_interval(SplitSize, month) ->
    SplitSize * 60 * 60 * 24 * 30;

get_split_interval(SplitSize, year) ->
    SplitSize * 60 * 60 * 24 * 365.

get_time_diff(From, To) ->
    {DateFrom, TimeFrom} = parse_rfc3339_datetime(From),
    {DateTo, TimeTo} = parse_rfc3339_datetime(To),
    UnixFrom = genlib_time:daytime_to_unixtime({DateFrom, TimeFrom}),
    UnixTo = genlib_time:daytime_to_unixtime({DateTo, TimeTo}),
    UnixTo - UnixFrom.

parse_rfc3339_datetime(DateTime) ->
    {ok, {DateFrom, TimeFrom, _, _}} = rfc3339:parse(DateTime),
    {DateFrom, TimeFrom}.

format_request_errors([]) ->
    <<>>;

format_request_errors(Errors) ->
    genlib_string:join(<<"\n">>, Errors).

process_woody_error(_Source, result_unexpected, _Details) ->
    {error, reply_5xx(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, reply_5xx(503)};
process_woody_error(_Source, result_unknown, _Details) ->
    {error, reply_5xx(504)}.

prepare_party(Context, ReqCtx, ServiceCall) ->
    Result0 = ServiceCall(),
    case Result0 of
        {exception, #payproc_PartyNotFound{}} ->
            _ = logger:info("Attempting to create a missing party"),
            Result1 = create_party(Context, ReqCtx),
            case Result1 of
                ok ->
                    ServiceCall();
                Error ->
                    Error
            end;
        _ ->
            Result0
    end.

create_party(Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    UserInfo = get_user_info(Context),
    PartyParams = get_party_params(Context),
    Result = service_call(
        party_management,
        'Create',
        [UserInfo, PartyID, PartyParams],
        ReqCtx
    ),
    case Result of
        {ok, _} ->
            ok;
        {exception, #payproc_PartyExists{}} ->
            ok;
        Error ->
            Error
    end.

get_invoice_by_id(ReqCtx, UserInfo, InvoiceID) ->
    service_call(
        invoicing,
        'Get',
        [UserInfo, InvoiceID],
        ReqCtx
    ).

get_customer_by_id(ReqCtx, CustomerID) ->
    service_call(
        customer_management,
        'Get',
        [CustomerID],
        ReqCtx
    ).

get_invoice_tpl(InvoiceTplID, UserInfo, ReqCtx, Context) ->
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            get_invoice_tpl_by_id(ReqCtx, UserInfo, InvoiceTplID)
        end
    ),
    case Result of
        {ok, InvoiceTpl} ->
            InvoiceTpl;
        {exception, Exception} ->
            throw(Exception)
    end.

get_invoice_tpl_by_id(ReqCtx, UserInfo, InvoiceTplID) ->
    service_call(
        invoice_templating,
        'Get',
        [UserInfo, InvoiceTplID],
        ReqCtx
    ).

get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID) ->
    service_call(
        invoicing,
        'GetPayment',
        [UserInfo, InvoiceID, PaymentID],
        ReqCtx
    ).

get_my_party(Context, ReqCtx, UserInfo, PartyID) ->
    prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'Get',
                [UserInfo, PartyID],
                ReqCtx
            )
        end
    ).

delete_webhook(PartyID, WebhookID, ReqCtx) ->
    case get_webhook(PartyID, WebhookID, ReqCtx) of
        {ok, #webhooker_Webhook{}} ->
            service_call(webhook_manager, 'Delete', [WebhookID], ReqCtx);
        Exception ->
            Exception
    end.

get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID) ->
    prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetContract',
                [UserInfo, PartyID, ContractID],
                ReqCtx
            )
        end
    ).

get_category_by_id(CategoryID, ReqCtx) ->
    CategoryRef = {category, #domain_CategoryRef{id = CategoryID}},
    capi_domain:get(CategoryRef, ReqCtx).

get_schedule_by_id(ScheduleID, ReqCtx) ->
    Ref = {business_schedule, #domain_BusinessScheduleRef{id = ScheduleID}},
    capi_domain:get(Ref, ReqCtx).

collect_events(Limit, After, GetterFun, DecodeFun, DecodingContext) ->
    collect_events([], Limit, After, GetterFun, DecodeFun, DecodingContext).

collect_events(Collected, 0, _, _, _, _) ->
    {ok, Collected};

collect_events(Collected0, Left, After, GetterFun, DecodeFun, DecodingContext) when Left > 0 ->
    case get_events(Left, After, GetterFun) of
        {ok, Events} ->
            Filtered = decode_and_filter_events(DecodeFun, DecodingContext, Events),
            Collected = Collected0 ++ Filtered,
            case length(Events) of
                Left ->
                    collect_events(
                        Collected,
                        Left - length(Filtered),
                        get_last_event_id(Events),
                        GetterFun,
                        DecodeFun,
                        DecodingContext
                    );
                N when N < Left ->
                    {ok, Collected}
            end;
        Error ->
            Error
    end.

decode_and_filter_events(DecodeFun, DecodingContext, Events) ->
    lists:foldr(
        fun(Event, Acc) ->
             case DecodeFun(Event, DecodingContext) of
                {true, Ev} ->
                    [Ev|Acc];
                false ->
                    Acc
            end
        end,
        [],
        Events
    ).

get_last_event_id(Events) ->
    #payproc_Event{
        id = ID
    } = lists:last(Events),
    ID.

get_events(Limit, After, GetterFun) ->
    EventRange = #'payproc_EventRange'{
        limit = Limit,
        'after' = After
    },
    GetterFun(EventRange).

construct_payment_methods(ServiceName, Args, Context) ->
    case compute_terms(ServiceName, Args, Context) of
        {ok, #domain_TermSet{payments = undefined}} ->
            {ok, []};
        {ok, #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = PaymentMethodRefs
            }
        }} ->
            {ok, decode_payment_methods(PaymentMethodRefs)};
        Error ->
            Error
    end.

decode_payment_methods(undefined) ->
    [];
decode_payment_methods({value, PaymentMethodRefs}) ->
    PaymentMethods = [ID || #domain_PaymentMethodRef{id = ID} <- PaymentMethodRefs],
    lists:foldl(
        fun(Method, Acc) ->
            {_, MethodTerms} = lists:unzip(proplists:lookup_all(Method, PaymentMethods)),
            decode_payment_method(Method, MethodTerms) ++ Acc
        end,
        [],
        proplists:get_keys(PaymentMethods)
    ).

decode_payment_method(empty_cvv_bank_card, PaymentSystems) ->
    [#{<<"method">> => <<"BankCard">>, <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems)}];
decode_payment_method(bank_card, PaymentSystems) ->
    [#{<<"method">> => <<"BankCard">>, <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems)}];
decode_payment_method(payment_terminal, Providers) ->
    [#{<<"method">> => <<"PaymentTerminal">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}];
decode_payment_method(digital_wallet, Providers) ->
    [#{<<"method">> => <<"DigitalWallet">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}];
decode_payment_method(tokenized_bank_card, TokenizedBankCards) ->
   decode_tokenized_bank_cards(TokenizedBankCards).

decode_tokenized_bank_cards(TokenizedBankCards) ->
    PropTokenizedBankCards = [
        {TP, PS} || #domain_TokenizedBankCard{payment_system = PS, token_provider = TP} <- TokenizedBankCards
    ],
    lists:map(
        fun(TokenProvider) ->
            {_, PaymentSystems} = lists:unzip(proplists:lookup_all(TokenProvider, PropTokenizedBankCards)),
            decode_tokenized_bank_card(TokenProvider, PaymentSystems)
        end,
        proplists:get_keys(PropTokenizedBankCards)
    ).

 decode_tokenized_bank_card(TokenProvider, PaymentSystems) ->
   #{
        <<"method">> => <<"BankCard">>,
        <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems),
        <<"tokenProviders">> => [genlib:to_binary(TokenProvider)]
    }.

compute_terms(ServiceName, Args, Context) ->
    service_call(
        ServiceName,
        'ComputeTerms',
        Args,
        Context
    ).

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, #{}, <<>>}.

-spec convert_crypto_currency_from_swag(binary()) -> atom().

convert_crypto_currency_from_swag(<<"bitcoinCash">>) ->
    bitcoin_cash;
convert_crypto_currency_from_swag(CryptoCurrency) when is_binary(CryptoCurrency) ->
    binary_to_existing_atom(CryptoCurrency, utf8).

-spec convert_crypto_currency_to_swag(atom()) -> binary().

convert_crypto_currency_to_swag(bitcoin_cash) ->
    <<"bitcoinCash">>;
convert_crypto_currency_to_swag(CryptoCurrency) when is_atom(CryptoCurrency) ->
    atom_to_binary(CryptoCurrency, utf8).

wrap_payment_session(ClientInfo, PaymentSession) ->
    capi_utils:map_to_base64url(#{
        <<"clientInfo">> => ClientInfo,
        <<"paymentSession">> => PaymentSession
    }).

unwrap_payment_session(Encoded) ->
    #{
        <<"clientInfo">> := ClientInfo,
        <<"paymentSession">> := PaymentSession
     } = try capi_utils:base64url_to_map(Encoded)
    catch
        error:badarg ->
            erlang:throw(invalid_payment_session)
    end,
    {ClientInfo, PaymentSession}.

get_default_url_lifetime() ->
    Now = erlang:system_time(second),
    Lifetime = application:get_env(capi, reporter_url_lifetime, ?DEFAULT_URL_LIFETIME),
    {ok, Val} = rfc3339:format(Now + Lifetime, second),
    Val.

compute_payment_institution_terms(PaymentInstitutionID, VS, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'ComputePaymentInstitutionTerms',
                [UserInfo, PartyID, ?payment_institution_ref(PaymentInstitutionID), VS],
                ReqCtx
            )
        end
    ).

prepare_varset(Req) ->
    Currency = encode_optional_currency(genlib_map:get(currency, Req)),
    PayoutMethod = encode_optional_payout_method(genlib_map:get(payoutMethod, Req)),
    #payproc_Varset{
        currency = Currency,
        payout_method = PayoutMethod
    }.

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

decode_optional(Arg, DecodeFun) when Arg /= undefined ->
    DecodeFun(Arg);
decode_optional(undefined, _) ->
    undefined.

create_sequence_id([Identifier | Rest], BenderPrefix) ->
    Next = create_sequence_id(Rest, BenderPrefix),
    <<Identifier/binary, ".", Next/binary>>;
create_sequence_id([], BenderPrefix) ->
    genlib:to_binary(BenderPrefix).
