-module(capi_real_handler).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").
-include_lib("dmsl/include/dmsl_webhooker_thrift.hrl").
-include_lib("dmsl/include/dmsl_user_interaction_thrift.hrl").
-include_lib("dmsl/include/dmsl_geo_ip_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").

-behaviour(swag_server_logic_handler).

%% API callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-define(DEFAULT_INVOICE_META, #{}).
-define(DEFAULT_INVOICE_LINE_META, #{}).
-define(DEFAULT_URL_LIFETIME, 60). % seconds

-define(payment_institution_ref(PaymentInstitutionID),
    #domain_PaymentInstitutionRef{id = PaymentInstitutionID}).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key()) ->
    Result :: false | {true, capi_auth:context()}.

authorize_api_key(OperationID, ApiKey) ->
    _ = capi_utils:logtag_process(operation_id, OperationID),
    capi_auth:authorize_api_key(OperationID, ApiKey).

-type request_data() :: #{atom() | binary() => term()}.

-spec handle_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: swag_server:request_context()
) ->
    {ok | error, swag_server_logic_handler:response()}.

handle_request(OperationID, Req, Context) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    try
        case capi_auth:authorize_operation(OperationID, Req, get_auth_context(Context)) of
            ok ->
                ReqContext = create_context(Req, get_auth_context(Context)),
                process_request(OperationID, Req, Context, ReqContext);
            {error, _} = Error ->
                _ = lager:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {error, {401, [], general_error(<<"Unauthorized operation">>)}}
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
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('CreateInvoice', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    try
        Params = encode_invoice_params(PartyID, maps:get('InvoiceParams', Req)),
        UserInfo = get_user_info(Context),
        prepare_party(
           Context,
           ReqCtx,
           fun () ->
               service_call(
                   invoicing,
                   'Create',
                   [UserInfo, Params],
                   ReqCtx
               )
           end
        )
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], make_invoice_and_token(Invoice, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    catch
        invoice_cart_empty ->
            {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        invalid_invoice_cost ->
            {ok, {400, [], logic_error(invalidInvoiceCost, <<"Invalid invoice amount">>)}}
    end;

process_request('CreatePayment', Req, Context, ReqCtx) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
    Payer = genlib_map:get(<<"payer">>, PaymentParams),
    UserInfo = get_user_info(Context),
    Result = try
        Params =  #payproc_InvoicePaymentParams{
            'payer' = encode_payer_params(Payer),
            'flow' = encode_flow(Flow)
        },
        service_call(
            invoicing,
            'StartPayment',
            [UserInfo, InvoiceID, Params],
            ReqCtx
        )
    catch
        throw:Error when Error =:= invalid_token orelse Error =:= invalid_payment_session ->
            {error, Error}
    end,

    case Result of
        {ok, Payment} ->
            {ok, {201, [], decode_invoice_payment(InvoiceID, Payment)}};
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

process_request('CreatePaymentResource', Req, Context, ReqCtx) ->
    Params = maps:get('PaymentResourceParams', Req),
    ClientInfo = enrich_client_info(maps:get(<<"clientInfo">>, Params), Context),
    try
        V = maps:get(<<"paymentTool">>, Params),
        {PaymentTool, PaymentSessionID} = case V of
            #{<<"paymentToolType">> := <<"CardData">>} ->
                process_card_data(V, ReqCtx);
            #{<<"paymentToolType">> := <<"PaymentTerminalData">>} ->
                process_payment_terminal_data(V, ReqCtx);
            #{<<"paymentToolType">> := <<"DigitalWalletData">>} ->
                process_digital_wallet_data(V, ReqCtx)
        end,
        {ok, {201, [], decode_disposable_payment_resource(#domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSessionID,
            client_info = encode_client_info(ClientInfo)
        })}}
    catch
        Result ->
            Result
    end;

process_request('CreateInvoiceAccessToken', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{}} ->
            Token = make_invoice_access_token(InvoiceID, PartyID, Context),
            {ok, {201, [], Token}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetInvoiceByID', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            Resp = decode_invoice(Invoice),
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvoicePaymentPending{} ->
                    {ok, {400, [], logic_error(invoicePaymentPending, <<"Invoice payment pending">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
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
    Result  = collect_events(
        maps:get(limit, Req),
        genlib_map:get(eventID, Req),
        GetterFun,
        fun decode_invoice_event/1
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, [], Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [],  general_error(<<"Invoice not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, [], general_error(<<"Event not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
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
            {ok, {200, [], PaymentMethods}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [],  general_error(<<"Invoice not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetPayments', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{payments = Payments}} ->
            Resp = [decode_invoice_payment(InvoiceID, P) || P <- Payments],
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
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
            {ok, {200, [], Resp}};
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

process_request('CapturePayment', Req, Context, ReqCtx) ->
    PaymentID = maps:get(paymentID, Req),
    InvoiceID = maps:get(invoiceID, Req),
    Params = maps:get('Reason', Req),
    Reason = maps:get(<<"reason">>, Params),
    UserInfo = get_user_info(Context),

    Result = service_call(
        invoicing,
        'CapturePayment',
        [UserInfo, InvoiceID, PaymentID, Reason],
        ReqCtx
    ),
    case Result of
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
        <<"invoice_amount">> => genlib_map:get('invoiceAmount', Req)
    },
    Opts = #{
        thrift_fun => 'GetInvoices',
        decode_fun => fun decode_stat_invoice/1
    },
    process_search_request(invoices, Query, Req, ReqCtx, Opts);

process_request('SearchPayments', Req, Context, ReqCtx) ->
    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"invoice_id">> =>  genlib_map:get('invoiceID', Req),
        <<"from_time">> => get_time('fromTime', Req),
        <<"to_time">> => get_time('toTime', Req),
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
        <<"payment_amount">> => genlib_map:get('paymentAmount', Req)
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
            {ok, {200, [], PreparedLocationNames}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
    end;

process_request('CreateRefund', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    Params = #payproc_InvoicePaymentRefundParams{
        reason = genlib_map:get(reason, Req)
    },
    UserInfo = get_user_info(Context),
    Result = service_call(
        invoicing,
        'RefundPayment',
        [UserInfo, InvoiceID, PaymentID, Params],
        ReqCtx
    ),
    case Result of
        {ok, Refund} ->
            Resp = decode_refund(Refund),
            {ok, {201, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, {404, [], general_error(<<"Payment not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, [], logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, {400, [], logic_error(invalidInvoicePaymentStatus, <<"Invalid invoice payment status">>)}};
                #payproc_InvoicePaymentRefundPending{} ->
                    {ok, {400, [], logic_error(invoicePaymentRefundPending, <<"Invoice payment refund pending">>)}};
                #payproc_InsufficientAccountBalance{} ->
                    {ok, {400, [], logic_error(
                        insufficentAccountBalance,
                        <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                    )}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
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
            {ok, {200, [], Resp}};
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
            {ok, {200, [], Resp}};
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
            {ok, {201, [], make_invoice_tpl_and_token(InvoiceTpl, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    catch
        throw:invoice_cart_empty ->
            {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        throw:zero_invoice_lifetime ->
            {ok, {400, [], logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}}
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
            {ok, {200, [], decode_invoice_tpl(InvoiceTpl)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    end;

process_request('UpdateInvoiceTemplate', Req, Context, ReqCtx) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    UserInfo = get_user_info(Context),
    try
        Params = encode_invoice_tpl_update_params(maps:get('InvoiceTemplateUpdateParams', Req)),
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
            {ok, {200, [], decode_invoice_tpl(InvoiceTpl)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    catch
        throw:#payproc_InvalidUser{} ->
            {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
        throw:#payproc_InvoiceTemplateNotFound{} ->
            {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
        throw:#payproc_InvoiceTemplateRemoved{} ->
            {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
        throw:invoice_cart_empty ->
            {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        throw:zero_invoice_lifetime ->
            {ok, {400, [], logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}}
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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    end;

process_request('CreateInvoiceWithTemplate', Req, Context, ReqCtx) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    InvoiceParams = maps:get('InvoiceParamsWithTemplate', Req),
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    try
        Params = encode_invoice_params_with_tpl(InvoiceTplID, InvoiceParams),
        prepare_party(
            Context,
            ReqCtx,
            fun () ->
                service_call(
                    invoicing,
                    'CreateWithTemplate',
                    [UserInfo, Params],
                    ReqCtx
                   )
            end
        )
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], make_invoice_and_token(Invoice, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    catch
        throw:{bad_invoice_params, currency_no_amount} ->
            {ok, {400, [], logic_error(invalidRequest, <<"Amount is required for the currency">>)}};
        throw:{bad_invoice_params, amount_no_currency} ->
            {ok, {400, [], logic_error(invalidRequest, <<"Currency is required for the amount">>)}}
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
            {ok, {200, [], PaymentMethods}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_PartyNotExistsYet{} ->
                    {ok, {400, [], logic_error(partyNotExistsYet, <<"Party not exists yet">>)}}
            end
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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{
                    status = {suspension, {active, _}}
                } ->
                    {ok, {204, [], undefined}}
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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{
                    status = {suspension, {suspended, _}}
                } ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('GetShops', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    {ok, #domain_Party{shops = Shops}} = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    Resp = decode_shops_map(Shops),
    {ok, {200, [], Resp}};

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
            {ok, {200, [], Resp}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {404, [], general_error(<<"Shop not found">>)}}
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
    ReportTypes = [],
    Result = service_call(reporting, 'GetReports', [ReportRequest, ReportTypes], ReqCtx),
    case Result of
        {ok, Reports} ->
            Resp = [decode_report(R) || #reports_Report{status = created} = R <- Reports],
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, {400, [], limit_exceeded_error(Limit)}}
            end
    end;

process_request('DownloadFile', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    ReportID = maps:get(reportID, Req),
    FileID = maps:get(fileID, Req),
    Result = service_call(reporting, 'GetReport', [PartyID, ShopID, ReportID], ReqCtx),
    case Result of
        {ok, #reports_Report{status = created, files = Files}} ->
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, ReqCtx);
                false ->
                    {ok, {404, [], general_error(<<"File not found">>)}}
            end;
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], general_error(<<"Report not found">>)}}
    end;

process_request('GetContracts', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = get_my_party(Context, ReqCtx, UserInfo, PartyID),

    case Result of
        {ok, #domain_Party{
            contracts = Contracts
        }} ->
            Resp = decode_contracts_map(Contracts),
            {ok, {200, [], Resp}}
    end;

process_request('GetContractByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, Contract} ->
            Resp = decode_contract(Contract),
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_ContractNotFound{} ->
                    {ok, {404, [], general_error(<<"Contract not found">>)}}
            end
    end;

process_request('GetPayoutTools', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            Resp = [decode_payout_tool(P) || P <- PayoutTools],
            {ok, {200, [], Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
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
                    {ok, {200, [], decode_payout_tool(P)}};
                false ->
                    {ok, {404, [], general_error(<<"PayoutTool not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetContractAdjustments', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, [], Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
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
                    {ok, {200, [], decode_contract_adjustment(A)}};
                false ->
                    {ok, {404, [], general_error(<<"Adjustment not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetMyParty', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    {ok, Party} = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    Resp = decode_party(Party),
    {ok, {200, [], Resp}};

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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, [], undefined}}
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
            {ok, {204, [], undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, [], undefined}}
    end;

process_request('GetCategories', _Req, Context, ReqCtx) ->
    _ = get_user_info(Context),
    _ = get_party_id(Context),
    {ok, Categories} = capi_domain:get_categories(ReqCtx),
    Resp = [decode_category(C) || C <- Categories],
    {ok, {200, [], Resp}};

process_request('GetCategoryByRef', Req, Context0, ReqCtx) ->
    _ = get_user_info(Context0),
    _ = get_party_id(Context0),
    CategoryID = maps:get(categoryID, Req),
    case get_category_by_id(genlib:to_int(CategoryID), ReqCtx) of
        {ok, Category} ->
            Resp = decode_category(Category),
            {ok, {200, [], Resp}};
        {error, not_found} ->
            {404, [], general_error(<<"Category not found">>)}
    end;

process_request('GetPaymentInstitutions', Req, _Context, ReqCtx) ->
    Residence = encode_residence(genlib_map:get(residence, Req)),
    Realm = genlib_map:get(realm, Req),
    {ok, PaymentInstObjects} = capi_domain:get_payment_institutions(ReqCtx),
    Resp = lists:filtermap(
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
    {ok, {200, [], Resp}};

process_request('GetPaymentInstitutionByRef', Req, _Context, ReqCtx) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case capi_domain:get({payment_institution, ?payment_institution_ref(PaymentInstitutionID)}, ReqCtx) of
        {ok, PaymentInstitution} ->
            Resp = decode_payment_institution_obj(PaymentInstitution),
            {ok, {200, [], Resp}};
        {error, not_found} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPaymentTerms', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'ComputePaymentInstitutionTerms',
                [UserInfo, PartyID, ?payment_institution_ref(PaymentInstitutionID)],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, #domain_TermSet{payments = PaymentTerms}} ->
            Resp = decode_payment_terms(PaymentTerms),
            {ok, {200, [], Resp}};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
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
            {ok, {200, [], Resp}};
        {exception, #payproc_AccountNotFound{}} ->
            {ok, {404, [], general_error(<<"Account not found">>)}}
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
    {ok, {200, [], Resp}};

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
            Resp = decode_claim(Claim),
            {ok, {200, [], Resp}};
        {exception, #payproc_ClaimNotFound{}} ->
            {ok, {404, [], general_error(<<"Claim not found">>)}}
    end;

process_request('CreateClaim', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    try
        Changeset = encode_claim_changeset(maps:get('ClaimChangeset', Req)),
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
                {ok, {201, [], Resp}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidPartyStatus{} ->
                        {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                    #payproc_ChangesetConflict{} ->
                        {ok, {400, [], logic_error(changesetConflict, <<"Changeset conflict">>)}};
                    #payproc_InvalidChangeset{} ->
                        {ok, {400, [], logic_error(invalidChangeset, <<"Invalid changeset">>)}}
                end
        end
    catch
        throw:{encode_contract_modification, adjustment_creation_not_supported} ->
            {ok, {400, [], logic_error(invalidChangeset, <<"Contract adjustment creation not supported">>)}}
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
%     {ok, {200, [], Resp}};

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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_ClaimNotFound{} ->
                    {ok, {404, [], general_error(<<"Claim not found">>)}};
                #payproc_InvalidClaimStatus{} ->
                    {ok, {400, [], logic_error(invalidClaimStatus, <<"Invalid claim status">>)}};
                #payproc_InvalidClaimRevision{} ->
                    {ok, {400, [], logic_error(invalidClaimRevision, <<"Invalid claim revision">>)}}
            end
    end;

process_request('CreateWebhook', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    WebhookParams = encode_webhook_params(PartyID, maps:get('Webhook', Req)),
    case validate_webhook_params(WebhookParams, Context, ReqCtx) of
        {ok, _} ->
            {ok, Webhook} = service_call(webhook_manager, 'Create', [WebhookParams], ReqCtx),
            Resp = decode_webhook(Webhook),
            {ok, {201, [], Resp}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}}
    end;

process_request('GetWebhooks', _Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    {ok, Webhooks} = service_call(webhook_manager, 'GetList', [PartyID], ReqCtx),
    {ok, {200, [], [decode_webhook(V) || V <- Webhooks]}};

process_request('GetWebhookByID', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case get_webhook(PartyID, WebhookID, ReqCtx) of
                {ok, Webhook} ->
                    {ok, {200, [], decode_webhook(Webhook)}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {404, [], general_error(<<"Webhook not found">>)}}
            end;
        error ->
            {ok, {404, [], general_error(<<"Webhook not found">>)}}
    end;

process_request('DeleteWebhookByID', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case delete_webhook(PartyID, WebhookID, ReqCtx) of
                {ok, _} ->
                    {ok, {204, [], undefined}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {204, [], undefined}}
            end;
        error ->
            {ok, {404, [], general_error(<<"Webhook not found">>)}}
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
            {ok, {201, [], make_customer_and_token(Customer, PartyID, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, [], logic_error(operationNotPermitted, <<"Operation not permitted">>)}}
            end
    end;

process_request('GetCustomerById', Req, _Context, ReqCtx) ->
    CustomerID = maps:get('customerID', Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, Customer} ->
            {ok, {200, [], decode_customer(Customer)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
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
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CreateCustomerAccessToken', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    CustomerID = maps:get(customerID, Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, #payproc_Customer{}} ->
            Token = make_customer_access_token(CustomerID, PartyID, Context),
            {ok, {201, [], Token}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
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
            {ok, {201, [], decode_customer_binding(CustomerBinding)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidPaymentTool{} ->
                    {ok, {400, [], logic_error(invalidPaymentResource, <<"Invalid payment resource">>)}};
                #payproc_OperationNotPermitted{} ->
                    {ok, {400, [], logic_error(operationNotPermitted, <<"Operation not permitted">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
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

process_request('GetBindings', Req, _Context, ReqCtx) ->
    CustomerID = maps:get(customerID, Req),
    Result = get_customer_by_id(ReqCtx, CustomerID),
    case Result of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            {ok, {200, [], [decode_customer_binding(B) || B <- Bindings]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
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
                    {ok, {200, [], decode_customer_binding(B)}};
                false ->
                    {ok, {404, [], general_error(<<"Customer binding not found">>)}}
            end;
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
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
        fun decode_customer_event/1
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, [], Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, [], general_error(<<"Event not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end.

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
            {ok, {303, [{<<"Location">>, URL}], undefined}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_FileNotFound{}->
                    {ok, {404, [], general_error(<<"File not found">>)}}
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
    _ = lager:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
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

parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year = case genlib:to_int(Year0) of
        Y when Y < 100 ->
            2000 + Y;
        Y ->
            Y
    end,
    {genlib:to_int(Month), Year}.

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

get_peer_info(#{peer := Peer}) ->
    Peer.

encode_invoice_params(PartyID, InvoiceParams) ->
    Amount = genlib_map:get(<<"amount">>, InvoiceParams),
    Currency = genlib_map:get(<<"currency">>, InvoiceParams),
    Cart = genlib_map:get(<<"cart">>, InvoiceParams),
    #payproc_InvoiceParams{
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

encode_invoice_params_with_tpl(InvoiceTplID, InvoiceParams) ->
    #payproc_InvoiceWithTemplateParams{
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
    Metadata = encode_invoice_line_meta(Line),
    Price = encode_cash(genlib_map:get(<<"price">>, Line), Currency),
    #domain_InvoiceLine{
        product = genlib_map:get(<<"product">>, Line),
        quantity = genlib_map:get(<<"quantity">>, Line),
        price = Price,
        metadata = Metadata
    }.

encode_invoice_line_meta(Line) ->
    case genlib_map:get(<<"taxMode">>, Line) of
        TaxMode when TaxMode =/= undefined ->
            TM = encode_invoice_line_tax_mode(TaxMode),
            #{<<"TaxMode">> => {str, TM}};
        undefined ->
            ?DEFAULT_INVOICE_LINE_META
    end.

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
        #{<<"type">> := <<"bank_card">>} = Encoded ->
            encode_bank_card(Encoded);
        #{<<"type">> := <<"payment_terminal">>} = Encoded ->
            encode_payment_terminal(Encoded);
        #{<<"type">> := <<"digital_wallet">>} = Encoded ->
            encode_digital_wallet(Encoded)
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end.

decode_bank_card(#domain_BankCard{
    'token'  = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan
}) ->
    capi_utils:map_to_base64url(#{
        <<"type">> => <<"bank_card">>,
        <<"token">> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin">> => Bin,
        <<"masked_pan">> => MaskedPan
    }).

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
    Details = encode_invoice_tpl_details(genlib_map:get(<<"details">>, Params)),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateCreateParams{
        party_id         = PartyID,
        shop_id          = genlib_map:get(<<"shopID">>, Params),
        invoice_lifetime = encode_lifetime(Params),
        product          = Product,
        description      = genlib_map:get(<<"description">>, Params),
        details          = Details,
        context          = encode_invoice_context(Params)
    }.

encode_invoice_tpl_update_params(Params) ->
    Details = encode_invoice_tpl_details(genlib_map:get(<<"details">>, Params)),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = encode_lifetime(Params),
        product          = Product,
        description      = genlib_map:get(<<"description">>, Params),
        details          = Details,
        context          = encode_optional_context(Params)
    }.

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

encode_invoice_context(Params) ->
    encode_invoice_context(Params, ?DEFAULT_INVOICE_META).

encode_invoice_context(Params, DefaultMeta) ->
    Context = jsx:encode(genlib_map:get(<<"metadata">>, Params, DefaultMeta)),
    #'Content'{
        type = <<"application/json">>,
        data = Context
    }.

encode_invoice_tpl_line_cost(#{<<"costType">> := CostType} = Cost) ->
    encode_invoice_tpl_line_cost(CostType, Cost);
encode_invoice_tpl_line_cost(_) ->
    undefined.

encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostUnlim">>, _Cost) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}};
encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostFixed">>, Cost) ->
    {fixed, encode_cash(Cost)};
encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostRange">>, Cost) ->
    Range = genlib_map:get(<<"range">>, Cost),
    {range, #domain_CashRange{
        lower = {inclusive, encode_cash(Cost#{<<"amount">> => genlib_map:get(<<"lowerBound">>, Range)})},
        upper = {inclusive, encode_cash(Cost#{<<"amount">> => genlib_map:get(<<"upperBound">>, Range)})}
    }}.

encode_lifetime(#{<<"lifetime">> := Lifetime}) ->
    encode_lifetime(
        genlib_map:get(<<"days">>, Lifetime),
        genlib_map:get(<<"months">>, Lifetime),
        genlib_map:get(<<"years">>, Lifetime)
    );
encode_lifetime(_) ->
    undefined.

encode_lifetime(0, 0, 0) ->
    throw(zero_invoice_lifetime);
encode_lifetime(DD, MM, YY) ->
    #domain_LifetimeInterval{
        days   = DD,
        months = MM,
        years  = YY
      }.

encode_invoice_tpl_details(#{<<"templateType">> := <<"InvoiceTemplateSingleLine">>} = Details) ->
    {product, encode_invoice_tpl_product(Details)};
encode_invoice_tpl_details(#{<<"templateType">> := <<"InvoiceTemplateMultiLine">>} = Details) ->
    {cart, encode_invoice_cart(Details)};
encode_invoice_tpl_details(undefined) ->
    undefined.

encode_invoice_tpl_product(Details) ->
    #domain_InvoiceTemplateProduct{
        product = genlib_map:get(<<"product">>, Details),
        price = encode_invoice_tpl_line_cost(genlib_map:get(<<"price">>, Details)),
        metadata = encode_invoice_line_meta(Details)
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

get_product_from_tpl_details({product, #domain_InvoiceTemplateProduct{product = Product}}) ->
    Product;
get_product_from_tpl_details({cart, #domain_InvoiceCart{lines = [FirstLine | _]}}) ->
    #domain_InvoiceLine{product = Product} = FirstLine,
    Product;
get_product_from_tpl_details(undefined) ->
    undefined.

encode_claim_changeset(Changeset) when is_list(Changeset)->
    lists:map(fun encode_party_modification/1, Changeset).

encode_party_modification(#{<<"partyModificationType">> := Type} = Modification) ->
    case Type of
        <<"ContractModification">> ->
            {contract_modification, encode_contract_modification(Modification)};
        <<"ShopModification">> ->
            {shop_modification, encode_shop_modification(Modification)}
    end.

encode_contract_modification(#{<<"contractID">> := ContractID} = Modification) ->
    EncodedMod = case maps:get(<<"contractModificationType">>, Modification) of
        <<"ContractCreation">> ->
            {creation, #payproc_ContractParams{
                contractor = encode_contractor(maps:get(<<"contractor">>, Modification)),
                payment_institution = encode_payment_institution_ref(maps:get(<<"paymentInstitutionID">>, Modification))
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
                modification = {creation, encode_payout_tool_params(Modification)}
            }}
    end,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = EncodedMod
    }.

encode_shop_modification(#{<<"shopID">> := ShopID} = Modification) ->
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
            {payout_tool_modification, maps:get(<<"payoutToolID">>, Modification)}
    end,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = EncodedMod
    }.

encode_reason(undefined) ->
    undefined;
encode_reason(#{<<"reason">> := Reason}) ->
    Reason.

encode_legal_agreement(#{<<"id">> := ID, <<"signedAt">> := SignedAt}) ->
    #domain_LegalAgreement{
        signed_at = SignedAt,
        legal_agreement_id = ID
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

encode_international_bank_account(#{
    <<"accountHolder">> := AccountHolder,
    <<"bankName">> := BankName,
    <<"bankAddress">> := BankAddress,
    <<"iban">> := Iban,
    <<"bic">> := Bic
}) ->
    #domain_InternationalBankAccount{
        account_holder = AccountHolder,
        bank_name = BankName,
        bank_address = BankAddress,
        iban = Iban,
        bic = Bic
    }.

encode_contractor(#{<<"contractorType">> := <<"LegalEntity">>} = Contractor) ->
    {legal_entity, encode_legal_entity(Contractor)};

encode_contractor(#{<<"contractorType">> := <<"RegisteredUser">>} = Contractor) ->
    {registered_user, encode_registered_user(Contractor)}.

encode_legal_entity(#{
    <<"entityType">> := <<"RussianLegalEntity">>
} = Entity) ->
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
        actual_address = genlib_map:get(<<"principalPlaceOfBusiness">>, Entity)
    }}.

encode_registered_user(#{<<"email">> := Email}) ->
    #domain_RegisteredUser{email = Email}.

encode_payment_institution_ref(Ref) ->
    #domain_PaymentInstitutionRef{
        id = Ref
    }.

encode_residence(Residence) when is_binary(Residence) ->
    binary_to_existing_atom(Residence, utf8);
encode_residence(undefined) ->
    undefined.

encode_flow(#{<<"type">> := <<"PaymentFlowInstant">>}) ->
    {instant, #payproc_InvoicePaymentParamsFlowInstant{}};

encode_flow(#{<<"type">> := <<"PaymentFlowHold">>} = Entity) ->
    OnHoldExpiration = maps:get(<<"onHoldExpiration">>, Entity, <<"cancel">>),
    {hold, #payproc_InvoicePaymentParamsFlowHold{
        on_hold_expiration = binary_to_existing_atom(OnHoldExpiration, utf8)
    }}.

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
    <<"bin">> := Bin,
    <<"masked_pan">> := MaskedPan
}) ->
    {bank_card, #domain_BankCard{
        'token'  = Token,
        'payment_system' = binary_to_existing_atom(PaymentSystem, utf8),
        'bin' = Bin,
        'masked_pan' = MaskedPan
    }}.

encode_payment_terminal(#{<<"terminal_type">> := Type}) ->
    {payment_terminal, #domain_PaymentTerminal{
        terminal_type = binary_to_existing_atom(Type, utf8)
    }}.

encode_digital_wallet(#{<<"provider">> := Provider, <<"id">> := ID}) ->
    {digital_wallet, #domain_DigitalWallet{
        provider = binary_to_existing_atom(Provider, utf8),
        id = ID
    }}.

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

decode_invoice_event(#payproc_Event{
    id = EventID,
    created_at = CreatedAt,
    payload =  {invoice_changes, InvoiceChanges},
    source =  {invoice_id, InvoiceID} %%@TODO deal with Party source
}) ->
    Changes = decode_invoice_changes(InvoiceID, InvoiceChanges),
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

decode_invoice_changes(InvoiceID, InvoiceChanges) when is_list(InvoiceChanges) ->
    lists:foldl(
        fun(Change, Acc) ->
            case decode_invoice_change(InvoiceID, Change) of
                #{} = Decoded ->
                    Acc ++ [Decoded];
                undefined ->
                    Acc
            end
        end,
        [],
        InvoiceChanges
    ).

decode_invoice_change(_, {
    invoice_created,
    #payproc_InvoiceCreated{invoice = Invoice}
}) ->
    #{
        <<"changeType">> => <<"InvoiceCreated">>,
        <<"invoice">> => decode_invoice(Invoice)
    };

decode_invoice_change(_, {
    invoice_status_changed,
    #payproc_InvoiceStatusChanged{status = {Status, _}}
}) ->
    #{
        <<"changeType">> => <<"InvoiceStatusChanged">>,
        <<"status">> => genlib:to_binary(Status)
    };

decode_invoice_change(
    InvoiceID,
    {invoice_payment_change, #payproc_InvoicePaymentChange{
        id = PaymentID,
        payload = Change
    }}
) ->
    decode_payment_change(InvoiceID, PaymentID, Change);

decode_invoice_change(_, _) ->
    undefined.

decode_payment_change(
    InvoiceID,
    _PaymentID,
    {invoice_payment_started, #payproc_InvoicePaymentStarted{
        payment = Payment
    }}
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
    }}
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
    }}
) ->
    genlib_map:compact(maps:merge(
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID
        },
        decode_payment_status(Status)
    ));

decode_payment_change(
    _InvoiceID,
    PaymentID,
    {invoice_payment_refund_change, #payproc_InvoicePaymentRefundChange{
        id = RefundID,
        payload = Change
    }}
) ->
    decode_refund_change(PaymentID, RefundID, Change);

decode_payment_change(_, _, _) ->
    undefined.

decode_refund_change(
    PaymentID,
    _RefundID,
    {invoice_payment_refund_created, #payproc_InvoicePaymentRefundCreated{
        refund = Refund
    }}
) ->
    #{
        <<"changeType">> => <<"RefundStarted">>,
        <<"paymentID">> => PaymentID,
        <<"refund">> => decode_refund(Refund)
    };

decode_refund_change(
    PaymentID,
    RefundID,
    {invoice_payment_refund_status_changed, #payproc_InvoicePaymentRefundStatusChanged{
        status = Status
    }}
) ->
    genlib_map:compact(maps:merge(
        #{
            <<"changeType">> => <<"RefundStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"refundID">> => RefundID
        },
        decode_refund_status(Status)
    ));

decode_refund_change(_, _, _) ->
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
    decode_digital_wallet(DigitalWallet).

decode_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>}).

decode_bank_card_details(#domain_BankCard{
    'payment_system' = PaymentSystem,
    'masked_pan' = MaskedPan
}, V) ->
    V#{
        <<"cardNumberMask">> => decode_masked_pan(MaskedPan),
        <<"paymentSystem">> => genlib:to_binary(PaymentSystem)
    }.

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

decode_masked_pan(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_masked_pan(MaskedPan) ->
    MaskedPan.

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
decode_operation_failure({external_failure, #domain_ExternalFailure{
    code = Code,
    description = Description
}}) ->
    logic_error(Code, Description).

decode_stat_payment(#merchstat_StatPayment{
    id = PaymentID,
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
    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
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
    }, decode_stat_payment_status(Status))).

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
    'masked_pan' = MaskedPan
}}) ->
    {bank_card, #domain_BankCard{
        'token' = Token,
        'payment_system' = PaymentSystem,
        'bin' = Bin,
        'masked_pan' = MaskedPan
    }};

merchstat_to_domain({payment_terminal, #merchstat_PaymentTerminal{
    terminal_type = Type}}
) ->
    {payment_terminal, #domain_PaymentTerminal{terminal_type = Type}};
merchstat_to_domain({bank_card, #merchstat_PayoutCard{card = BankCard}}) ->
    merchstat_to_domain({bank_card, BankCard});
merchstat_to_domain({bank_account, #merchstat_PayoutAccount{account = #merchstat_BankAccount{
    account = Account,
    bank_name = BankName,
    bank_post_account = BankPostAccount,
    bank_bik = BankBik
}}}) ->
    {russian_bank_account, #domain_RussianBankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
    }};

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
    failure = {external_failure, #merchstat_ExternalFailure{
        code = Code,
        description = Description
    }}
}}) ->
    {Status, #domain_InvoicePaymentFailed{
        failure = {external_failure, #domain_ExternalFailure{
            code = Code,
            description = Description
        }}
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
    shop_id = ShopID,
    context = RawContext,
    template_id = TemplateID
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
        <<"cart">> => decode_invoice_cart(Cart),
        <<"invoiceTemplateID">> => TemplateID
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
    invoice_lifetime = #domain_LifetimeInterval{
        days = DD,
        months = MM,
        years = YY
    },
    description = Description,
    details = Details,
    context = RawContext
}) ->
    genlib_map:compact(#{
        <<"id">> => InvoiceTplID,
        <<"shopID">> => ShopID,
        <<"description">> => Description,
        <<"lifetime">> => #{
            <<"days">>   => undef_to_zero(DD),
            <<"months">> => undef_to_zero(MM),
            <<"years">>  => undef_to_zero(YY)
        },
        <<"details">> => decode_invoice_tpl_details(Details),
        <<"metadata">> => decode_context(RawContext)
    }).

undef_to_zero(undefined) ->
    0;
undef_to_zero(Int) ->
    Int.

decode_invoice_tpl_details({cart, Cart}) ->
    #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency">> => get_currency_from_cart(Cart),
        <<"cart">> => decode_invoice_cart(Cart)
    };
decode_invoice_tpl_details({product, #domain_InvoiceTemplateProduct{
    product = Product,
    price = Price,
    metadata = Metadata
}}) ->
    genlib_map:compact(#{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => Product,
        <<"price">> => decode_invoice_tpl_line_cost(Price),
        <<"taxMode">> => decode_invoice_line_tax_mode(Metadata)
    }).

get_currency_from_cart(#domain_InvoiceCart{lines = [FirstLine | _]}) ->
    #domain_InvoiceLine{price = #domain_Cash{currency = Currency}} = FirstLine,
    decode_currency(Currency).

decode_context(#'Content'{
    type = <<"application/json">>,
    data = InvoiceContext
}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);

decode_context(undefined) ->
    undefined.

decode_invoice_tpl_line_cost({unlim, _}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostUnlim">>
    };

decode_invoice_tpl_line_cost({fixed, #domain_Cash{amount = Amount, currency = Currency}}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
        <<"currency">> => decode_currency(Currency),
        <<"amount">> => Amount
    };

decode_invoice_tpl_line_cost({range, #domain_CashRange{
    upper = {_, #domain_Cash{amount = UpperBound, currency = Currency}},
    lower = {_, #domain_Cash{amount = LowerBound, currency = Currency}}
}}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostRange">>,
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

decode_contracts_map(Contracts) ->
    decode_map(Contracts, fun decode_contract/1).

decode_shops_map(Shops) ->
    decode_map(Shops, fun (S) -> decode_shop(S) end).

decode_map(Items, Fun) ->
    maps:values(maps:map(
        fun(_, I) -> Fun(I) end,
        Items
    )).

decode_contract(#domain_Contract{
    id = ContractID,
    created_at = CreatedAt,
    contractor = Contractor,
    payment_institution = PaymentInstitutionRef,
    valid_since = ValidSince,
    valid_until = ValidUntil,
    status = Status0,
    legal_agreement = LegalAgreement
}) ->
    Status = decode_contract_status(Status0),
    genlib_map:compact(maps:merge(#{
        <<"id">> => ContractID,
        <<"createdAt">> => CreatedAt,
        <<"contractor">> => decode_contractor(Contractor),
        <<"paymentInstitutionID">> => decode_payment_institution_ref(PaymentInstitutionRef),
        <<"validSince">> => ValidSince,
        <<"validUntil">> => ValidUntil,
        <<"legalAgreement">> => decode_legal_agreement(LegalAgreement)
    }, Status)).

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

decode_payout_tool(#domain_PayoutTool{
    id = ID,
    currency = Currency,
    payout_tool_info = Info
}) ->
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

decode_international_bank_account(#domain_InternationalBankAccount{
    account_holder = AccountHolder,
    bank_name = BankName,
    bank_address = BankAddress,
    iban = Iban,
    bic = Bic
}, V) ->
    V#{
        <<"accountHolder">> => AccountHolder,
        <<"bankName">> => BankName,
        <<"bankAddress">> => BankAddress,
        <<"iban">> => Iban,
        <<"bic">> => Bic
    }.

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

decode_shop(#domain_Shop{
    id = ShopID,
    created_at = CreatedAt,
    blocking = Blocking,
    suspension = Suspension,
    category  = #domain_CategoryRef{
        id = CategoryRef
    },
    details  = ShopDetails,
    location = Location,
    account = ShopAccount,
    contract_id = ContractID,
    payout_tool_id = PayoutToolID
}) ->
    genlib_map:compact(#{
        <<"id">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension),
        <<"categoryID">> => CategoryRef,
        <<"details">> => decode_shop_details(ShopDetails),
        <<"location">> => decode_shop_location(Location),
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID,
        <<"account">> => decode_shop_account(ShopAccount)
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
        actual_address = PrincipalPlaceOfBusiness
    }
}) ->
    genlib_map:compact(#{
        <<"entityType">> => <<"InternationalLegalEntity">>,
        <<"legalName">> => LegalName,
        <<"tradingName">> => TradingName,
        <<"registeredOffice">> => RegisteredOffice,
        <<"principalPlaceOfBusiness">> => PrincipalPlaceOfBusiness
    }).

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
        <<"residences">> => [list_to_binary(string:to_upper(atom_to_list(R))) || R <- ordsets:to_list(Residences)]
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

decode_refund(#domain_InvoicePaymentRefund{
    id = ID,
    status = Status,
    created_at = CreatedAt,
    reason = Reason
}) ->
    genlib_map:compact(maps:merge(
        #{
            <<"id">> => ID,
            <<"createdAt">> => CreatedAt,
            <<"reason">> => Reason
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
    type = PayoutType
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> => PayoutID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"amount">> => Amount,
        <<"fee">> => Fee,
        <<"currency">> => Currency,
        <<"payoutToolDetails">> => decode_stat_payout_tool_details(PayoutType)
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

filter_claims(undefined, Claims) ->
    Claims;
filter_claims(ClaimStatus, Claims) ->
    [Claim ||  Claim = #payproc_Claim{status = {Status, _}} <- Claims, Status =:= ClaimStatus].

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
    genlib_map:compact(maps:merge(
        #{
            <<"id">> => ID,
            <<"revision">> => Revision,
            <<"createdAt">> => CreatedAt,
            <<"updatedAt">> => UpdatedAt,
            <<"changeset">> => decode_party_changeset(ChangeSet)
        },
        decode_claim_status(Status)
    )).

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
    [decode_party_modification(PartyModification) || PartyModification <- PartyChangeset].

decode_party_modification({
    contract_modification,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = Modification
    }
}) ->
    maps:merge(#{
        <<"partyModificationType">> => <<"ContractModification">>,
        <<"contractID">> => ContractID
    }, decode_contract_modification(Modification));

decode_party_modification({
    shop_modification,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = ShopModification
    }
}) ->
    maps:merge(#{
        <<"partyModificationType">> => <<"ShopModification">>,
        <<"shopID">> => ShopID
    }, decode_shop_modification(ShopModification)).

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
    Basic = #{
        <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
        <<"payoutToolID">> => PayoutToolID
    },
    maps:merge(Basic, decode_payout_tool_params(PayoutToolParams)).

decode_legal_agreement(#domain_LegalAgreement{
    signed_at = SignedAt,
    legal_agreement_id = ID
}) ->
    #{
        <<"id">> => ID,
        <<"signedAt">> => SignedAt
    };

decode_legal_agreement(undefined) ->
    undefined.


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
    }.

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
        <<"type">> => genlib:to_binary(Type),
        <<"files">> => [decode_report_file(F) || F <- Files]
    }.

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
}) ->
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
    {ok, {200, [], Resp}};

process_merchant_stat_result(StatType, Result) ->
    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_info(StatType, S) || S <- Stats],
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #merchstat_DatasetTooBig{limit = Limit} ->
                    {ok, {400, [], limit_exceeded_error(Limit)}}
            end
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
            Resp = #{
                <<"result">> => DecodedData,
                <<"totalCount">> => TotalCount
            },
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #merchstat_DatasetTooBig{limit = Limit} ->
                    {ok, {400, [], limit_exceeded_error(Limit)}}
            end
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
            _ = lager:info("Attempting to create a missing party"),
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

collect_events(Limit, After, GetterFun, DecodeFun) ->
    collect_events([], Limit, After, GetterFun, DecodeFun).

collect_events(Collected, 0, _, _, _) ->
    {ok, Collected};

collect_events(Collected0, Left, After, GetterFun, DecodeFun) when Left > 0 ->
    case get_events(Left, After, GetterFun) of
        {ok, Events} ->
            Filtered = decode_and_filter_events(DecodeFun, Events),
            Collected = Collected0 ++ Filtered,
            case length(Events) of
                Left ->
                    collect_events(
                        Collected,
                        Left - length(Filtered),
                        get_last_event_id(Events),
                        GetterFun,
                        DecodeFun
                    );
                N when N < Left ->
                    {ok, Collected}
            end;
        Error ->
            Error
    end.

decode_and_filter_events(DecodeFun, Events) ->
    lists:filtermap(DecodeFun, Events).

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
    lists:map(
        fun(Method) ->
            {_, MethodTerms} = lists:unzip(proplists:lookup_all(Method, PaymentMethods)),
            decode_payment_method(Method, MethodTerms)
        end,
        proplists:get_keys(PaymentMethods)
    ).

decode_payment_method(bank_card, PaymentSystems) ->
    #{<<"method">> => <<"BankCard">>, <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems)};
decode_payment_method(payment_terminal, Providers) ->
    #{<<"method">> => <<"PaymentTerminal">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)};
decode_payment_method(digital_wallet, Providers) ->
    #{<<"method">> => <<"DigitalWallet">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}.

compute_terms(ServiceName, Args, Context) ->
    service_call(
        ServiceName,
        'ComputeTerms',
        Args,
        Context
    ).

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, [], <<>>}.

process_card_data(Data, ReqCtx) ->
    Result = service_call(
        cds_storage,
        'PutCardData',
        [encode_card_data(Data)],
        ReqCtx
    ),
    case Result of
        {ok, #'PutCardDataResult'{
            session_id = SessionID,
            bank_card = BankCard
        }} ->
            {{bank_card, BankCard}, SessionID};
        {exception, Exception} ->
            case Exception of
                #'InvalidCardData'{} ->
                    throw({ok, {400, [], logic_error(invalidRequest, <<"Card data is invalid">>)}});
                #'KeyringLocked'{} ->
                    % TODO
                    % It's better for the cds to signal woody-level unavailability when the
                    % keyring is locked, isn't it? It could always mention keyring lock as a
                    % reason in a woody error definition.
                    throw({error, reply_5xx(503)})
            end
    end.

encode_card_data(CardData) ->
    {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, CardData)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, CardData)),
    #'CardData'{
        pan  = CardNumber,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = genlib_map:get(<<"cardHolder">>, CardData),
        cvv = genlib_map:get(<<"cvv">>, CardData)
    }.

process_payment_terminal_data(Data, _ReqCtx) ->
    PaymentTerminal = #domain_PaymentTerminal{
        terminal_type = binary_to_existing_atom(
            genlib_map:get(<<"provider">>, Data),
            utf8
        )
    },
    {{payment_terminal, PaymentTerminal}, <<>>}.

process_digital_wallet_data(Data, _ReqCtx) ->
    DigitalWallet = case Data of
        #{<<"digitalWalletType">> := <<"DigitalWalletQIWI">>} ->
            #domain_DigitalWallet{
                provider = qiwi,
                id = maps:get(<<"phoneNumber">>, Data)
            }
    end,
    {{digital_wallet, DigitalWallet}, <<>>}.

enrich_client_info(ClientInfo, Context) ->
    ClientInfo#{<<"ip">> => prepare_client_ip(Context)}.

prepare_client_ip(Context) ->
    #{ip_address := IP} = get_peer_info(Context),
    genlib:to_binary(inet:ntoa(IP)).

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
    case rfc3339:format(Now + Lifetime, second) of
        {ok, Val} when is_binary(Val)->
            Val;
        Error ->
            error(Error)
    end.
