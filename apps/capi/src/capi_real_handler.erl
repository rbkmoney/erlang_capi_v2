-module(capi_real_handler).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").
-include_lib("dmsl/include/dmsl_webhooker_thrift.hrl").
-include_lib("dmsl/include/dmsl_user_interaction_thrift.hrl").
-include_lib("dmsl/include/dmsl_geo_ip_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_tool_provider_thrift.hrl").

-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").

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

-define(CAPI_NS, <<"com.rbkmoney.capi">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key()) ->
    Result :: false | {true, capi_auth:context()}.

authorize_api_key(OperationID, ApiKey) ->
    _ = capi_utils:logtag_process(operation_id, OperationID),
    capi_auth:authorize_api_key(OperationID, ApiKey).

-type request_data() :: #{atom() | binary() => term()}.
-type processing_context() :: #{
    swagger_context := swag_server:request_context(),
    woody_context   := woody_context:ctx()
}.

-spec handle_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    SwagContext :: swag_server:request_context()
) ->
    {ok | error, swag_server_logic_handler:response()}.

handle_request(OperationID, Req, SwagContext = #{auth_context := AuthContext}) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    try
        case capi_auth:authorize_operation(OperationID, Req, AuthContext) of
            ok ->
                WoodyContext = attach_deadline(Req, create_woody_context(Req, AuthContext)),
                Context = create_processing_context(SwagContext, WoodyContext),
                process_request(OperationID, Req, Context);
            {error, _} = Error ->
                _ = lager:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {ok, {401, [], undefined}}
        end
    catch
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        throw:{bad_deadline, Deadline} ->
            _ = lager:warning("Operation ~p failed due to invalid deadline ~p", [OperationID, Deadline]),
            {ok, {400, [], logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>)}}
    end.

-spec process_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: processing_context()
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

%%
%% invoicing
%%
process_request('CreateInvoice', Req, Context) ->
    PartyID = get_party_id(Context),
    try
        Call = {invoicing, 'Create', [encode_invoice_params(PartyID, maps:get('InvoiceParams', Req))]},
        service_call_with([user_info, party_creation], Call, Context)
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], make_invoice_and_token(Invoice, PartyID)}};
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

process_request('CreatePayment', Req, Context) ->
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

process_request('CreatePaymentResource', Req, Context) ->
    Params = maps:get('PaymentResourceParams', Req),
    ClientInfo = enrich_client_info(maps:get(<<"clientInfo">>, Params), Context),
    try
        Data = maps:get(<<"paymentTool">>, Params), % "V" ????
        {PaymentTool, PaymentSessionID} =
            case Data of
                #{<<"paymentToolType">> := <<"CardData"           >>} -> process_card_data(Data, Context);
                #{<<"paymentToolType">> := <<"PaymentTerminalData">>} -> process_payment_terminal_data(Data);
                #{<<"paymentToolType">> := <<"DigitalWalletData"  >>} -> process_digital_wallet_data(Data);
                #{<<"paymentToolType">> := <<"TokenizedCardData"  >>} -> process_tokenized_card_data(Data, Context)
            end,
        PaymentResource =
            #domain_DisposablePaymentResource{
                payment_tool = PaymentTool,
                payment_session_id = PaymentSessionID,
                client_info = encode_client_info(ClientInfo)
            },
        {ok, {201, [], decode_disposable_payment_resource(PaymentResource)}}
    catch
        Result -> Result
    end;

process_request('CreateInvoiceAccessToken', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    case get_invoice_by_id(InvoiceID, Context) of
        {ok, #'payproc_Invoice'{}} ->
            {ok, {201, [], issue_access_token(get_party_id(Context), {invoice, InvoiceID})}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetInvoiceByID', Req, Context) ->
    case get_invoice_by_id(maps:get(invoiceID, Req), Context) of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {200, [], decode_invoice(Invoice)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('FulfillInvoice', Req, Context) ->
    Call = {invoicing, 'Fulfill', [maps:get(invoiceID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))]},
    case service_call_with([user_info], Call, Context) of
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

process_request('RescindInvoice', Req, Context) ->
    Call = {invoicing, 'Rescind', [maps:get(invoiceID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))]},
    case service_call_with([user_info], Call, Context) of
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

process_request('GetInvoiceEvents', Req, Context) ->
    Result =
        collect_events(
            maps:get(limit, Req),
            genlib_map:get(eventID, Req),
            fun(Range) ->
                service_call_with([user_info], {invoicing, 'GetEvents', [maps:get(invoiceID, Req), Range]}, Context)
            end,
            fun decode_invoice_event/2,
            Context
        ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, [], Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, [], general_error(<<"Event not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetInvoicePaymentMethods', Req, Context) ->
    case construct_payment_methods(invoicing, [maps:get(invoiceID, Req)], Context) of
        {ok, PaymentMethods} when is_list(PaymentMethods) ->
            {ok, {200, [], PaymentMethods}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetPayments', Req, Context) ->
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

process_request('GetPaymentByID', Req, Context) ->
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

process_request('CancelPayment', Req, Context) ->
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

process_request('CapturePayment', Req, Context) ->
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

process_request('SearchInvoices', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => get_time('fromTime', Req),
        <<"to_time"                  >> => get_time('toTime', Req),
        <<"invoice_status"           >> => genlib_map:get('invoiceStatus', Req),
        <<"payment_status"           >> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow"             >> => genlib_map:get('paymentFlow', Req),
        <<"payment_method"           >> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id"      >> => genlib_map:get('customerID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"payment_email"            >> => genlib_map:get('payerEmail', Req),
        <<"payment_ip"               >> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint"      >> => genlib_map:get('payerFingerprint', Req),
        <<"payment_last_digits"      >> => genlib_map:get('lastDigits', Req),
        <<"payment_amount"           >> => genlib_map:get('paymentAmount', Req),
        <<"invoice_amount"           >> => genlib_map:get('invoiceAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_bin"              >> => genlib_map:get('bin', Req)
    },
    Opts = #{
        thrift_fun => 'GetInvoices',
        decode_fun => fun decode_stat_invoice/2
    },
    process_search_request(invoices, Query, Req, Context, Opts);

process_request('SearchPayments', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => get_time('fromTime', Req),
        <<"to_time"                  >> => get_time('toTime', Req),
        <<"payment_status"           >> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow"             >> => genlib_map:get('paymentFlow', Req),
        <<"payment_method"           >> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id"      >> => genlib_map:get('customerID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"payment_email"            >> => genlib_map:get('payerEmail', Req),
        <<"payment_ip"               >> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint"      >> => genlib_map:get('payerFingerprint', Req),
        <<"payment_last_digits"      >> => genlib_map:get('lastDigits', Req),
        <<"payment_amount"           >> => genlib_map:get('paymentAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_bin"              >> => genlib_map:get('bin', Req)
    },
    Opts = #{
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_payment/2
    },
    process_search_request(payments, Query, Req, Context, Opts);

process_request('SearchPayouts', Req, Context) ->
    Query = #{
        <<"merchant_id"    >> => get_party_id(Context),
        <<"shop_id"        >> => genlib_map:get('shopID', Req),
        <<"from_time"      >> => get_time('fromTime', Req),
        <<"to_time"        >> => get_time('toTime', Req),
        <<"payout_statuses">> => [<<"confirmed">>, <<"paid">>],
        <<"payout_id"      >> => genlib_map:get('payoutID', Req),
        <<"payout_type"    >> => encode_payout_type(genlib_map:get('payoutToolType', Req))
    },
    Opts = #{
        thrift_fun => 'GetPayouts',
        decode_fun => fun decode_stat_payout/2
    },
    process_search_request(payouts, Query, Req, Context, Opts);

process_request('SearchRefunds', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"refund_id"                >> => genlib_map:get('refundID', Req),
        <<"from_time"                >> => get_time('fromTime', Req),
        <<"to_time"                  >> => get_time('toTime', Req),
        <<"refund_status"            >> => genlib_map:get('refundStatus', Req)
    },
    Opts = #{
        %% TODO no special fun for refunds so we can use any
        %% should be fixed in new magista
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_refund/2
    },
    process_search_request(refunds, Query, Req, Context, Opts);

process_request('GetPaymentConversionStats', Req, Context) ->
    process_merchant_stat(payments_conversion_stat, Req, Context);

process_request('GetPaymentRevenueStats', Req, Context) ->
    process_merchant_stat(payments_turnover, Req, Context);

process_request('GetPaymentGeoStats', Req, Context) ->
    process_merchant_stat(payments_geo_stat, Req, Context);

process_request('GetPaymentRateStats', Req, Context) ->
    process_merchant_stat(customers_rate_stat, Req, Context);

process_request('GetPaymentMethodStats', Req, Context) ->
    bankCard =  maps:get(paymentMethod, Req),
    StatType = payments_pmt_cards_stat,
    process_merchant_stat(StatType, Req, Context);

process_request('GetLocationsNames', Req, Context) ->
    CallArgs = [ordsets:from_list(maps:get('geoIDs', Req)), maps:get('language', Req)],
    Call = {geo_ip_service, 'GetLocationName', CallArgs},
    case service_call(Call, Context) of
        {ok, LocationNames = #{}} ->
            PreparedLocationNames =
                maps:fold(
                    fun(GeoID, Name, Acc) -> [decode_location_name(GeoID, Name) | Acc] end,
                    [],
                    LocationNames
                ),
            {ok, {200, [], PreparedLocationNames}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
    end;

process_request('CreateRefund', Req, Context) ->
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

process_request('GetRefunds', Req, Context) ->
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

process_request('GetRefundByID', Req, Context) ->
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
%% invoice templating
%%
process_request('CreateInvoiceTemplate', Req, Context) ->
    PartyID = get_party_id(Context),
    try
        CallArgs = [encode_invoice_tpl_create_params(PartyID, maps:get('InvoiceTemplateCreateParams', Req))],
        service_call_with([user_info, party_creation], {invoice_templating, 'Create', CallArgs}, Context)
    of
        {ok, InvoiceTpl} ->
            {ok, {201, [], make_invoice_tpl_and_token(InvoiceTpl, PartyID)}};
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

process_request('GetInvoiceTemplateByID', Req, Context) ->
    Call = {invoice_templating, 'Get', [maps:get('invoiceTemplateID', Req)]},
    case service_call_with([user_info, party_creation], Call, Context) of
        {ok, InvoiceTpl} ->
            {ok, {200, [], decode_invoice_tpl(InvoiceTpl)}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, {404, [], general_error(<<"Invoice template not found">>)}}
    end;

process_request('UpdateInvoiceTemplate', Req, Context) ->
    try
        Params = encode_invoice_tpl_update_params(maps:get('InvoiceTemplateUpdateParams', Req)),
        Call = {invoice_templating, 'Update', [maps:get('invoiceTemplateID', Req), Params]},
        service_call_with([user_info, party_creation], Call, Context)
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

process_request('DeleteInvoiceTemplate', Req, Context) ->
    Call = {invoice_templating, 'Delete', [maps:get('invoiceTemplateID', Req)]},
    case service_call_with([user_info, party_creation], Call, Context) of
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

process_request('CreateInvoiceWithTemplate', Req, Context) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    InvoiceParams = maps:get('InvoiceParamsWithTemplate', Req),
    try
        Call = {invoicing, 'CreateWithTemplate', [encode_invoice_params_with_tpl(InvoiceTplID, InvoiceParams)]},
        service_call_with([user_info, party_creation], Call, Context)
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], make_invoice_and_token(Invoice, get_party_id(Context))}};
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

process_request('GetInvoicePaymentMethodsByTemplateID', Req, Context) ->
    Result =
        construct_payment_methods(
            invoice_templating,
            [maps:get('invoiceTemplateID', Req), capi_utils:unwrap(rfc3339:format(erlang:system_time()))],
            Context
        ),
    case Result of
        {ok, PaymentMethods} when is_list(PaymentMethods) ->
            {ok, {200, [], PaymentMethods}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, {404, [], general_error(<<"Invoice template not found">>)}}
    end;

%%
%% reports
%%
process_request('GetReports', Req, Context) ->
    ReportRequest = #reports_ReportRequest{
        party_id   = get_party_id(Context),
        shop_id    = maps:get(shopID, Req),
        time_range =
            #reports_ReportTimeRange{
                from_time = get_time('fromTime', Req),
                to_time   = get_time('toTime'  , Req)
            }
    },
    ReportTypes = [],
    Call = {reporting, 'GetReports', [ReportRequest, ReportTypes]},
    case service_call(Call, Context) of
        {ok, Reports} ->
            {ok, {200, [], [decode_report(R) || R <- Reports]}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, {400, [], limit_exceeded_error(Limit)}}
            end
    end;

process_request('GetReport', Req, Context) ->
    PartyId  = get_party_id(Context),
    ShopId   = maps:get(shopID, Req),
    ReportId = maps:get(reportID, Req),
    Call = {reporting, 'GetReport', [PartyId, ShopId, ReportId]},
    case service_call(Call, Context) of
        {ok, Report} ->
            {ok, {200, [], decode_report(Report)}};
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], general_error(<<"Report not found">>)}}
    end;

process_request('CreateReport', Req, Context) ->
    PartyId = get_party_id(Context),
    ShopId = maps:get(shopID, Req),
    ReportParams = maps:get('ReportParams', Req),
    ReportRequest = #reports_ReportRequest{
        party_id   = PartyId,
        shop_id    = ShopId,
        time_range =
            #reports_ReportTimeRange{
                from_time = get_time(<<"fromTime">>, ReportParams),
                to_time   = get_time(<<"toTime">>  , ReportParams)
            }
    },
    ReportType = encode_report_type(maps:get(<<"reportType">>, ReportParams)),
    case service_call({reporting, 'GenerateReport', [ReportRequest, ReportType]}, Context) of
        {ok, ReportId} ->
            {ok, Report} = service_call({reporting, 'GetReport', [PartyId, ShopId, ReportId]}, Context),
            {ok, {201, [], decode_report(Report)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}}
            end
    end;

process_request('DownloadFile', Req, Context) ->
    Call = {reporting, 'GetReport', [get_party_id(Context), maps:get(shopID, Req), maps:get(reportID, Req)]},
    case service_call(Call, Context) of
        {ok, #reports_Report{status = created, files = Files}} ->
            FileID = maps:get(fileID, Req),
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, Context);
                false ->
                    {ok, {404, [], general_error(<<"File not found">>)}}
            end;
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], general_error(<<"Report not found">>)}}
    end;

%%
%% party management
%%
process_request('ActivateShop', Req, Context) ->
    Call = {party_management, 'ActivateShop', [maps:get(shopID, Req)]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('SuspendShop', Req, Context) ->
    Call = {party_management, 'SuspendShop', [maps:get(shopID, Req)]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('GetShops', _Req, Context) ->
    Party = capi_utils:unwrap(get_my_party(Context)),
    {ok, {200, [], decode_shops_map(Party#domain_Party.shops)}};

process_request('GetShopByID', Req, Context) ->
    Call = {party_management, 'GetShop', [maps:get(shopID, Req)]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, Shop} ->
            {ok, {200, [], decode_shop(Shop)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {404, [], general_error(<<"Shop not found">>)}}
    end;

process_request('GetContracts', _Req, Context) ->
    Party = capi_utils:unwrap(get_my_party(Context)),
    {ok, {200, [], decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};

process_request('GetContractByID', Req, Context) ->
    ContractID = maps:get('contractID', Req),
    Party = capi_utils:unwrap(get_my_party(Context)),
    case genlib_map:get(ContractID, Party#domain_Party.contracts) of
        undefined ->
            {ok, {404, [], general_error(<<"Contract not found">>)}};
        Contract ->
            {ok, {200, [], decode_contract(Contract, Party#domain_Party.contractors)}}
    end;

process_request('GetPayoutTools', Req, Context) ->
    case get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            {ok, {200, [], [decode_payout_tool(P) || P <- PayoutTools]}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetPayoutToolByID', Req, Context) ->
    case get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            PayoutToolID = maps:get('payoutToolID', Req),
            case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                #domain_PayoutTool{} = P ->
                    {ok, {200, [], decode_payout_tool(P)}};
                false ->
                    {ok, {404, [], general_error(<<"PayoutTool not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetContractAdjustments', Req, Context) ->
    case get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, [], Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetContractAdjustmentByID', Req, Context) ->
    case get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            AdjustmentID = maps:get('adjustmentID', Req),
            case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
                #domain_ContractAdjustment{} = A ->
                    {ok, {200, [], decode_contract_adjustment(A)}};
                false ->
                    {ok, {404, [], general_error(<<"Adjustment not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetMyParty', _Req, Context) ->
    Party = capi_utils:unwrap(get_my_party(Context)),
    {ok, {200, [], decode_party(Party)}};

process_request('SuspendMyParty', _Req, Context) ->
    Call = {party_management, 'Suspend', []},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
            {ok, {204, [], undefined}}
    end;

process_request('ActivateMyParty', _Req, Context) ->
    Call = {party_management, 'Activate', []},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, [], undefined}}
    end;

process_request('GetCategories', _Req, #{woody_context := WoodyContext}) ->
    Categories = capi_utils:unwrap(capi_domain:get_categories(WoodyContext)),
    {ok, {200, [], [decode_category(C) || C <- Categories]}};

process_request('GetCategoryByRef', Req, Context) ->
    case get_category_by_id(genlib:to_int(maps:get(categoryID, Req)), Context) of
        {ok, Category} ->
            {ok, {200, [], decode_category(Category)}};
        {error, not_found} ->
            {404, [], general_error(<<"Category not found">>)}
    end;

process_request('GetScheduleByRef', Req, Context) ->
    case get_schedule_by_id(genlib:to_int(maps:get(scheduleID, Req)), Context) of
        {ok, Schedule} ->
            {ok, {200, [], decode_business_schedule(Schedule)}};
        {error, not_found} ->
            {404, [], general_error(<<"Schedule not found">>)}
    end;

process_request('GetPaymentInstitutions', Req, #{woody_context := WoodyContext}) ->
    try
        Residence = encode_residence(genlib_map:get(residence, Req)),
        Realm = genlib_map:get(realm, Req),
        {ok, PaymentInstObjects} = capi_domain:get_payment_institutions(WoodyContext),
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
        {ok, {200, [], Resp}}
    catch
        throw:{encode_residence, invalid_residence} ->
            {ok, {400, [], logic_error(invalidRequest, <<"Invalid residence">>)}}
    end;

process_request('GetPaymentInstitutionByRef', Req, #{woody_context := WoodyContext}) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case capi_domain:get({payment_institution, ?payment_institution_ref(PaymentInstitutionID)}, WoodyContext) of
        {ok, PaymentInstitution} ->
            {ok, {200, [], decode_payment_institution_obj(PaymentInstitution)}};
        {error, not_found} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPaymentTerms', Req, Context) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, #payproc_Varset{}, Context) of
        {ok, #domain_TermSet{payments = PaymentTerms}} ->
            {ok, {200, [], decode_payment_terms(PaymentTerms)}};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutMethods', Req, Context) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, prepare_varset(Req), Context) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_methods = PayoutMethods}}} ->
            {ok, {200, [], decode_payout_methods_selector(PayoutMethods)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, [], general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutSchedules', Req, Context) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, prepare_varset(Req), Context) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_schedules = Schedules}}} ->
            {ok, {200, [], decode_business_schedules_selector(Schedules)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, [], general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetAccountByID', Req, Context) ->
    Call = {party_management, 'GetAccountState', [genlib:to_int(maps:get('accountID', Req))]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, S} ->
            {ok, {200, [], decode_account_state(S)}};
        {exception, #payproc_AccountNotFound{}} ->
            {ok, {404, [], general_error(<<"Account not found">>)}}
    end;

process_request('GetClaims', Req, Context) ->
    Call = {party_management, 'GetClaims', []},
    Claims = capi_utils:unwrap(service_call_with([user_info, party_id, party_creation], Call, Context)),
    {ok, {200, [], decode_claims(filter_claims(maps:get('claimStatus', Req), Claims))}};

process_request('GetClaimByID', Req, Context) ->
    Call = {party_management, 'GetClaim', [get_party_id(Context), genlib:to_int(maps:get('claimID', Req))]},
    case service_call_with([user_info, party_creation], Call, Context) of
        {ok, Claim} ->
            case is_wallet_claim(Claim) of
                true ->
                    %% filter this out
                    {ok, {404, [], general_error(<<"Claim not found">>)}};
                false ->
                    {ok, {200, [], decode_claim(Claim)}}
            end;
        {exception, #payproc_ClaimNotFound{}} ->
            {ok, {404, [], general_error(<<"Claim not found">>)}}
    end;

process_request('CreateClaim', Req, Context) ->
    try
        Changeset = encode_claim_changeset(maps:get('ClaimChangeset', Req)),
        Call = {party_management, 'CreateClaim', [get_party_id(Context), Changeset]},
        case service_call_with([user_info, party_creation], Call, Context) of
            {ok, Claim} ->
                {ok, {201, [], decode_claim(Claim)}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidPartyStatus{} ->
                        {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                    #payproc_ChangesetConflict{} ->
                        {ok, {400, [], logic_error(changesetConflict, <<"Changeset conflict">>)}};
                    #payproc_InvalidChangeset{} ->
                        {ok, {400, [], logic_error(invalidChangeset, <<"Invalid changeset">>)}};
                    #'InvalidRequest'{errors = Errors} ->
                        {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
                end
        end
    catch
        throw:{encode_contract_modification, adjustment_creation_not_supported} ->
            {ok, {400, [], logic_error(invalidChangeset, <<"Contract adjustment creation not supported">>)}};
        throw:{encode_residence, invalid_residence} ->
            {ok, {400, [], logic_error(invalidRequest, <<"Invalid residence">>)}}
    end;

% TODO disabled temporary, exception handling must be fixed befor enabling
% process_request('UpdateClaimByID', Req, Context) ->
%     Call =
%         {party_management, 'UpdateClaim', [
%             genlib:to_int(maps:get('claimID', Req)),
%             genlib:to_int(maps:get('claimRevision', Req)),
%             encode_claim_changeset(maps:get('claimChangeset', Req))
%         ]},
%     Party = capi_utils:unwrap(service_call_with([user_info, party_id, party_creation], Call, Context)),
%     {ok, {200, [], decode_party(Party)}};

process_request('RevokeClaimByID', Req, Context) ->
    Call =
        {party_management, 'RevokeClaim', [
            genlib:to_int(maps:get('claimID', Req)),
            genlib:to_int(maps:get('claimRevision', Req)),
            encode_reason(maps:get('Reason', Req))
        ]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
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

%%
%% web hooks
%%
process_request('CreateWebhook', Req, Context) ->
    WebhookParams = encode_webhook_params(get_party_id(Context), maps:get('Webhook', Req)),
    ShopID = validate_webhook_params(WebhookParams),
    Call = {party_management, 'GetShop', [ShopID]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _} ->
            Webhook = capi_utils:unwrap(service_call({webhook_manager, 'Create', [WebhookParams]}, Context)),
            {ok, {201, [], decode_webhook(Webhook)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}}
    end;

process_request('GetWebhooks', _Req, Context) ->
    Webhooks = capi_utils:unwrap(service_call_with([party_id], {webhook_manager, 'GetList', []}, Context)),
    {ok, {200, [], [decode_webhook(V) || V <- Webhooks]}};

process_request('GetWebhookByID', Req, Context) ->
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case get_webhook(WebhookID, Context) of
                {ok, Webhook} ->
                    {ok, {200, [], decode_webhook(Webhook)}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {404, [], general_error(<<"Webhook not found">>)}}
            end;
        error ->
            {ok, {404, [], general_error(<<"Webhook not found">>)}}
    end;

process_request('DeleteWebhookByID', Req, Context) ->
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case delete_webhook(WebhookID, Context) of
                {ok, _} ->
                    {ok, {204, [], undefined}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {204, [], undefined}}
            end;
        error ->
            {ok, {404, [], general_error(<<"Webhook not found">>)}}
    end;

process_request('CreateCustomer', Req, Context) ->
    PartyID = get_party_id(Context),
    Call = {customer_management, 'Create', [encode_customer_params(PartyID, maps:get('Customer', Req))]},
    case service_call_with([party_creation], Call, Context) of
        {ok, Customer} ->
            {ok, {201, [], make_customer_and_token(Customer, PartyID)}};
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

process_request('GetCustomerById', Req, Context) ->
    case get_customer_by_id(maps:get('customerID', Req), Context) of
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

process_request('DeleteCustomer', Req, Context) ->
    case service_call({customer_management, 'Delete', [maps:get(customerID, Req)]}, Context) of
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

process_request('CreateCustomerAccessToken', Req, Context) ->
    CustomerID = maps:get(customerID, Req),
    case get_customer_by_id(CustomerID, Context) of
        {ok, #payproc_Customer{}} ->
            {ok, {201, [], issue_access_token(get_party_id(Context), {customer, CustomerID})}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
            end
    end;

process_request('CreateBinding', Req, Context) ->
    CallArgs = [maps:get(customerID, Req), encode_customer_binding_params(maps:get('CustomerBindingParams', Req))],
    Result =
        try
            service_call({customer_management, 'StartBinding', CallArgs}, Context)
        catch
            throw:Error when Error =:= invalid_token orelse Error =:= invalid_payment_session ->
                {error, Error}
        end,

    case Result of
        {ok, CustomerBinding} ->
            {ok, {201, [], decode_customer_binding(CustomerBinding, Context)}};
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
            {ok, {400, [], logic_error(invalidPaymentToolToken, <<"Specified payment tool token is invalid">>)}};
        {error, invalid_payment_session} ->
            {ok, {400, [], logic_error(invalidPaymentSession, <<"Specified payment session is invalid">>)}}
    end;

process_request('GetBindings', Req, Context) ->
    case get_customer_by_id(maps:get(customerID, Req), Context) of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            {ok, {200, [], [decode_customer_binding(B, Context) || B <- Bindings]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}};
                #payproc_CustomerNotFound{} ->
                    {ok, {404, [], general_error(<<"Customer not found">>)}}
            end
    end;

process_request('GetBinding', Req, Context) ->
    case get_customer_by_id(maps:get(customerID, Req), Context) of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            case lists:keyfind(maps:get(customerBindingID, Req), #payproc_CustomerBinding.id, Bindings) of
                #payproc_CustomerBinding{} = B ->
                    {ok, {200, [], decode_customer_binding(B, Context)}};
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

process_request('GetCustomerEvents', Req, Context) ->
    GetterFun =
        fun(Range) ->
            service_call({customer_management, 'GetEvents', [maps:get(customerID, Req), Range]}, Context)
        end,
    Result =
        collect_events(
            maps:get(limit, Req),
            genlib_map:get(eventID, Req),
            GetterFun,
            fun decode_customer_event/2,
            undefined
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

generate_report_presigned_url(FileID, Context) ->
    ExpiresAt = get_default_url_lifetime(),
    Call = {reporting, 'GeneratePresignedUrl', [FileID, ExpiresAt]},
    case service_call(Call, Context) of
        {ok, URL} ->
            {ok, {200, [], #{<<"url">> => URL}}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_FileNotFound{}->
                    {ok, {404, [], general_error(<<"File not found">>)}}
            end
    end.

validate_webhook_params(#webhooker_WebhookParams{event_filter = EventFilter}) ->
    validate_event_filter(EventFilter).

validate_event_filter({invoice, #webhooker_InvoiceEventFilter{shop_id = ShopID}}) ->
    validate_event_filter_shop(ShopID);

validate_event_filter({customer, #webhooker_CustomerEventFilter{shop_id = ShopID}}) ->
    validate_event_filter_shop(ShopID).

validate_event_filter_shop(ShopID) when ShopID /= undefined ->
    ShopID.

get_webhook(WebhookID, Context) ->
    PartyID = get_party_id(Context),
    case service_call({webhook_manager, 'Get', [WebhookID]}, Context) of
        {ok, Webhook = #webhooker_Webhook{party_id = PartyID}} ->
            {ok, Webhook};
        {ok, _Webhook} ->
            {exception, #webhooker_WebhookNotFound{}};
        {exception, Exception} ->
            {exception, Exception}
    end.

encode_webhook_id(WebhookID) ->
    try
        {ok, binary_to_integer(WebhookID)}
    catch
        error:badarg ->
            error
    end.

encode_webhook_params(PartyID, #{<<"scope">> := Scope, <<"url">> := URL}) ->
    #webhooker_WebhookParams{
        party_id     = PartyID,
        url          = URL,
        event_filter = encode_webhook_scope(Scope)
    }.

encode_webhook_scope(#{<<"topic">> := <<"InvoicesTopic">>, <<"shopID">> := ShopID, <<"eventTypes">> := EventTypes}) ->
    {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ShopID,
        types   = ordsets:from_list([
            encode_invoice_event_type(V) || V <- EventTypes
        ])
    }};
encode_webhook_scope(#{<<"topic">> := <<"CustomersTopic">>, <<"shopID">> := ShopID, <<"eventTypes">> := EventTypes}) ->
    {customer, #webhooker_CustomerEventFilter{
        shop_id = ShopID,
        types   = ordsets:from_list([
            encode_customer_event_type(V) || V <- EventTypes
        ])
    }}.

%%%

% Нужно быть аккуратным с флагами их порядок влияет на порядок аргументов при вызове функций!
% обычно параметры идут в порядке [user_info, party_id, party_creation],
% но это зависит от damsel протокола
service_call_with(Flags, Call, Context) ->
    % реверс тут чтобы в флагах писать порядок аналогично вызову функций
    service_call_with_(lists:reverse(Flags), Call, Context).

service_call_with_([user_info|T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(T, {ServiceName, Function, [get_user_info(Context) | Args]}, Context);
service_call_with_([party_id|T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(T, {ServiceName, Function, [get_party_id(Context) | Args]}, Context);
service_call_with_([party_creation|T], Call, Context) ->
    case service_call_with_(T, Call, Context) of
        {exception, #payproc_PartyNotFound{}} ->
            _ = lager:info("Attempting to create a missing party"),
            CreateCall = {party_management, 'Create', [get_party_params(Context)]},
            case service_call_with([user_info, party_id], CreateCall, Context) of
                {ok       , _                     } -> service_call_with_(T, Call, Context);
                {exception, #payproc_PartyExists{}} -> service_call_with_(T, Call, Context);
                Error                               -> Error
            end;
        Result ->
            Result
    end;
service_call_with_([], Call, Context) ->
    service_call(Call, Context).

service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    capi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

create_processing_context(SwaggerContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwaggerContext
    }.

create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    _ = lager:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => capi_auth:get_subject_id(AuthContext),
        realm    => ?REALM,
        email    => capi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => capi_auth:get_claim(<<"name">> , AuthContext, undefined)
    }).

attach_deadline(#{'X-Request-Deadline' := undefined}, Context) ->
    Context;
attach_deadline(#{'X-Request-Deadline' := Header}, Context) ->
    case capi_utils:parse_deadline(Header) of
        {ok, Deadline} when Deadline /= undefined ->
            woody_context:set_deadline(Deadline, Context);
        _ ->
            throw({bad_deadline, Header})
    end.

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

get_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

get_party_id(Context) ->
    capi_auth:get_subject_id(get_auth_context(Context)).

get_party_params(Context) ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = capi_auth:get_claim(<<"email">>, get_auth_context(Context))
        }
    }.

get_peer_info(#{swagger_context := #{peer := Peer}}) ->
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
            P = genlib_map:get(<<"price"   >>, Line),
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
        product     = genlib_map:get(<<"product"    >>, Params),
        description = genlib_map:get(<<"description">>, Params),
        cart        = encode_invoice_cart(Params)
    }.

encode_invoice_cart(Params) ->
    Cart     = genlib_map:get(<<"cart"    >>, Params),
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
        product  = genlib_map:get(<<"product" >>, Line),
        quantity = genlib_map:get(<<"quantity">>, Line),
        price    = Price,
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
    Amount   = genlib_map:get(<<"amount"  >>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_cash(Amount, Currency).

encode_cash(Amount, Currency) ->
    #domain_Cash{
        amount   = Amount,
        currency = encode_currency(Currency)
    }.

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
    PaymentTool = encode_payment_tool_token(Token),
    {ClientInfo, PaymentSession} = unwrap_payment_session(EncodedSession),
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = encode_client_info(ClientInfo)
        },
        contact_info = encode_contact_info(ContactInfo)
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
    'token'          = Token,
    'payment_system' = PaymentSystem,
    'bin'            = Bin,
    'masked_pan'     = MaskedPan,
    'token_provider' = TokenProvider,
    'issuer_country' = IssuerCountry,
    'bank_name'      = BankName,
    'metadata'       = Metadata
}) ->
    capi_utils:map_to_base64url(genlib_map:compact(#{
        <<"type"          >> => <<"bank_card">>,
        <<"token"         >> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin"           >> => Bin,
        <<"masked_pan"    >> => MaskedPan,
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
        <<"type"         >> => <<"payment_terminal">>,
        <<"terminal_type">> => Type
    }).

decode_digital_wallet(#domain_DigitalWallet{
    provider = Provider,
    id = ID
}) ->
    capi_utils:map_to_base64url(#{
        <<"type"    >> => <<"digital_wallet">>,
        <<"provider">> => atom_to_binary(Provider, utf8),
        <<"id"      >> => ID
    }).

decode_client_info(undefined) ->
    undefined;
decode_client_info(ClientInfo) ->
    #{
        <<"fingerprint">> => ClientInfo#domain_ClientInfo.fingerprint,
        <<"ip"         >> => ClientInfo#domain_ClientInfo.ip_address
    }.

encode_client_info(ClientInfo) ->
    #domain_ClientInfo{
        fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
        ip_address  = maps:get(<<"ip"         >>, ClientInfo)
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
    Context = genlib_map:get(<<"metadata">>, Params, DefaultMeta),
    encode_content(json, Context).

encode_content(json, Data) ->
    #'Content'{
        type = <<"application/json">>,
        data = jsx:encode(Data)
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

encode_shop_details(Details = #{<<"name">> := Name}) ->
    #domain_ShopDetails{
        name        = Name,
        description = genlib_map:get(<<"description">>, Details)
    }.

encode_shop_location(#{
    <<"locationType">> := <<"ShopLocationUrl">>,
    <<"url"         >> := Url
}) ->
    {url, Url}.

encode_category_ref(undefined) ->
    undefined;

encode_category_ref(Ref) ->
    #domain_CategoryRef{id = Ref}.

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
                contractor          = encode_contractor(maps:get(<<"contractor">>, Modification)),
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
                modification   = {creation, encode_payout_tool_params(Modification)}
            }};
        <<"ContractPayoutToolInfoModification">> ->
            {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                payout_tool_id = maps:get(<<"payoutToolID">>, Modification),
                modification   = {info_modification, encode_payout_tool_info(maps:get(<<"details">>, Modification))}
            }};
        <<"ContractReportingPreferencesChange">> ->
            {report_preferences_modification, encode_report_preferences(Modification)}
    end,
    #payproc_ContractModificationUnit{
        id           = ContractID,
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
                contract_id    = maps:get(<<"contractID"  >>, Modification),
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

encode_reason(undefined                ) ->undefined;
encode_reason(#{<<"reason">> := Reason}) -> Reason.

encode_legal_agreement(LegalAgreement) ->
    #domain_LegalAgreement{
        signed_at = maps:get(<<"signedAt">>, LegalAgreement),
        legal_agreement_id = maps:get(<<"id">>, LegalAgreement),
        valid_until = genlib_map:get(<<"validUntil">>, LegalAgreement)
    }.

encode_payout_tool_params(#{<<"currency">> := Currency, <<"details">> := Details}) ->
    #payproc_PayoutToolParams{
        currency = encode_currency(Currency),
        tool_info = encode_payout_tool_info(Details)
    }.

encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsBankAccount">>} = Tool) ->
   {russian_bank_account, encode_russian_bank_account(Tool)};
encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsInternationalBankAccount">>} = Tool) ->
   {international_bank_account, encode_international_bank_account(Tool)}.

encode_russian_bank_account(BankAccount) ->
    #domain_RussianBankAccount{
        account           = maps:get(<<"account"        >>, BankAccount),
        bank_name         = maps:get(<<"bankName"       >>, BankAccount),
        bank_post_account = maps:get(<<"bankPostAccount">>, BankAccount),
        bank_bik          = maps:get(<<"bankBik"        >>, BankAccount)
    }.

encode_international_bank_account(undefined) ->
    undefined;
encode_international_bank_account(Acc) ->
    #domain_InternationalBankAccount{
        iban   = genlib_map:get(<<"iban">>, Acc),
        number = genlib_map:get(<<"number">>, Acc),
        bank   = encode_international_bank_details(genlib_map:get(<<"bankDetails">>, Acc)),
        correspondent_account = encode_international_bank_account(genlib_map:get(<<"correspondentBankAccount">>, Acc))
    }.

encode_international_bank_details(undefined) ->
    undefined;
encode_international_bank_details(Acc) ->
    #domain_InternationalBankDetails{
        bic     = genlib_map:get(<<"bic">>, Acc),
        country = encode_residence(genlib_map:get(<<"countryCode">>, Acc)),
        name    = genlib_map:get(<<"name">>, Acc),
        address = genlib_map:get(<<"address">>, Acc),
        aba_rtn = genlib_map:get(<<"abartn">>, Acc)
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
encode_legal_entity(#{<<"entityType">> := <<"InternationalLegalEntity">>} = Entity) ->
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
    #domain_PaymentInstitutionRef{id = Ref}.

encode_residence(undefined) ->
    undefined;
encode_residence(Residence) when is_binary(Residence) ->
    try
        list_to_existing_atom(string:to_lower(binary_to_list(Residence)))
    catch
        error:badarg ->
            throw({encode_residence, invalid_residence})
    end.

decode_residence(undefined) ->
    undefined;
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

make_invoice_and_token(Invoice, PartyID) ->
    #{
        <<"invoice"           >> => decode_invoice(Invoice),
        <<"invoiceAccessToken">> => issue_access_token(PartyID, {invoice, Invoice#domain_Invoice.id})
    }.

make_invoice_tpl_and_token(InvoiceTpl, PartyID) ->
    #{
        <<"invoiceTemplate"           >> => decode_invoice_tpl(InvoiceTpl),
        <<"invoiceTemplateAccessToken">> =>
            issue_access_token(PartyID, {invoice_tpl, InvoiceTpl#domain_InvoiceTemplate.id})
    }.

make_customer_and_token(Customer, PartyID) ->
    #{
        <<"customer"           >> => decode_customer(Customer),
        <<"customerAccessToken">> => issue_access_token(PartyID, {customer, Customer#payproc_Customer.id})
    }.

issue_access_token(PartyID, TokenSpec) ->
    #{<<"payload">> => capi_auth:issue_access_token(PartyID, TokenSpec)}.

encode_bank_card(BankCard) ->
    {bank_card, #domain_BankCard{
        token          = maps:get(<<"token">>, BankCard),
        payment_system = encode_payment_system(maps:get(<<"payment_system">>, BankCard)),
        bin            = maps:get(<<"bin">>, BankCard),
        masked_pan     = maps:get(<<"masked_pan">>, BankCard),
        token_provider = encode_token_provider(genlib_map:get(<<"token_provider">>, BankCard)),
        issuer_country = encode_residence(genlib_map:get(<<"issuer_country">>, BankCard)),
        bank_name      = genlib_map:get(<<"bank_name">>, BankCard),
        metadata       = encode_bank_card_metadata(genlib_map:get(<<"metadata">>, BankCard))
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
        id       = ID
    }}.

encode_token_provider(TokenProvider) when TokenProvider /= undefined ->
    binary_to_existing_atom(TokenProvider, utf8);
encode_token_provider(undefined) ->
    undefined.

encode_customer_params(PartyID, Params) ->
    #payproc_CustomerParams{
        party_id     = PartyID,
        shop_id      = genlib_map:get(<<"shopID">>, Params),
        contact_info = encode_contact_info(genlib_map:get(<<"contactInfo">>, Params)),
        metadata     = encode_customer_metadata(genlib_map:get(<<"metadata">>, Params))
    }.

encode_contact_info(ContactInfo) ->
    #domain_ContactInfo{
        phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
        email        = genlib_map:get(<<"email">>, ContactInfo)
    }.

encode_customer_metadata(Meta) ->
    capi_json_marshalling:marshal(Meta).

encode_customer_binding_params(#{<<"paymentResource">> := PaymentResource}) ->
    PaymentTool = encode_payment_tool_token(maps:get(<<"paymentToolToken">>, PaymentResource)),
    {ClientInfo, PaymentSession} = unwrap_payment_session(maps:get(<<"paymentSession">>, PaymentResource)),
    #payproc_CustomerBindingParams{
        payment_resource =
            #domain_DisposablePaymentResource{
                payment_tool       = PaymentTool,
                payment_session_id = PaymentSession,
                client_info        = encode_client_info(ClientInfo)
            }
    }.

encode_optional_refund_cash(Params = #{<<"amount">> := _, <<"currency">> := _}, _, _, _) ->
    encode_cash(Params);
encode_optional_refund_cash(Params = #{<<"amount">> := _}, InvoiceID, PaymentID, Context) ->
    {ok, #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{
            cost = #domain_Cash{currency = Currency}
        }
    }} = get_payment_by_id(InvoiceID, PaymentID, Context),
    encode_cash(Params#{<<"currency">> => decode_currency(Currency)});
encode_optional_refund_cash(_, _, _, _) ->
    undefined.

decode_invoice_event(Event, Context) ->
    %%@TODO deal with Party source
    #payproc_Event{payload = {invoice_changes, InvoiceChanges}, source = {invoice_id, InvoiceID}} = Event,
    case decode_invoice_changes(InvoiceID, InvoiceChanges, Context) of
        [_Something | _] = Changes ->
            {true, #{
                <<"id"       >> => Event#payproc_Event.id,
                <<"createdAt">> => Event#payproc_Event.created_at,
                <<"changes"  >> => Changes
            }};
        [] ->
            false
    end.

decode_invoice_changes(InvoiceID, InvoiceChanges, Context) ->
    F = fun(Change, Acc) ->
            case decode_invoice_change(InvoiceID, Change, Context) of
                #{} = Decoded ->
                    Acc ++ [Decoded];
                undefined ->
                    Acc
            end
        end,
    lists:foldl(F, [], InvoiceChanges).

decode_invoice_change(_, {invoice_created, #payproc_InvoiceCreated{invoice = Invoice}}, _Context) ->
    #{
        <<"changeType">> => <<"InvoiceCreated">>,
        <<"invoice"   >> => decode_invoice(Invoice)
    };

decode_invoice_change(_, {invoice_status_changed, #payproc_InvoiceStatusChanged{status = {Status, _}}}, _Context) ->
    #{
        <<"changeType">> => <<"InvoiceStatusChanged">>,
        <<"status"    >> => genlib:to_binary(Status)
    };

decode_invoice_change(InvoiceID, {invoice_payment_change, PaymentChange}, Context) ->
    #payproc_InvoicePaymentChange{id = PaymentID, payload = Change} = PaymentChange,
    decode_payment_change(InvoiceID, PaymentID, Change, Context);

decode_invoice_change(_, _, _) ->
    undefined.

decode_payment_change(InvoiceID, _PaymentID, {invoice_payment_started, PaymentStarted}, Context) ->
    #payproc_InvoicePaymentStarted{payment = Payment} = PaymentStarted,
    #{
        <<"changeType">> => <<"PaymentStarted">>,
        <<"payment"   >> => decode_payment(InvoiceID, Payment, Context)
    };

decode_payment_change(_InvoiceID, PaymentID, {invoice_payment_session_change,
    #payproc_InvoicePaymentSessionChange{payload = {session_interaction_requested, InteractionRequested}}}, _Context) ->
    #payproc_SessionInteractionRequested{interaction = Interaction} = InteractionRequested,
    #{
        <<"changeType">> => <<"PaymentInteractionRequested">>,
        <<"paymentID">> => PaymentID,
        <<"userInteraction">> => decode_user_interaction(Interaction)
    };

decode_payment_change(_InvoiceID, PaymentID, {invoice_payment_status_changed, PaymentStatusChanged}, Context) ->
    #payproc_InvoicePaymentStatusChanged{status = Status} = PaymentStatusChanged,
    merge_and_compact(
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID" >> => PaymentID
        },
        decode_payment_status(Status, Context)
    );

decode_payment_change(InvoiceID, PaymentID, {invoice_payment_refund_change, PaymentRefundChange}, Context) ->
    #payproc_InvoicePaymentRefundChange{
        id      = RefundID,
        payload = Change
    } = PaymentRefundChange,
    decode_refund_change(InvoiceID, PaymentID, RefundID, Change, Context);

decode_payment_change(_, _, _, _) ->
    undefined.

decode_refund_change(InvoiceID, PaymentID, _RefundID, {invoice_payment_refund_created, Created}, Context) ->
    #payproc_InvoicePaymentRefundCreated{refund = Refund} = Created,
    #{
        <<"changeType">> => <<"RefundStarted">>,
        <<"paymentID" >> => PaymentID,
        <<"refund"    >> => decode_refund_for_event(Refund, InvoiceID, PaymentID, Context)
    };

decode_refund_change(_, PaymentID, RefundID, {invoice_payment_refund_status_changed, StatusChanged}, Context) ->
    #payproc_InvoicePaymentRefundStatusChanged{status = Status} = StatusChanged,
    merge_and_compact(
        #{
            <<"changeType">> => <<"RefundStatusChanged">>,
            <<"paymentID" >> => PaymentID,
            <<"refundID"  >> => RefundID
        },
        decode_refund_status(Status, Context)
    );

decode_refund_change(_, _, _, _, _) ->
    undefined.

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{payment = Payment}, Context) ->
    decode_payment(InvoiceID, Payment, Context).

decode_payment(InvoiceID, Payment, Context) ->
    #domain_Cash{
        amount   = Amount,
        currency = Currency
    } = Payment#domain_InvoicePayment.cost,
    merge_and_compact(#{
        <<"id"                    >> => Payment#domain_InvoicePayment.id,
        <<"invoiceID"             >> => InvoiceID,
        <<"createdAt"             >> => Payment#domain_InvoicePayment.created_at,
        % TODO whoops, nothing to get it from yet
        <<"flow"                  >> => decode_flow(Payment#domain_InvoicePayment.flow),
        <<"amount"                >> => Amount,
        <<"currency"              >> => decode_currency(Currency),
        <<"payer"                 >> => decode_payer(Payment#domain_InvoicePayment.payer),
        <<"makeRecurrent"         >> => decode_make_recurrent(Payment#domain_InvoicePayment.make_recurrent)
    }, decode_payment_status(Payment#domain_InvoicePayment.status, Context)).

decode_payer({customer, #domain_CustomerPayer{customer_id = ID}}) ->
    #{
        <<"payerType" >> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_payer({recurrent, #domain_RecurrentPayer{recurrent_parent = RecurrentParent, contact_info = ContactInfo}}) ->
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"contactInfo">> => decode_contact_info(ContactInfo),
        <<"recurrentParentPayment">> => decode_recurrent_parent(RecurrentParent)
    };
decode_payer({payment_resource, #domain_PaymentResourcePayer{resource = Resource, contact_info = ContactInfo}}) ->
    maps:merge(
        #{
            <<"payerType"  >> => <<"PaymentResourcePayer">>,
            <<"contactInfo">> => decode_contact_info(ContactInfo)
        },
        decode_disposable_payment_resource(Resource)
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

decode_bank_card_details(BankCard, V) ->
    LastDigits = decode_last_digits(BankCard#domain_BankCard.masked_pan),
    Bin = BankCard#domain_BankCard.bin,
    merge_and_compact(V, #{
        <<"lastDigits">>     => LastDigits,
        <<"bin">>            => Bin,
        <<"cardNumberMask">> => decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem" >> => genlib:to_binary(BankCard#domain_BankCard.payment_system),
        <<"tokenProvider" >> => decode_token_provider(BankCard#domain_BankCard.token_provider)
    }).

decode_token_provider(Provider) when Provider /= undefined ->
    genlib:to_binary(Provider);
decode_token_provider(undefined) ->
    undefined.

decode_payment_terminal_details(#domain_PaymentTerminal{terminal_type = Type}, V) ->
    V#{
        <<"provider">> => genlib:to_binary(Type)
    }.

decode_digital_wallet_details(#domain_DigitalWallet{provider = qiwi, id = ID}, V) ->
    V#{
        <<"digitalWalletDetailsType">> => <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask"         >> => mask_phone_number(ID)
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

decode_contact_info(#domain_ContactInfo{phone_number = PhoneNumber, email = Email}) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => PhoneNumber,
        <<"email"      >> => Email
    }).

decode_payment_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentFailed{failure = OperationFailure} ->
                decode_payment_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

decode_payment_operation_failure({operation_timeout, _}, _) ->
    payment_error(<<"timeout">>);
decode_payment_operation_failure({failure, Failure}, Context) ->
    case capi_auth:get_consumer(capi_auth:get_claims(get_auth_context(Context))) of
        client ->
            payment_error(payproc_errors:match('PaymentFailure', Failure, fun payment_error_client_maping/1));
        merchant ->
            % чтобы не городить ещё один обход дерева как в payproc_errors проще отформатировать в текст,
            % а потом уже в json
            decode_payment_operation_failure_(
                binary:split(erlang:list_to_binary(payproc_errors:format_raw(Failure)), <<":">>, [global])
            )
    end.

decode_payment_operation_failure_([H|T]) ->
    R = payment_error(H),
    case T of
        [] -> R;
        _  -> R#{<<"subError">> => decode_payment_operation_failure_(T)}
    end.

decode_make_recurrent(undefined) ->
    false;
decode_make_recurrent(Value) when is_boolean(Value) ->
    Value.

decode_recurrent_parent(#domain_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    };
decode_recurrent_parent(#merchstat_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    }.

payment_error(Code) ->
    #{<<"code">> => Code}.

%% client error mapping
%% @see https://github.com/petrkozorezov/swag/blob/master/spec/definitions/PaymentError.yaml
-spec payment_error_client_maping(dmsl_payment_processing_errors_thrift:'PaymentFailure'()) ->
    binary().
payment_error_client_maping({preauthorization_failed, _})->
    <<"PreauthorizationFailed">>;
payment_error_client_maping({authorization_failed, {account_blocked, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {rejected_by_issuer, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {payment_tool_rejected, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_not_found, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_limit_exceeded, _}}) ->
    <<"AccountLimitsExceeded">>;
payment_error_client_maping({authorization_failed, {insufficient_funds, _}}) ->
    <<"InsufficientFunds">>;
payment_error_client_maping(_) ->
    <<"PaymentRejected">>.

decode_stat_payment(Stat, Context) ->
    merge_and_compact(#{
        <<"id"             >> => Stat#merchstat_StatPayment.id,
        <<"shortID"        >> => Stat#merchstat_StatPayment.short_id,
        <<"invoiceID"      >> => Stat#merchstat_StatPayment.invoice_id,
        <<"shopID"         >> => Stat#merchstat_StatPayment.shop_id,
        <<"createdAt"      >> => Stat#merchstat_StatPayment.created_at,
        <<"amount"         >> => Stat#merchstat_StatPayment.amount,
        <<"flow"           >> => decode_stat_payment_flow(Stat#merchstat_StatPayment.flow),
        <<"fee"            >> => Stat#merchstat_StatPayment.fee,
        <<"currency"       >> => Stat#merchstat_StatPayment.currency_symbolic_code,
        <<"payer"          >> => decode_stat_payer(Stat#merchstat_StatPayment.payer),
        <<"geoLocationInfo">> => decode_geo_location_info(Stat#merchstat_StatPayment.location_info),
        <<"metadata"       >> => decode_context(Stat#merchstat_StatPayment.context),
        <<"makeRecurrent"  >> => decode_make_recurrent(Stat#merchstat_StatPayment.make_recurrent),
        <<"statusChangedAt">> => decode_status_changed_at(Stat#merchstat_StatPayment.status)
    }, decode_stat_payment_status(Stat#merchstat_StatPayment.status, Context)).

decode_stat_payer({customer, #merchstat_CustomerPayer{customer_id = ID}}) ->
    #{
        <<"payerType" >> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_stat_payer({recurrent, RecurrentPayer}) ->
    #merchstat_RecurrentPayer{
        recurrent_parent = RecurrentParent,
        phone_number = PhoneNumber,
        email = Email
    } = RecurrentPayer,
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email"      >> => Email
        }),
        <<"recurrentParentPayment">> => decode_recurrent_parent(RecurrentParent)
    };
decode_stat_payer({payment_resource, PaymentResource}) ->
    #merchstat_PaymentResourcePayer{
        payment_tool = PaymentTool,
        session_id = PaymentSession,
        fingerprint = Fingerprint,
        ip_address = IP,
        phone_number = PhoneNumber,
        email = Email
    } = PaymentResource,
    genlib_map:compact(#{
        <<"payerType"         >> => <<"PaymentResourcePayer">>,
        <<"paymentToolToken"  >> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentSession"    >> => PaymentSession,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"clientInfo"        >> => genlib_map:compact(#{
            <<"ip"         >> => IP,
            <<"fingerprint">> => Fingerprint
        }),
        <<"contactInfo"       >> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email"      >> => Email
        })
    }).

decode_stat_payment_tool_token(PaymentTool) ->
    decode_payment_tool_token(merchstat_to_domain(PaymentTool)).

decode_stat_payment_tool_details(PaymentTool) ->
    decode_payment_tool_details(merchstat_to_domain(PaymentTool)).

decode_stat_payment_status(PaymentStatus, Context) ->
    decode_payment_status(merchstat_to_domain(PaymentStatus), Context).

decode_stat_payment_flow(Flow) ->
    decode_flow(merchstat_to_domain(Flow)).

decode_status_changed_at({_, #merchstat_InvoicePaymentPending{}}) ->
    undefined;
decode_status_changed_at({_, #merchstat_InvoicePaymentProcessed{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentCaptured{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentCancelled{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentRefunded{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentFailed{at = ChangedAt}}) ->
    ChangedAt.

decode_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};

decode_flow({hold, #domain_InvoicePaymentFlowHold{on_hold_expiration = OnHoldExpiration, held_until = HeldUntil}}) ->
    #{
        <<"type"            >> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil"       >> => HeldUntil
    }.

merchstat_to_domain({bank_card, BankCard = #merchstat_BankCard{}}) ->
    {bank_card, #domain_BankCard{
        token          = BankCard#merchstat_BankCard.token,
        payment_system = BankCard#merchstat_BankCard.payment_system,
        bin            = BankCard#merchstat_BankCard.bin,
        masked_pan     = BankCard#merchstat_BankCard.masked_pan,
        token_provider = BankCard#merchstat_BankCard.token_provider
    }};
merchstat_to_domain({payment_terminal, #merchstat_PaymentTerminal{terminal_type = Type}}) ->
    {payment_terminal, #domain_PaymentTerminal{terminal_type = Type}};
merchstat_to_domain({digital_wallet, #merchstat_DigitalWallet{provider = Provider, id = ID}}) ->
    {digital_wallet, #domain_DigitalWallet{provider = Provider, id = ID}};
merchstat_to_domain({bank_card, #merchstat_PayoutCard{card = BankCard}}) ->
    merchstat_to_domain({bank_card, BankCard});
merchstat_to_domain({bank_account, {russian_payout_account, PayoutAccount}}) ->
    #merchstat_RussianPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {russian_bank_account, #domain_RussianBankAccount{
        account           = BankAccount#merchstat_RussianBankAccount.account,
        bank_name         = BankAccount#merchstat_RussianBankAccount.bank_name,
        bank_post_account = BankAccount#merchstat_RussianBankAccount.bank_post_account,
        bank_bik          = BankAccount#merchstat_RussianBankAccount.bank_bik
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
merchstat_to_domain({Status, #merchstat_InvoicePaymentFailed{failure = Failure}}) ->
    NewFailure =
        case Failure of
            {failure, _} ->
                Failure;
            {operation_timeout, #merchstat_OperationTimeout{}} ->
                {operation_timeout, #domain_OperationTimeout{}}
        end,
    {Status, #domain_InvoicePaymentFailed{failure = NewFailure}};

merchstat_to_domain({Status, #merchstat_InvoiceUnpaid{}}) ->
    {Status, #domain_InvoiceUnpaid{}};
merchstat_to_domain({Status, #merchstat_InvoicePaid{}}) ->
    {Status, #domain_InvoicePaid{}};
merchstat_to_domain({Status, #merchstat_InvoiceCancelled{details = Details}}) ->
    {Status, #domain_InvoiceCancelled{details = Details}};
merchstat_to_domain({Status, #merchstat_InvoiceFulfilled{details = Details}}) ->
    {Status, #domain_InvoiceFulfilled{details = Details}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentRefundPending{}}) ->
    {Status, #domain_InvoicePaymentRefundPending{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentRefundSucceeded{}}) ->
    {Status, #domain_InvoicePaymentRefundSucceeded{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentRefundFailed{failure = Failure}}) ->
    NewFailure =
        case Failure of
            {failure, _} ->
                Failure;
            {operation_timeout, #merchstat_OperationTimeout{}} ->
                {operation_timeout, #domain_OperationTimeout{}}
        end,
    {Status, #domain_InvoicePaymentRefundFailed{failure = NewFailure}};

merchstat_to_domain({instant, #merchstat_InvoicePaymentFlowInstant{}}) ->
    {instant, #domain_InvoicePaymentFlowInstant{}};
merchstat_to_domain({hold, Hold}) ->
    {hold, #domain_InvoicePaymentFlowHold{
        on_hold_expiration = Hold#merchstat_InvoicePaymentFlowHold.on_hold_expiration,
        held_until         = Hold#merchstat_InvoicePaymentFlowHold.held_until
    }}.

decode_invoice(Invoice) ->
    #domain_Cash{amount = Amount, currency = Currency} = Invoice#domain_Invoice.cost,
    #domain_InvoiceDetails{product = Product, description = Description, cart = Cart} =
        Invoice#domain_Invoice.details,
    merge_and_compact(#{
        <<"id"               >> => Invoice#domain_Invoice.id,
        <<"shopID"           >> => Invoice#domain_Invoice.shop_id,
        <<"createdAt"        >> => Invoice#domain_Invoice.created_at,
        <<"dueDate"          >> => Invoice#domain_Invoice.due,
        <<"amount"           >> => Amount,
        <<"currency"         >> => decode_currency(Currency),
        <<"metadata"         >> => decode_context(Invoice#domain_Invoice.context),
        <<"product"          >> => Product,
        <<"description"      >> => Description,
        <<"cart"             >> => decode_invoice_cart(Cart),
        <<"invoiceTemplateID">> => Invoice#domain_Invoice.template_id
    }, decode_invoice_status(Invoice#domain_Invoice.status)).

decode_invoice_status({Status, StatusInfo}) ->
    Reason =
        case StatusInfo of
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

decode_invoice_line(InvoiceLine = #domain_InvoiceLine{quantity = Quantity, price = #domain_Cash{amount = Price}}) ->
    genlib_map:compact(#{
        <<"product" >> => InvoiceLine#domain_InvoiceLine.product,
        <<"quantity">> => Quantity,
        <<"price"   >> => Price,
        <<"cost"    >> => Price * Quantity,
        <<"taxMode" >> => decode_invoice_line_tax_mode(InvoiceLine#domain_InvoiceLine.metadata)
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

decode_invoice_tpl(InvoiceTpl) ->
    #domain_LifetimeInterval{days = DD, months = MM, years = YY} = InvoiceTpl#domain_InvoiceTemplate.invoice_lifetime,
    genlib_map:compact(#{
        <<"id"         >> => InvoiceTpl#domain_InvoiceTemplate.id,
        <<"shopID"     >> => InvoiceTpl#domain_InvoiceTemplate.shop_id,
        <<"description">> => InvoiceTpl#domain_InvoiceTemplate.description,
        <<"lifetime"   >> =>
            #{
                <<"days"  >> => undef_to_zero(DD),
                <<"months">> => undef_to_zero(MM),
                <<"years" >> => undef_to_zero(YY)
            },
        <<"details"    >> => decode_invoice_tpl_details(InvoiceTpl#domain_InvoiceTemplate.details),
        <<"metadata"   >> => decode_context(InvoiceTpl#domain_InvoiceTemplate.context)
    }).

undef_to_zero(undefined) -> 0;
undef_to_zero(Int      ) -> Int.


decode_invoice_tpl_details({cart, Cart}) ->
    #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency"    >> => get_currency_from_cart(Cart),
        <<"cart"        >> => decode_invoice_cart(Cart)
    };
decode_invoice_tpl_details({product, Product}) ->
    genlib_map:compact(#{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product"     >> => Product#domain_InvoiceTemplateProduct.product,
        <<"price"       >> => decode_invoice_tpl_line_cost(Product#domain_InvoiceTemplateProduct.price),
        <<"taxMode"     >> => decode_invoice_line_tax_mode(Product#domain_InvoiceTemplateProduct.metadata)
    }).

get_currency_from_cart(#domain_InvoiceCart{lines = [FirstLine | _]}) ->
    #domain_InvoiceLine{price = #domain_Cash{currency = Currency}} = FirstLine,
    decode_currency(Currency).

decode_context(#'Content'{type = <<"application/json">>, data = InvoiceContext}) ->
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

decode_invoice_tpl_line_cost({range, #domain_CashRange{upper = {_, UpperCashBound}, lower = {_, LowerCashBound}}}) ->
    #domain_Cash{amount = UpperBound, currency = Currency} = UpperCashBound,
    #domain_Cash{amount = LowerBound, currency = Currency} = LowerCashBound,
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostRange">>,
        <<"currency">> => decode_currency(Currency),
        <<"range">> => #{
            <<"upperBound">> => UpperBound,
            <<"lowerBound">> => LowerBound
        }
    }.

decode_party(#domain_Party{id = PartyID, blocking = Blocking, suspension = Suspension}) ->
    #{
        <<"id">> => PartyID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension)
    }.

decode_contracts_map(Contracts, Contractors) ->
    decode_map(Contracts, fun(C) -> decode_contract(C, Contractors) end).

decode_shops_map(Shops) ->
    decode_map(Shops, fun decode_shop/1).

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

decode_contract_status({terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}) ->
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

decode_payout_tool_params(#payproc_PayoutToolParams{currency = Currency, tool_info = ToolInfo}) ->
    decode_payout_tool_params(Currency, ToolInfo).

decode_payout_tool_params(Currency, Info) ->
    #{
        <<"currency">> => decode_currency(Currency),
        <<"details">> => decode_payout_tool_details(Info)
    }.

decode_russian_bank_account(BankAccount, V) ->
    V#{
        <<"account"        >> => BankAccount#domain_RussianBankAccount.account,
        <<"bankName"       >> => BankAccount#domain_RussianBankAccount.bank_name,
        <<"bankPostAccount">> => BankAccount#domain_RussianBankAccount.bank_post_account,
        <<"bankBik"        >> => BankAccount#domain_RussianBankAccount.bank_bik
    }.
decode_international_bank_account(undefined, _) ->
    undefined;
decode_international_bank_account(BankAccount, V) ->
    genlib_map:compact(V#{
        <<"number">>                   => BankAccount#domain_InternationalBankAccount.number,
        <<"iban">>                     => BankAccount#domain_InternationalBankAccount.iban,
        <<"bankDetails">>              => decode_international_bank_details(
            BankAccount#domain_InternationalBankAccount.bank
        ),
        <<"correspondentBankAccount">> => decode_international_bank_account(
            BankAccount#domain_InternationalBankAccount.correspondent_account, #{}
        )
    }).

decode_international_bank_details(undefined) ->
    undefined;
decode_international_bank_details(Bank) ->
    genlib_map:compact(#{
         <<"bic">>         => Bank#domain_InternationalBankDetails.bic,
         <<"abartn">>      => Bank#domain_InternationalBankDetails.aba_rtn,
         <<"name">>        => Bank#domain_InternationalBankDetails.name,
         <<"countryCode">> => decode_residence(Bank#domain_InternationalBankDetails.country),
         <<"address">>     => Bank#domain_InternationalBankDetails.address
    }).

decode_contract_adjustment(ContractAdjustment) ->
    genlib_map:compact(#{
        <<"id"        >> => ContractAdjustment#domain_ContractAdjustment.id,
        <<"createdAt" >> => ContractAdjustment#domain_ContractAdjustment.created_at,
        <<"validSince">> => ContractAdjustment#domain_ContractAdjustment.valid_since,
        <<"validUntil">> => ContractAdjustment#domain_ContractAdjustment.valid_until
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

decode_shop_details(#domain_ShopDetails{name = Name, description = Description}) ->
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

decode_legal_entity({russian_legal_entity, LegalEntity}) ->
    #{
        <<"entityType"            >> => <<"RussianLegalEntity">>,
        <<"registeredName"        >> => LegalEntity#domain_RussianLegalEntity.registered_name,
        <<"registeredNumber"      >> => LegalEntity#domain_RussianLegalEntity.registered_number,
        <<"inn"                   >> => LegalEntity#domain_RussianLegalEntity.inn,
        <<"actualAddress"         >> => LegalEntity#domain_RussianLegalEntity.actual_address,
        <<"postAddress"           >> => LegalEntity#domain_RussianLegalEntity.post_address,
        <<"representativePosition">> => LegalEntity#domain_RussianLegalEntity.representative_position,
        <<"representativeFullName">> => LegalEntity#domain_RussianLegalEntity.representative_full_name,
        <<"representativeDocument">> => LegalEntity#domain_RussianLegalEntity.representative_document,
        <<"bankAccount"           >> =>
            decode_russian_bank_account(LegalEntity#domain_RussianLegalEntity.russian_bank_account, #{})
    };
decode_legal_entity({international_legal_entity, LegalEntity}) ->
    genlib_map:compact(#{
        <<"entityType">> => <<"InternationalLegalEntity">>,
        <<"legalName"               >> => LegalEntity#domain_InternationalLegalEntity.legal_name,
        <<"tradingName"             >> => LegalEntity#domain_InternationalLegalEntity.trading_name,
        <<"registeredOffice"        >> => LegalEntity#domain_InternationalLegalEntity.registered_address,
        <<"principalPlaceOfBusiness">> => LegalEntity#domain_InternationalLegalEntity.actual_address,
        <<"registeredNumber"        >> => LegalEntity#domain_InternationalLegalEntity.registered_number
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

decode_payment_institution_obj(#domain_PaymentInstitutionObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"id">> => Ref#domain_PaymentInstitutionRef.id,
        <<"name">> => Data#domain_PaymentInstitution.name,
        <<"description">> => Data#domain_PaymentInstitution.description,
        <<"realm">> => genlib:to_binary(Data#domain_PaymentInstitution.realm),
        <<"residences">> => [decode_residence(R) || R <- ordsets:to_list(Data#domain_PaymentInstitution.residences)]
    }).

is_blocked({blocked  , _}) -> true;
is_blocked({unblocked, _}) -> false.

is_suspended({suspended, _}) -> true;
is_suspended({active   , _}) ->false.

decode_stat_info(payments_conversion_stat, Response) ->
    #{
        <<"offset"         >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"successfulCount">> => genlib:to_int(maps:get(<<"successful_count">>, Response)),
        <<"totalCount"     >> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"conversion"     >> => genlib:to_float(maps:get(<<"conversion">>, Response))
    };

decode_stat_info(payments_geo_stat, Response) ->
    #{
        <<"offset"  >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"geoID"   >> => genlib:to_int(maps:get(<<"city_id">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit"  >> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue" >> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_info(payments_turnover, Response) ->
    #{
        <<"offset"  >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit"  >> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue" >> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_info(customers_rate_stat, Response) ->
    #{
        <<"uniqueCount">> => genlib:to_int(maps:get(<<"unic_count">>, Response))
    };

decode_stat_info(payments_pmt_cards_stat, Response) ->
    #{
        <<"statType"     >> => <<"PaymentMethodBankCardStat">>, %% @TODO deal with nested responses decoding
        <<"offset"       >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"totalCount"   >> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"paymentSystem">> => maps:get(<<"payment_system">>, Response),
        <<"profit"       >> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue"      >> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    }.

decode_stat_invoice(Invoice, _Context) ->
    merge_and_compact(#{
        <<"id"         >> => Invoice#merchstat_StatInvoice.id,
        <<"shopID"     >> => Invoice#merchstat_StatInvoice.shop_id,
        <<"createdAt"  >> => Invoice#merchstat_StatInvoice.created_at,
        <<"dueDate"    >> => Invoice#merchstat_StatInvoice.due,
        <<"amount"     >> => Invoice#merchstat_StatInvoice.amount,
        <<"currency"   >> => Invoice#merchstat_StatInvoice.currency_symbolic_code,
        <<"metadata"   >> => decode_context(Invoice#merchstat_StatInvoice.context),
        <<"product"    >> => Invoice#merchstat_StatInvoice.product,
        <<"description">> => Invoice#merchstat_StatInvoice.description,
        <<"cart"       >> => decode_invoice_cart(Invoice#merchstat_StatInvoice.cart)
    }, decode_stat_invoice_status(Invoice#merchstat_StatInvoice.status)).

decode_stat_invoice_status(Status) ->
    decode_invoice_status(merchstat_to_domain(Status)).

decode_refund_for_event(#domain_InvoicePaymentRefund{cash = #domain_Cash{}} = Refund, _, _, Context) ->
    decode_refund(Refund, Context);
decode_refund_for_event(#domain_InvoicePaymentRefund{cash = undefined} = Refund, InvoiceID, PaymentID, Context) ->
    % Need to fix it!
    {ok, #payproc_InvoicePayment{payment = #domain_InvoicePayment{cost = Cash}}} =
        get_payment_by_id(InvoiceID, PaymentID, Context),
    decode_refund(Refund#domain_InvoicePaymentRefund{cash = Cash}, Context).

decode_refund(Refund, Context) ->
    #domain_Cash{amount = Amount, currency = Currency} = Refund#domain_InvoicePaymentRefund.cash,
    merge_and_compact(
        #{
            <<"id"       >> => Refund#domain_InvoicePaymentRefund.id,
            <<"createdAt">> => Refund#domain_InvoicePaymentRefund.created_at,
            <<"reason"   >> => Refund#domain_InvoicePaymentRefund.reason,
            <<"amount"   >> => Amount,
            <<"currency" >> => decode_currency(Currency)
        },
        decode_refund_status(Refund#domain_InvoicePaymentRefund.status, Context)
    ).

decode_refund_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentRefundFailed{failure = OperationFailure} ->
                decode_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

decode_operation_failure({operation_timeout, _}, _) ->
    logic_error(timeout, <<"timeout">>);
decode_operation_failure({failure, #domain_Failure{code = Code, reason = Reason}}, _) ->
    logic_error(Code, Reason).

decode_stat_refund(Refund, Context) ->
    merge_and_compact(
        #{
            <<"invoiceID">> => Refund#merchstat_StatRefund.invoice_id,
            <<"paymentID">> => Refund#merchstat_StatRefund.payment_id,
            <<"id">>        => Refund#merchstat_StatRefund.id,
            <<"createdAt">> => Refund#merchstat_StatRefund.created_at,
            <<"amount">>    => Refund#merchstat_StatRefund.amount,
            <<"currency">>  => Refund#merchstat_StatRefund.currency_symbolic_code,
            <<"reason">>    => Refund#merchstat_StatRefund.reason
        },
        decode_stat_refund_status(Refund#merchstat_StatRefund.status, Context)
    ).

decode_stat_refund_status(RefundStatus, Context) ->
    decode_refund_status(merchstat_to_domain(RefundStatus), Context).

decode_stat_payout(Payout, _Context) ->
    merge_and_compact(#{
        <<"id"               >> => Payout#merchstat_StatPayout.id,
        <<"shopID"           >> => Payout#merchstat_StatPayout.shop_id,
        <<"createdAt"        >> => Payout#merchstat_StatPayout.created_at,
        <<"amount"           >> => Payout#merchstat_StatPayout.amount,
        <<"fee"              >> => Payout#merchstat_StatPayout.fee,
        <<"currency"         >> => Payout#merchstat_StatPayout.currency_symbolic_code,
        <<"payoutToolDetails">> => decode_stat_payout_tool_details(Payout#merchstat_StatPayout.type),
        <<"payoutSummary"    >> => decode_stat_payout_summary(Payout#merchstat_StatPayout.summary)
    }, decode_stat_payout_status(Payout#merchstat_StatPayout.status)).

decode_stat_payout_status({cancelled, #merchstat_PayoutCancelled{details = Details}}) ->
    #{
        <<"status"             >> => <<"cancelled">>,
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

decode_stat_payout_summary_item(PayoutSummary) ->
    genlib_map:compact(#{
        <<"amount"  >> => PayoutSummary#merchstat_PayoutSummaryItem.amount,
        <<"fee"     >> => PayoutSummary#merchstat_PayoutSummaryItem.fee,
        <<"currency">> => PayoutSummary#merchstat_PayoutSummaryItem.currency_symbolic_code,
        <<"count"   >> => PayoutSummary#merchstat_PayoutSummaryItem.count,
        <<"fromTime">> => PayoutSummary#merchstat_PayoutSummaryItem.from_time,
        <<"toTime"  >> => PayoutSummary#merchstat_PayoutSummaryItem.to_time,
        <<"type"    >> => genlib:to_binary(PayoutSummary#merchstat_PayoutSummaryItem.operation_type)
    }).

encode_payout_type('PayoutCard'   ) -> <<"bank_card">>;
encode_payout_type('PayoutAccount') -> <<"bank_account">>;
encode_payout_type(undefined      ) -> undefined.

create_dsl(QueryType, QueryBody, QueryParams) ->
    merge_and_compact(
        #{<<"query">> => maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{})},
        QueryParams
    ).

encode_payment_method('bankCard'       ) -> <<"bank_card">>;
encode_payment_method('paymentTerminal') -> <<"payment_terminal">>;
encode_payment_method(undefined        ) -> undefined.

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

decode_claim(Claim) ->
    merge_and_compact(
        #{
            <<"id"       >> => Claim#payproc_Claim.id,
            <<"revision" >> => Claim#payproc_Claim.revision,
            <<"createdAt">> => Claim#payproc_Claim.created_at,
            <<"updatedAt">> => Claim#payproc_Claim.updated_at,
            <<"changeset">> => decode_party_changeset(Claim#payproc_Claim.changeset)
        },
        decode_claim_status(Claim#payproc_Claim.status)
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
    #{<<"status">> => <<"ClaimPending">>};
decode_claim_status({'accepted', #payproc_ClaimAccepted{}}) ->
    #{<<"status">> => <<"ClaimAccepted">>};
decode_claim_status({'denied', #payproc_ClaimDenied{reason = Reason}}) ->
    #{<<"status">> => <<"ClaimDenied">>, <<"reason">> => Reason};
decode_claim_status({'revoked', #payproc_ClaimRevoked{reason = Reason}}) ->
    #{<<"status">> => <<"ClaimRevoked">>, <<"reason">> => Reason}.

decode_party_changeset(PartyChangeset) ->
    lists:filtermap(fun decode_party_modification/1, PartyChangeset).

decode_party_modification({contract_modification, ContractModification}) ->
    {true, maps:merge(
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID"           >> => ContractModification#payproc_ContractModificationUnit.id
        },
        decode_contract_modification(ContractModification#payproc_ContractModificationUnit.modification)
    )};
decode_party_modification({shop_modification, ShopModification}) ->
    {true, maps:merge(
        #{
            <<"partyModificationType">> => <<"ShopModification">>,
            <<"shopID"               >> => ShopModification#payproc_ShopModificationUnit.id
        },
        decode_shop_modification(ShopModification#payproc_ShopModificationUnit.modification)
    )}.

decode_contract_modification({creation, ContractParams}) ->
    #{
        <<"contractModificationType">> => <<"ContractCreation">>,
        <<"contractor"              >> => decode_contractor(ContractParams#payproc_ContractParams.contractor),
        <<"paymentInstitutionID"    >> =>
            decode_payment_institution_ref(ContractParams#payproc_ContractParams.payment_institution)
    };
decode_contract_modification({legal_agreement_binding, LegalAgreement}) ->
    #{
        <<"contractModificationType">> => <<"ContractLegalAgreementBinding">>,
        <<"legalAgreement"          >> => decode_legal_agreement(LegalAgreement)
    };
decode_contract_modification({adjustment_modification, AdjustmentModification}) ->
    #payproc_ContractAdjustmentModificationUnit{
        adjustment_id = AdjustmentID,
        modification = {creation, #payproc_ContractAdjustmentParams{
            % FIXME need swag support for this
            template = _Template
        }}
    } = AdjustmentModification,
    #{
        <<"contractModificationType">> => <<"ContractAdjustmentCreation">>,
        <<"adjustmentID"            >> => AdjustmentID
    };
decode_contract_modification({termination, #payproc_ContractTermination{reason = Reason}}) ->
    genlib_map:compact(#{
        <<"contractModificationType">> => <<"ContractTermination">>,
        <<"reason"                  >> => Reason
    });
decode_contract_modification({payout_tool_modification, #payproc_PayoutToolModificationUnit{
    payout_tool_id = PayoutToolID,
    modification   = {creation, PayoutToolParams}
}}) ->
    maps:merge(#{
        <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
        <<"payoutToolID"            >> => PayoutToolID
    }, decode_payout_tool_params(PayoutToolParams));
decode_contract_modification({payout_tool_modification, #payproc_PayoutToolModificationUnit{
    payout_tool_id = PayoutToolID,
    modification   = {info_modification, ToolInfo}
}}) ->
    #{
        <<"contractModificationType">> => <<"ContractPayoutToolInfoModification">>,
        <<"payoutToolID"            >> => PayoutToolID,
        <<"details"                 >> => decode_payout_tool_details(ToolInfo)
    };
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
decode_shop_modification({shop_account_creation, #payproc_ShopAccountParams{currency = Currency}}) ->
    #{
        <<"shopModificationType">> => <<"ShopAccountCreation">>,
        <<"currency"            >> => decode_currency(Currency)
    };
decode_shop_modification({category_modification, CategoryRef}) ->
    #{
        <<"shopModificationType">> => <<"ShopCategoryChange">>,
        <<"categoryID"          >> => decode_category_ref(CategoryRef)
    };
decode_shop_modification({location_modification, Location}) ->
    #{
        <<"shopModificationType">> => <<"ShopLocationChange">>,
        <<"location"            >> => decode_shop_location(Location)
    };
decode_shop_modification({details_modification, Details}) ->
    #{
        <<"shopModificationType">> => <<"ShopDetailsChange">>,
        <<"details"             >> => decode_shop_details(Details)
    };
decode_shop_modification({contract_modification, ContractMod}) ->
    #{
        <<"shopModificationType">> => <<"ShopContractBinding">>,
        <<"contractID"          >> => ContractMod#payproc_ShopContractModification.contract_id,
        <<"payoutToolID"        >> => ContractMod#payproc_ShopContractModification.payout_tool_id
    };
decode_shop_modification({payout_tool_modification, PayoutToolID}) ->
    #{
        <<"shopModificationType">> => <<"ShopPayoutToolChange">>,
        <<"payoutToolID"        >> => PayoutToolID
    };
decode_shop_modification({payout_schedule_modification, #payproc_ScheduleModification{schedule = ScheduleRef}}) ->
    genlib_map:compact(#{
        <<"shopModificationType">> => <<"ShopPayoutScheduleChange">>,
        <<"scheduleID"          >> => decode_business_schedule_ref(ScheduleRef)
    }).

decode_shop_params(ShopParams) ->
    #{
        <<"location"    >> => decode_shop_location(ShopParams#payproc_ShopParams.location),
        <<"details"     >> => decode_shop_details(ShopParams#payproc_ShopParams.details),
        <<"contractID"  >> => ShopParams#payproc_ShopParams.contract_id,
        <<"payoutToolID">> => ShopParams#payproc_ShopParams.payout_tool_id
    }.

decode_category(#domain_CategoryObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"name"       >> => Data#domain_Category.name,
        <<"categoryID" >> => Ref#domain_CategoryRef.id,
        <<"description">> => Data#domain_Category.description
    }).

decode_category_ref(#domain_CategoryRef{id = CategoryRef}) ->
    CategoryRef.

decode_shop_account(undefined) ->
    undefined;
decode_shop_account(#domain_ShopAccount{currency = Currency, settlement = SettlementID, guarantee = GuaranteeID}) ->
    #{
        <<"guaranteeID" >> => GuaranteeID,
        <<"settlementID">> => SettlementID,
        <<"currency"    >> => decode_currency(Currency)
    }.

decode_account_state(AccountState) ->
    #{
        <<"id"             >> => AccountState#payproc_AccountState.account_id,
        <<"ownAmount"      >> => AccountState#payproc_AccountState.own_amount,
        <<"availableAmount">> => AccountState#payproc_AccountState.available_amount,
        <<"currency"       >> => decode_currency(AccountState#payproc_AccountState.currency)
    }.

decode_payment_terms(#domain_PaymentsServiceTerms{currencies = Currencies, categories = Categories}) ->
    genlib_map:compact(#{
        <<"currencies">> => decode_payment_terms(fun decode_currency    /1, Currencies),
        <<"categories">> => decode_payment_terms(fun decode_category_ref/1, Categories)
    });
decode_payment_terms(undefined) ->
    #{}.

decode_payment_terms(Fun, {value, Val}) when is_list(Val) ->
    [Fun(V) || V <- Val];
decode_payment_terms(_, _) ->
    undefined.

decode_user_interaction({payment_terminal_reciept, TerminalReceipt}) ->
    #{
        <<"interactionType">> => <<"PaymentTerminalReceipt">>,
        <<"shortPaymentID" >> => TerminalReceipt#'PaymentTerminalReceipt'.short_payment_id,
        <<"dueDate"        >> => TerminalReceipt#'PaymentTerminalReceipt'.due
    };
decode_user_interaction({redirect, BrowserRequest}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => decode_browser_request(BrowserRequest)
    }.

decode_browser_request({get_request, #'BrowserGetRequest'{uri = UriTemplate}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => UriTemplate
    };
decode_browser_request({post_request, #'BrowserPostRequest'{uri = UriTemplate, form = UserInteractionForm}}) ->
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

decode_geo_location_info(#geo_ip_LocationInfo{city_geo_id = CityID, country_geo_id = CountryID}) ->
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

decode_currency(#domain_Currency   {symbolic_code = SymbolicCode}) -> SymbolicCode;
decode_currency(#domain_CurrencyRef{symbolic_code = SymbolicCode}) -> SymbolicCode.

encode_optional_currency(undefined   ) -> undefined;
encode_optional_currency(SymbolicCode) -> encode_currency(SymbolicCode).

encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{symbolic_code = SymbolicCode}.

decode_report(Report) ->
    #reports_ReportTimeRange{from_time = FromTime, to_time = ToTime} = Report#reports_Report.time_range,
    #{
        <<"id"       >> => Report#reports_Report.report_id,
        <<"createdAt">> => Report#reports_Report.created_at,
        <<"fromTime" >> => FromTime,
        <<"toTime"   >> => ToTime,
        <<"status"   >> => decode_report_status(Report#reports_Report.status),
        <<"type"     >> => decode_report_type(Report#reports_Report.report_type),
        <<"files"    >> => [decode_report_file(F) || F <- Report#reports_Report.files]
    }.

encode_report_type(<<"provisionOfService">>) -> provision_of_service;
encode_report_type(<<"paymentRegistry">>) -> payment_registry.

decode_report_status(pending) -> <<"pending">>;
decode_report_status(created) -> <<"created">>.

decode_report_type(provision_of_service) -> <<"provisionOfService">>;
decode_report_type(payment_registry) -> <<"paymentRegistry">>.

decode_report_file(#reports_FileMeta{file_id = ID, filename = Filename, signature = Signature}) ->
    #{
        <<"id"        >> => ID,
        <<"filename"  >> => Filename,
        <<"signatures">> => decode_report_file_signature(Signature)
    }.

decode_report_file_signature(#reports_Signature{md5 = MD5, sha256 = SHA256}) ->
    #{<<"md5">> => MD5, <<"sha256">> => SHA256}.

decode_customer_event(Event = #payproc_Event{source = {customer_id, _}, payload = Payload}, Context) ->
    case decode_customer_changes(Payload, Context) of
        [_Something | _] = Changes ->
            {true, #{
                <<"id"       >> => Event#payproc_Event.id,
                <<"createdAt">> => Event#payproc_Event.created_at,
                <<"changes"  >> => Changes
            }};
        [] ->
            false
    end.

decode_customer_changes({customer_changes, CustomerChanges}, Context) ->
    lists:filtermap(
        fun(V) -> decode_customer_change(V, Context) end,
        CustomerChanges
    ).

decode_customer_change({customer_binding_changed, CustomerBindingChanged}, Context) ->
    #payproc_CustomerBindingChanged{id = BindingID, payload = Payload} = CustomerBindingChanged,
    decode_customer_binding_change(BindingID, Payload, Context);
decode_customer_change(_, _) ->
    false.

decode_customer_binding_change(_, {started, Start}, Context) ->
    #payproc_CustomerBindingStarted{binding = CustomerBinding} = Start,
    {true, #{
        <<"changeType"     >> => <<"CustomerBindingStarted">>,
        <<"customerBinding">> => decode_customer_binding(CustomerBinding, Context)
    }};
decode_customer_binding_change(BindingID, {status_changed, StatusChange}, Context) ->
    #payproc_CustomerBindingStatusChanged{status = Status} = StatusChange,
    {true, merge_and_compact(
        #{
            <<"changeType">> => <<"CustomerBindingStatusChanged">>,
            <<"customerBindingID">> => BindingID
        },
        decode_customer_binding_status(Status, Context)
    )};
decode_customer_binding_change(BindingID, {interaction_requested, InteractionRequest}, _) ->
    #payproc_CustomerBindingInteractionRequested{interaction = UserInteraction} = InteractionRequest,
    {true, #{
        <<"changeType">> => <<"CustomerBindingInteractionRequested">>,
        <<"customerBindingID">> => BindingID,
        <<"userInteraction">> => decode_user_interaction(UserInteraction)
    }}.

decode_customer(Customer) ->
    #{
        <<"id"         >> => Customer#payproc_Customer.id,
        <<"shopID"     >> => Customer#payproc_Customer.shop_id,
        <<"status"     >> => decode_customer_status(Customer#payproc_Customer.status),
        <<"contactInfo">> => decode_contact_info(Customer#payproc_Customer.contact_info),
        <<"metadata"   >> => decode_customer_metadata(Customer#payproc_Customer.metadata)
    }.

decode_customer_status({Status, _}) ->
    erlang:atom_to_binary(Status, utf8).

decode_customer_metadata(Meta) ->
    capi_json_marshalling:unmarshal(Meta).

decode_customer_binding(CustomerBinding, Context) ->
    merge_and_compact(
        #{
            <<"id"             >> => CustomerBinding#payproc_CustomerBinding.id,
            <<"paymentResource">> =>
                decode_disposable_payment_resource(CustomerBinding#payproc_CustomerBinding.payment_resource)
        },
        decode_customer_binding_status(CustomerBinding#payproc_CustomerBinding.status, Context)
    ).

decode_disposable_payment_resource(Resource) ->
    #domain_DisposablePaymentResource{payment_tool = PaymentTool, payment_session_id = SessionID} = Resource,
    ClientInfo = decode_client_info(Resource#domain_DisposablePaymentResource.client_info),
    #{
        <<"paymentToolToken"  >> => decode_payment_tool_token(PaymentTool),
        <<"paymentSession"    >> => wrap_payment_session(ClientInfo, SessionID),
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"clientInfo"        >> => ClientInfo
    }.

decode_customer_binding_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #payproc_CustomerBindingFailed{failure = OperationFailure} ->
                decode_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

encode_optional_payout_method('BankAccount') ->
    #domain_PayoutMethodRef{id = russian_bank_account};
encode_optional_payout_method('InternationalBankAccount') ->
    #domain_PayoutMethodRef{id = international_bank_account};
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
    {binding, {started, #webhooker_CustomerBindingStarted{}}};
encode_customer_event_type(<<"CustomerBindingSucceeded">>) ->
    {binding, {succeeded, #webhooker_CustomerBindingSucceeded{}}};
encode_customer_event_type(<<"CustomerBindingFailed">>) ->
    {binding, {failed, #webhooker_CustomerBindingFailed{}}}.

decode_event_filter({invoice, #webhooker_InvoiceEventFilter{shop_id = ShopID, types = EventTypes}}) ->
    genlib_map:compact(#{
        <<"topic"     >> => <<"InvoicesTopic">>,
        <<"shopID"    >> => ShopID,
        <<"eventTypes">> => lists:flatmap(fun decode_invoice_event_type/1, ordsets:to_list(EventTypes))
    });
decode_event_filter({customer, #webhooker_CustomerEventFilter{shop_id = ShopID, types = EventTypes}}) ->
    genlib_map:compact(#{
        <<"topic"     >> => <<"CustomersTopic">>,
        <<"shopID"    >> => ShopID,
        <<"eventTypes">> => lists:map(fun decode_customer_event_type/1, ordsets:to_list(EventTypes))
    }).

decode_invoice_event_type({created, #webhooker_InvoiceCreated{}}) ->
    [<<"InvoiceCreated">>];
decode_invoice_event_type({status_changed, #webhooker_InvoiceStatusChanged{value = undefined}}) ->
    % TODO seems unmaintainable
    [decode_invoice_status_event_type(V) || V <- [
        ?invpaid(),
        ?invcancelled(),
        ?invfulfilled()
    ]];
decode_invoice_event_type({status_changed, #webhooker_InvoiceStatusChanged{value = Value}}) ->
    [decode_invoice_status_event_type(Value)];
decode_invoice_event_type({payment, {created, #webhooker_InvoicePaymentCreated{}}}) ->
    [<<"PaymentStarted">>];
decode_invoice_event_type({payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = undefined}}}) ->
    % TODO seems unmaintainable
    [decode_payment_status_event_type(V) || V <- [
        ?pmtprocessed(),
        ?pmtcaptured(),
        ?pmtcancelled(),
        ?pmtrefunded(),
        ?pmtfailed()
    ]];
decode_invoice_event_type({payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = Value}}}) ->
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

decode_webhook(Hook) ->
    #{
        <<"id"       >> => integer_to_binary(Hook#webhooker_Webhook.id),
        <<"active"   >> => Hook#webhooker_Webhook.enabled,
        <<"scope"    >> => decode_event_filter(Hook#webhooker_Webhook.event_filter),
        <<"url"      >> => Hook#webhooker_Webhook.url,
        <<"publicKey">> => Hook#webhooker_Webhook.pub_key
    }.

encode_stat_request(Dsl) ->
    encode_stat_request(Dsl, undefined).

encode_stat_request(Dsl, ContinuationToken) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl), ContinuationToken);

encode_stat_request(Dsl, ContinuationToken) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl,
        continuation_token = ContinuationToken
    }.

create_stat_dsl(StatType, Req, Context) ->
    FromTime = get_time('fromTime', Req),
    ToTime   = get_time('toTime'  , Req),
    SplitInterval =
        case StatType of
            customers_rate_stat ->
                get_time_diff(FromTime, ToTime);
            _ ->
                SplitUnit = genlib_map:get('splitUnit', Req),
                SplitSize = genlib_map:get('splitSize', Req),
                get_split_interval(SplitSize, SplitUnit)
        end,
    Query = #{
        <<"merchant_id"   >> => get_party_id(Context),
        <<"shop_id"       >> => genlib_map:get('shopID', Req),
        <<"from_time"     >> => FromTime,
        <<"to_time"       >> => ToTime,
        <<"split_interval">> => SplitInterval
    },
    create_dsl(StatType, Query, #{}).

process_merchant_stat(StatType, Req, Context) ->
    Call = {merchant_stat, 'GetStatistics', [encode_stat_request(create_stat_dsl(StatType, Req, Context))]},
    process_merchant_stat_result(StatType, service_call(Call, Context)).

process_merchant_stat_result(customers_rate_stat = StatType, {ok, #merchstat_StatResponse{data = {records, Stats}}}) ->
    Resp =
        case Stats of
            [            ] -> #{<<"uniqueCount">> => 0};
            [StatResponse] -> decode_stat_info(StatType, StatResponse)
        end,
    {ok, {200, [], Resp}};

process_merchant_stat_result(StatType, Result) ->
    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_info(StatType, S) || S <- Stats],
            {ok, {200, [], Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
    end.

process_search_request(QueryType, Query, Req, Context, Opts = #{thrift_fun := ThriftFun}) ->
    QueryParams = #{
        <<"size">> => genlib_map:get('limit', Req),
        <<"from">> => genlib_map:get('offset', Req)
    },
    ContinuationToken = genlib_map:get('continuationToken', Req),
    Call = {
        merchant_stat,
        ThriftFun,
        [encode_stat_request(create_dsl(QueryType, Query, QueryParams), ContinuationToken)]
    },
    process_search_request_result(QueryType, service_call(Call, Context), Context, Opts).

process_search_request_result(QueryType, Result, Context, #{decode_fun := DecodeFun}) ->
    case Result of
        {ok, #merchstat_StatResponse{
            data = {QueryType, Data},
            total_count = TotalCount,
            continuation_token = ContinuationToken
        }} ->
            DecodedData = [DecodeFun(D, Context) || D <- Data],
            Resp = genlib_map:compact(#{
                <<"result">> => DecodedData,
                <<"totalCount">> => TotalCount,
                <<"continuationToken">> => ContinuationToken
            }),
            {ok, {200, [], Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
    end.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            capi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

get_split_interval(SplitSize, minute) -> SplitSize * 60;
get_split_interval(SplitSize, hour  ) -> get_split_interval(SplitSize, minute) * 60;
get_split_interval(SplitSize, day   ) -> get_split_interval(SplitSize, hour  ) * 24;
get_split_interval(SplitSize, week  ) -> get_split_interval(SplitSize, day   ) * 7;
get_split_interval(SplitSize, month ) -> get_split_interval(SplitSize, day   ) * 30;
get_split_interval(SplitSize, year  ) -> get_split_interval(SplitSize, day   ) * 365.

get_time_diff(From, To) ->
    {DateFrom, TimeFrom} = parse_rfc3339_datetime(From),
    {DateTo, TimeTo} = parse_rfc3339_datetime(To),
    UnixFrom = genlib_time:daytime_to_unixtime({DateFrom, TimeFrom}),
    UnixTo = genlib_time:daytime_to_unixtime({DateTo, TimeTo}),
    UnixTo - UnixFrom.

parse_rfc3339_datetime(DateTime) ->
    {ok, {DateFrom, TimeFrom, _, _}} = rfc3339:parse(DateTime),
    {DateFrom, TimeFrom}.

format_request_errors([]    ) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

process_woody_error(_Source, result_unexpected   , _Details) -> {error, reply_5xx(500)};
process_woody_error(_Source, resource_unavailable, _Details) -> {error, reply_5xx(503)};
process_woody_error(_Source, result_unknown      , _Details) -> {error, reply_5xx(504)}.

get_invoice_by_id(InvoiceID, Context) ->
    service_call_with([user_info], {invoicing, 'Get', [InvoiceID]}, Context).

get_customer_by_id(CustomerID, Context) ->
    service_call({customer_management, 'Get', [CustomerID]}, Context).

get_payment_by_id(InvoiceID, PaymentID, Context) ->
    service_call_with([user_info], {invoicing, 'GetPayment', [InvoiceID, PaymentID]}, Context).

get_my_party(Context) ->
    Call = {party_management, 'Get', []},
    service_call_with([user_info, party_id, party_creation], Call, Context).


delete_webhook(WebhookID, Context) ->
    case get_webhook(WebhookID, Context) of
        {ok, #webhooker_Webhook{}} ->
            service_call({webhook_manager, 'Delete', [WebhookID]}, Context);
        Exception ->
            Exception
    end.

get_contract_by_id(ContractID, Context) ->
    Call = {party_management, 'GetContract', [ContractID]},
    service_call_with([user_info, party_id, party_creation], Call, Context).

get_category_by_id(CategoryID, #{woody_context := WoodyContext}) ->
    CategoryRef = {category, #domain_CategoryRef{id = CategoryID}},
    capi_domain:get(CategoryRef, WoodyContext).

get_schedule_by_id(ScheduleID, #{woody_context := WoodyContext}) ->
    Ref = {business_schedule, #domain_BusinessScheduleRef{id = ScheduleID}},
    capi_domain:get(Ref, WoodyContext).

collect_events(Limit, After, GetterFun, DecodeFun, Context) ->
    collect_events([], Limit, After, GetterFun, DecodeFun, Context).

collect_events(Collected, 0, _, _, _, _) ->
    {ok, Collected};

collect_events(Collected0, Left, After, GetterFun, DecodeFun, Context) when Left > 0 ->
    case get_events(Left, After, GetterFun) of
        {ok, Events} ->
            Filtered = decode_and_filter_events(DecodeFun, Context, Events),
            Collected = Collected0 ++ Filtered,
            case length(Events) of
                Left ->
                    collect_events(
                        Collected,
                        Left - length(Filtered),
                        get_last_event_id(Events),
                        GetterFun,
                        DecodeFun,
                        Context
                    );
                N when N < Left ->
                    {ok, Collected}
            end;
        Error ->
            Error
    end.

decode_and_filter_events(DecodeFun, Context, Events) ->
    lists:foldr(
        fun(Event, Acc) ->
             case DecodeFun(Event, Context) of
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
    service_call_with([user_info], {ServiceName, 'ComputeTerms', Args}, Context).

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, [], <<>>}.

process_card_data(Data, Context) ->
    put_card_data_to_cds(
        encode_card_data(Data),
        encode_session_data(Data),
        Context
    ).

encode_card_data(CardData) ->
    {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, CardData)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, CardData)),
    #'CardData'{
        pan  = CardNumber,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = genlib_map:get(<<"cardHolder">>, CardData)
    }.

encode_session_data(CardData) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = genlib_map:get(<<"cvv">>, CardData)
        }}
    }.

process_payment_terminal_data(Data) ->
    PaymentTerminal =
        #domain_PaymentTerminal{
            terminal_type = binary_to_existing_atom(genlib_map:get(<<"provider">>, Data), utf8)
        },
    {{payment_terminal, PaymentTerminal}, <<>>}.

process_digital_wallet_data(Data) ->
    DigitalWallet = case Data of
        #{<<"digitalWalletType">> := <<"DigitalWalletQIWI">>} ->
            #domain_DigitalWallet{
                provider = qiwi,
                id       = maps:get(<<"phoneNumber">>, Data)
            }
    end,
    {{digital_wallet, DigitalWallet}, <<>>}.

process_tokenized_card_data(Data, Context) ->
    Call = {get_token_provider_service_name(Data), 'Unwrap', [encode_wrapped_payment_tool(Data)]},
    UnwrappedPaymentTool = case service_call(Call, Context) of
        {ok, Tool} ->
            Tool;
        {exception, #'InvalidRequest'{}} ->
            throw({ok, {400, [], logic_error(invalidRequest, <<"Tokenized card data is invalid">>)}})
    end,
    process_put_card_data_result(
        put_card_data_to_cds(
            encode_tokenized_card_data(UnwrappedPaymentTool),
            encode_tokenized_session_data(UnwrappedPaymentTool),
            Context
        ),
        UnwrappedPaymentTool
    ).

get_token_provider_service_name(Data) ->
    case Data of
        #{<<"provider">> := <<"ApplePay">>} ->
            payment_tool_provider_apple_pay;
        #{<<"provider">> := <<"GooglePay">>} ->
            payment_tool_provider_google_pay;
        #{<<"provider">> := <<"SamsungPay">>} ->
            payment_tool_provider_samsung_pay
    end.

process_put_card_data_result(
    {{bank_card, BankCard}, SessionID},
    #paytoolprv_UnwrappedPaymentTool{
        card_info = #paytoolprv_CardInfo{
            payment_system = PaymentSystem,
            last_4_digits  = Last4
        },
        details = PaymentDetails
    }
) ->
    {
        {bank_card, BankCard#domain_BankCard{
            payment_system = PaymentSystem,
            masked_pan     = capi_utils:define(Last4, BankCard#domain_BankCard.masked_pan),
            token_provider = get_payment_token_provider(PaymentDetails)
        }},
        SessionID
    }.

get_payment_token_provider({apple, _}) ->
    applepay;
get_payment_token_provider({google, _}) ->
    googlepay;
get_payment_token_provider({samsung, _}) ->
    samsungpay.

encode_wrapped_payment_tool(Data) ->
    #paytoolprv_WrappedPaymentTool{
        request = encode_payment_request(Data)
    }.

encode_payment_request(#{<<"provider" >> := <<"ApplePay">>} = Data) ->
    {apple, #paytoolprv_ApplePayRequest{
        merchant_id = maps:get(<<"merchantID">>, Data),
        payment_token = encode_content(json, maps:get(<<"paymentToken">>, Data))
    }};
encode_payment_request(#{<<"provider" >> := <<"GooglePay">>} = Data) ->
    {google, #paytoolprv_GooglePayRequest{
        gateway_merchant_id = maps:get(<<"gatewayMerchantID">>, Data),
        payment_token = encode_content(json, maps:get(<<"paymentToken">>, Data))
    }};
encode_payment_request(#{<<"provider" >> := <<"SamsungPay">>} = Data) ->
    {samsung, #paytoolprv_SamsungPayRequest{
        service_id = genlib_map:get(<<"serviceID">>, Data),
        reference_id = genlib_map:get(<<"referenceID">>, Data)
    }}.

encode_tokenized_card_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {tokenized_card, #paytoolprv_TokenizedCard{
        dpan = DPAN,
        exp_date = #paytoolprv_ExpDate{
            month = Month,
            year = Year
        }
    }},
    card_info = #paytoolprv_CardInfo{
        cardholder_name = CardholderName
    }
}) ->
    #'CardData'{
        pan  = DPAN,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName
    };
encode_tokenized_card_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {card, #paytoolprv_Card{
        pan = PAN,
        exp_date = #paytoolprv_ExpDate{
            month = Month,
            year = Year
        }
    }},
    card_info = #paytoolprv_CardInfo{
        cardholder_name = CardholderName
    }
}) ->
    #'CardData'{
        pan  = PAN,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName
    }.

encode_tokenized_session_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {tokenized_card, #paytoolprv_TokenizedCard{
        auth_data = {auth_3ds, #paytoolprv_Auth3DS{
            cryptogram = Cryptogram,
            eci = ECI
        }}
    }}
}) ->
    #'SessionData'{
        auth_data = {auth_3ds, #'Auth3DS'{
            cryptogram = Cryptogram,
            eci = ECI
        }}
    };
encode_tokenized_session_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {card, #paytoolprv_Card{}}
}) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            %% TODO dirty hack for test GooglePay card data
            value = <<"">>
        }}
    }.

put_card_data_to_cds(CardData, SessionData, Context) ->
    BinData = lookup_bank_info(CardData#'CardData'.pan, Context),
    Call = {cds_storage, 'PutCardData', [CardData, SessionData]},
    case service_call(Call, Context) of
        {ok, #'PutCardDataResult'{session_id = SessionID, bank_card = BankCard}} ->
            {{bank_card, expand_card_info(BankCard, BinData)}, SessionID};
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

lookup_bank_info(Pan, Context) ->
    RequestVersion = {'last', #binbase_Last{}},
    Call = {binbase, 'Lookup', [Pan, RequestVersion]},
    case service_call(Call, Context) of
        {ok, #'binbase_ResponseData'{bin_data = BinData, version = Version}} ->
            {BinData, Version};
        {exception, #'binbase_BinNotFound'{}} ->
            throw({ok, {400, [], logic_error(invalidRequest, <<"Card data is invalid">>)}})
    end.

expand_card_info(BankCard, {BinData, Version}) ->
    try
        BankCard#'domain_BankCard'{
            payment_system = encode_binbase_payment_system(BinData#'binbase_BinData'.payment_system),
            issuer_country = encode_residence(BinData#'binbase_BinData'.iso_country_code),
            bank_name = BinData#'binbase_BinData'.bank_name,
            metadata = #{
                ?CAPI_NS =>
                    {obj, #{
                        {str, <<"version">>} => {i, Version}
                    }
                }
            }
        }
    catch
        throw:{encode_binbase_payment_system, invalid_payment_system} ->
            throw({ok, {400, [], logic_error(invalidRequest, <<"Unsupported card">>)}});
        throw:{encode_residence, invalid_residence} ->
            throw({ok, {400, [], logic_error(invalidRequest, <<"Unsupported card">>)}})
    end.

encode_binbase_payment_system(<<"VISA">>)                      -> visa;
encode_binbase_payment_system(<<"VISA/DANKORT">>)              -> visa;         % supposedly 🤔
encode_binbase_payment_system(<<"MASTERCARD">>)                -> mastercard;
% encode_binbase_payment_system(<<"???">>)                       -> visaelectron;
encode_binbase_payment_system(<<"MAESTRO">>)                   -> maestro;
% encode_binbase_payment_system(<<"???">>)                       -> forbrugsforeningen;
encode_binbase_payment_system(<<"DANKORT">>)                   -> dankort;
encode_binbase_payment_system(<<"AMERICAN EXPRESS">>)          -> amex;
encode_binbase_payment_system(<<"DINERS CLUB INTERNATIONAL">>) -> dinersclub;
encode_binbase_payment_system(<<"DISCOVER">>)                  -> discover;
encode_binbase_payment_system(<<"UNIONPAY">>)                  -> unionpay;
encode_binbase_payment_system(<<"JCB">>)                       -> jcb;
encode_binbase_payment_system(<<"NSPK MIR">>)                  -> nspkmir;
encode_binbase_payment_system(_) ->
    throw({encode_binbase_payment_system, invalid_payment_system}).

enrich_client_info(ClientInfo, Context) ->
    ClientInfo#{<<"ip">> => prepare_client_ip(Context)}.

prepare_client_ip(Context) ->
    #{ip_address := IP} = get_peer_info(Context),
    genlib:to_binary(inet:ntoa(IP)).

wrap_payment_session(ClientInfo, PaymentSession) ->
    capi_utils:map_to_base64url(#{
        <<"clientInfo"    >> => ClientInfo,
        <<"paymentSession">> => PaymentSession
    }).

unwrap_payment_session(Encoded) ->
    #{
        <<"clientInfo">> := ClientInfo,
        <<"paymentSession">> := PaymentSession
     } = try
            capi_utils:base64url_to_map(Encoded)
        catch
            error:badarg ->
                erlang:throw(invalid_payment_session)
        end,
    {ClientInfo, PaymentSession}.

get_default_url_lifetime() ->
    Now      = erlang:system_time(second),
    Lifetime = application:get_env(capi, reporter_url_lifetime, ?DEFAULT_URL_LIFETIME),
    capi_utils:unwrap(rfc3339:format(Now + Lifetime, second)).

compute_payment_institution_terms(PaymentInstitutionID, VS, Context) ->
    Call = {party_management, 'ComputePaymentInstitutionTerms', [?payment_institution_ref(PaymentInstitutionID), VS]},
    service_call_with([user_info, party_id, party_creation], Call, Context).

prepare_varset(Req) ->
    #payproc_Varset{
        currency      = encode_optional_currency     (genlib_map:get(currency    , Req)),
        payout_method = encode_optional_payout_method(genlib_map:get(payoutMethod, Req))
    }.

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

decode_optional(Arg, DecodeFun) when Arg /= undefined ->
    DecodeFun(Arg);
decode_optional(undefined, _) ->
    undefined.
