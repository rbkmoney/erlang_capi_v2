-module(capi_real_handler).

-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_cds_thrift.hrl").
-include_lib("cp_proto/include/cp_merch_stat_thrift.hrl").
-include_lib("cp_proto/include/cp_webhooker_thrift.hrl").
-include_lib("cp_proto/include/cp_user_interaction_thrift.hrl").
-include_lib("cp_proto/include/cp_geo_ip_thrift.hrl").

-behaviour(swag_server_logic_handler).

%% API callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

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
            process_woody_error(OperationID, Source, Class, Details)
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
    Params = encode_invoice_params(PartyID, maps:get('InvoiceParams', Req)),
    UserInfo = get_user_info(Context),
    Result = prepare_party(
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
    ),
    case Result of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], decode_invoice(Invoice)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    end;

process_request('CreatePayment', Req, Context, ReqCtx) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    ContactInfo = genlib_map:get(<<"contactInfo">>, PaymentParams),
    Token = genlib_map:get(<<"paymentToolToken">>, PaymentParams),
    EncodedSession = genlib_map:get(<<"paymentSession">>, PaymentParams),
    UserInfo = get_user_info(Context),

    Result = try
        PaymentTool = decode_bank_card(Token),
        {ClientInfo, PaymentSession} = unwrap_session(EncodedSession),
        Params =  #payproc_InvoicePaymentParams{
            'payer' = #domain_Payer{
                payment_tool = PaymentTool,
                session_id = PaymentSession,
                client_info = #domain_ClientInfo{
                    fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
                    ip_address = maps:get(<<"ip_address">>, ClientInfo)
                },
                contact_info = #domain_ContactInfo{
                    phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
                    email = genlib_map:get(<<"email">>, ContactInfo)
                }
            }
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
                    {ok, {400, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {400, [], general_error(<<"Invoice not found">>)}}
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

process_request('CreatePaymentToolToken', Req, Context, ReqCtx) ->
    Params = maps:get('PaymentToolTokenParams', Req),
    ClientInfo0 = maps:get(<<"clientInfo">>, Params),
    PaymentTool = maps:get(<<"paymentTool">>, Params),
    case PaymentTool of
        #{<<"paymentToolType">> := <<"CardData">>} ->
            process_card_data(ClientInfo0, PaymentTool, Context, ReqCtx);
        _ ->
            {ok, {400, [], logic_error(
                invalidPaymentTool,
                <<"Specified payment tool is invalid or unsupported">>
            )}}
    end;

process_request('CreateInvoiceAccessToken', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_Invoice'{}} ->
            AdditionalClaims = maps:with(
                [<<"name">>, <<"email">>],
                capi_auth:get_claims(get_auth_context(Context))
            ),
            {ok, Token} = capi_auth:issue_invoice_access_token(
                PartyID,
                InvoiceID,
                AdditionalClaims
            ),
            Resp = #{<<"payload">> => Token},
            {ok, {201, [], Resp}};
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
    Result  = collect_events(
        get_user_info(Context),
        maps:get(invoiceID, Req),
        maps:get(limit, Req),
        genlib_map:get(eventID, Req),
        ReqCtx
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            Resp = remove_empty_events([decode_event(I) || I <- Events]),
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invoice not found">>)}};
                #payproc_PartyNotFound{} ->
                    {ok, {400, [],  general_error(<<"Party not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, [], general_error(<<"Event not found">>)}};
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
                    {ok, {400, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {400, [], general_error(<<"Invoice not found">>)}}
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
                    {ok, {400, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {400, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('SearchInvoices', Req, Context, ReqCtx) ->
    Limit = genlib_map:get('limit', Req),
    Offset = genlib_map:get('offset', Req),
    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"invoice_id">> =>  genlib_map:get('invoiceID', Req),
        <<"from_time">> => get_time('fromTime', Req),
        <<"to_time">> => get_time('toTime', Req),
        <<"invoice_status">> => genlib_map:get('invoiceStatus', Req),
        <<"payment_status">> => genlib_map:get('paymentStatus', Req),
        <<"payment_id">> => genlib_map:get('paymentID', Req),
        <<"payment_email">> => genlib_map:get('payerEmail', Req),
        <<"payment_ip">> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint">> => genlib_map:get('payerFingerprint', Req),
        <<"payment_pan_mask">> => genlib_map:get('cardNumberMask', Req),
        <<"payment_amount">> => genlib_map:get('paymentAmount', Req),
        <<"invoice_amount">> => genlib_map:get('invoiceAmount', Req)
    },
    QueryParams = #{
        <<"size">> => Limit,
        <<"from">> => Offset
    },
    Dsl = create_dsl(invoices, Query, QueryParams),
    Result = service_call(
        merchant_stat,
        'GetInvoices',
        [encode_stat_request(Dsl)],
        ReqCtx
    ),
    case Result of
        {ok, #merchstat_StatResponse{data = {'invoices', Invoices}, total_count = TotalCount}} ->
            DecodedInvoices = [decode_stat_invoice(I) || I <- Invoices],
            Resp = #{
                <<"result">> => DecodedInvoices,
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
    end;

process_request('SearchPayments', Req, Context, ReqCtx) ->
    Limit = genlib_map:get('limit', Req),
    Offset = genlib_map:get('offset', Req),
    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"invoice_id">> =>  genlib_map:get('invoiceID', Req),
        <<"from_time">> => get_time('fromTime', Req),
        <<"to_time">> => get_time('toTime', Req),
        <<"payment_status">> => genlib_map:get('paymentStatus', Req),
        <<"payment_id">> => genlib_map:get('paymentID', Req),
        <<"payment_email">> => genlib_map:get('payerEmail', Req),
        <<"payment_ip">> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint">> => genlib_map:get('payerFingerprint', Req),
        <<"payment_pan_mask">> => genlib_map:get('cardNumberMask', Req),
        <<"payment_amount">> => genlib_map:get('paymentAmount', Req)
    },
    QueryParams = #{
        <<"size">> => Limit,
        <<"from">> => Offset
    },
    Dsl = create_dsl(payments, Query, QueryParams),
    Result = service_call(
        merchant_stat,
        'GetPayments',
        [encode_stat_request(Dsl)],
        ReqCtx
    ),
    case Result of
        {ok, #merchstat_StatResponse{data = {'payments', Payments}, total_count = TotalCount}} ->
            DecodedPayments = [decode_stat_payment(P) || P <- Payments],
            Resp = #{
                <<"result">> => DecodedPayments,
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
    end;

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

% process_request('CreateShop', Req, Context, ReqCtx) ->
%     UserInfo = get_user_info(Context),
%     PartyID = get_party_id(Context),
%     Params = maps:get('ShopParams', Req),

%     ShopParams = #payproc_ShopParams{
%         category =  encode_category_ref(genlib_map:get(<<"categoryID">>, Params)),
%         details = encode_shop_details(genlib_map:get(<<"details">>, Params)),
%         contract_id = genlib_map:get(<<"contractID">>, Params),
%         payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params)
%     },

%     Result = prepare_party(
%         Context,
%         ReqCtx,
%         fun () ->
%             service_call(
%                 party_management,
%                 'CreateShop',
%                 [UserInfo, PartyID, ShopParams],
%                 ReqCtx
%             )
%         end
%     ),

%     case Result of
%         {ok, R} ->
%             {ok, {202, [], decode_claim_result(R)}};
%         {exception, Exception} ->
%             case Exception of
%                 #payproc_InvalidUser{} ->
%                     {ok, {400, [], general_error(<<"Invalid party">>)}};
%                 #payproc_InvalidPartyStatus{} ->
%                     {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
%                 #payproc_ContractNotFound{} ->
%                     {ok, {400, [], general_error(<<"Contract not found">>)}};
%                 #payproc_InvalidContractStatus{} ->
%                     {ok, {400, [], logic_error(invalidContractStatus, <<"Invalid contract status">>)}};
%                 #payproc_PayoutToolNotFound{} ->
%                     {ok, {400, [], general_error(<<"Payout tool not found">>)}};
%                 #'InvalidRequest'{errors = Errors} ->
%                     {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
%             end
%     end;

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
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
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
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{
                    status = {suspension, {suspended, _}}
                } ->
                    {ok, {204, [], undefined}}
            end
    end;

% process_request('UpdateShop', Req, Context, ReqCtx) ->
%     UserInfo = get_user_info(Context),
%     PartyID = get_party_id(Context),
%     ShopID = maps:get(shopID, Req),
%     Params = maps:get('UpdateShopParams', Req),

%     ShopUpdate = #payproc_ShopUpdate{
%         category = encode_category_ref(genlib_map:get(<<"categoryID">>, Params)),
%         details =  encode_shop_details(genlib_map:get(<<"details">>, Params)),
%         contract_id = genlib_map:get(<<"contractID">>, Params),
%         payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params)
%     },

%     Result = prepare_party(
%         Context,
%         ReqCtx,
%         fun () ->
%             service_call(
%                 party_management,
%                 'UpdateShop',
%                 [UserInfo, PartyID, ShopID, ShopUpdate],
%                 ReqCtx
%             )
%         end
%     ),

%     case Result of
%         {ok, R} ->
%             {ok, {202, [], decode_claim_result(R)}};
%         {exception, Exception} ->
%             case Exception of
%                 #payproc_InvalidUser{} ->
%                     {ok, {400, [], general_error(<<"Invalid party">>)}};
%                 #payproc_ShopNotFound{} ->
%                     {ok, {404, [], general_error(<<"Shop not found">>)}};
%                 #payproc_InvalidPartyStatus{} ->
%                     {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
%                 #payproc_InvalidShopStatus{} ->
%                     {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
%                 #payproc_ContractNotFound{} ->
%                     {ok, {400, [], general_error(<<"Contract not found">>)}};
%                 #payproc_InvalidContractStatus{} ->
%                     {ok, {400, [], logic_error(invalidContractStatus, <<"Invalid contract status">>)}};
%                 #payproc_PayoutToolNotFound{} ->
%                     {ok, {400, [], general_error(<<"Payout tool not found">>)}};
%                 #'InvalidRequest'{errors = Errors} ->
%                     {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
%             end
%     end;

process_request('GetShops', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    case Result of
        {ok, #domain_Party{shops = Shops}} ->
            Resp = decode_shops_map(Shops),
            {ok, {200, [], Resp}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, {400, [], general_error(<<"Invalid party">>)}}
    end;

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
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}}
            end
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
            {ok, {200, [], Resp}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, {400, [], general_error(<<"Invalid party">>)}}
    end;

% process_request('CreateContract', Req, Context, ReqCtx) ->
%     UserInfo = get_user_info(Context),
%     PartyID = get_party_id(Context),
%     Params = maps:get('ContractParams', Req),

%     ContractParams = #payproc_ContractParams{
%         contractor = encode_contractor(genlib_map:get(<<"contractor">>, Params)),
%         payout_tool_params = encode_payout_tool_params(genlib_map:get(<<"payoutToolParams">>, Params))
%     },
%     Result = prepare_party(
%         Context,
%         ReqCtx,
%         fun () ->
%             service_call(
%                 party_management,
%                 'CreateContract',
%                 [UserInfo, PartyID, ContractParams],
%                 ReqCtx
%             )
%         end
%     ),
%     case Result of
%         {ok, R} ->
%             {ok, {202, [], decode_claim_result(R)}};
%         {exception, Exception} ->
%             case Exception of
%                 #payproc_InvalidUser{} ->
%                     {ok, {400, [], general_error(<<"Invalid party">>)}};
%                 #payproc_InvalidPartyStatus{} ->
%                     {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
%                 #'InvalidRequest'{errors = Errors} ->
%                     {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
%             end
%     end;

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
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
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
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_ContractNotFound{} ->
                    {ok, {404, [], general_error(<<"Contract not found">>)}}
            end
    end;

% process_request('CreatePayoutTool', Req, Context, ReqCtx) ->
%     UserInfo = get_user_info(Context),
%     PartyID = get_party_id(Context),
%     ContractID = maps:get('contractID', Req),
%     Params = maps:get('PayoutToolParams', Req),
%     PayoutToolParams = encode_payout_tool_params(Params),
%     Result = prepare_party(
%         Context,
%         ReqCtx,
%         fun () ->
%             service_call(
%                 party_management,
%                 'CreatePayoutTool',
%                 [UserInfo, PartyID, ContractID, PayoutToolParams],
%                 ReqCtx
%             )
%         end
%     ),
%     case Result of
%         {ok, _R} ->
%             {ok, {204, [], undefined}};
%         {exception, #payproc_InvalidPartyStatus{
%             status = {suspension, {suspended, _}}
%         }} ->
%             {ok, {204, [], undefined}};
%         {exception, Exception} ->
%             case Exception of
%                 #payproc_InvalidUser{} ->
%                     {ok, {400, [], general_error(<<"Invalid party">>)}};
%                 #payproc_InvalidPartyStatus{} ->
%                     {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
%                 #payproc_ContractNotFound{} ->
%                     {ok, {400, [], general_error(<<"Contract not found">>)}};
%                 #payproc_InvalidContractStatus{} ->
%                     {ok, {400, [], logic_error(invalidContractStatus, <<"Invalid contract status">>)}};
%                 #'InvalidRequest'{errors = Errors} ->
%                     {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
%             end
%     end;

process_request('GetMyParty', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    case Result of
        {ok, Party} ->
            Resp = decode_party(Party),
            {ok, {200, [], Resp}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, {400, [], general_error(<<"Invalid party">>)}}
    end;

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
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_InvalidPartyStatus{
                    status = {suspension, {suspended, _}}
                } ->
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
        {exception, #payproc_InvalidUser{}} ->
            {ok, {400, [], general_error(<<"Invalid party">>)}};
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
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_AccountNotFound{} ->
                    {ok, {404, [], general_error(<<"Account not found">>)}}
            end
    end;

process_request('GetClaims', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimStatus = encode_claim_status(maps:get('claimStatus', Req)),
    Result = prepare_party(
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
    case Result of
        {ok, Claims} ->
            Resp = decode_claims(filter_claims(ClaimStatus, Claims)),
            {ok, {200, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_ClaimNotFound{} ->
                    {ok, {404, [], general_error(<<"Claim not found">>)}};
                #payproc_InvalidClaimStatus{} ->
                    {ok, {400, [], general_error(<<"Invalid claim status">>)}}
            end
    end;

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
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_InvalidPartyStatus{
                    status = {suspension, {suspended, _}}
                } ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('CreateClaim', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Changeset = encode_claim_changeset(maps:get('claimChangeset', Req)),
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
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_InvalidPartyStatus{
                    status = {suspension, {active, _}}
                } ->
                    {ok, {204, [], undefined}}
            end
    end;

% TODO disabled temporary, exception handling must be fixed befor enabling
% process_request('UpdateClaimByID', Req, Context, ReqCtx) ->
%     UserInfo = get_user_info(Context),
%     PartyID = get_party_id(Context),
%     ClaimID = genlib:to_int(maps:get('claimID', Req)),
%     ClaimRevision = genlib:to_int(maps:get('claimRevision', Req)),
%     Changeset = encode_claim_changeset(maps:get('claimChangeset', Req)),
%     Result = prepare_party(
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
%     case Result of
%         {ok, Party} ->
%             Resp = decode_party(Party),
%             {ok, {200, [], Resp}};
%         {exception, #payproc_InvalidUser{}} ->
%             {ok, {400, [], general_error(<<"Invalid party">>)}}
%     end;

process_request('RevokeClaimByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimID = genlib:to_int(maps:get('claimID', Req)),
    ClaimRevision = genlib:to_int(maps:get('claimRevision', Req)),
    Reason = encode_reason(maps:get('reason', Req)),
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
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_ClaimNotFound{} ->
                    {ok, {404, [], general_error(<<"Claim not found">>)}}
            end
    end;

process_request('CreateWebhook', Req, Context, ReqCtx) ->
    PartyID = get_party_id(Context),
    Params = maps:get('WebhookParams', Req),
    EventFilter = encode_event_filter(maps:get(<<"scope">>, Params)),
    case validate_event_filter(EventFilter, Context, ReqCtx) of
        {ok, _} ->
            WebhookParams = #webhooker_WebhookParams{
                party_id     = PartyID,
                url          = maps:get(<<"url">>, Params),
                event_filter = EventFilter
            },
            {ok, Webhook} = service_call(webhook_manager, 'Create', [WebhookParams], ReqCtx),
            Resp = decode_webhook(Webhook),
            {ok, {201, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {400, [], general_error(<<"Invalid party">>)}};
                #payproc_PartyNotFound{} ->
                    {ok, {400, [],  general_error(<<"Party not found">>)}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}}
            end
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

process_request(_OperationID, _Req, _Context, _ReqCtx) ->
    {error, reply_5xx(501)}.

validate_event_filter({invoice, #webhooker_InvoiceEventFilter{
    shop_id = ShopID
}}, Context, ReqCtx) when ShopID /= undefined ->
    PartyID = get_party_id(Context),
    UserInfo = get_user_info(Context),
    service_call(party_management, 'GetShop', [UserInfo, PartyID, ShopID], ReqCtx).

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

%%%

service_call(ServiceName, Function, Args, Context) ->
    cp_proto:call_service(ServiceName, Function, Args, Context, capi_woody_event_handler).

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
    InvoiceContext = jsx:encode(genlib_map:get(<<"metadata">>, InvoiceParams)),
    #payproc_InvoiceParams{
        party_id = PartyID,
        details = encode_invoice_details(InvoiceParams),
        cost = encode_cash(InvoiceParams),
        due      = get_time(<<"dueDate">>, InvoiceParams),
        context  = #'Content'{
            type = <<"application/json">>,
            data = InvoiceContext
        },
        shop_id = genlib_map:get(<<"shopID">>, InvoiceParams)
    }.

encode_invoice_details(Params) ->
    #domain_InvoiceDetails{
        product = genlib_map:get(<<"product">>, Params),
        description = genlib_map:get(<<"description">>, Params)
    }.

encode_cash(Params) ->
    #domain_Cash{
        amount = genlib_map:get(<<"amount">>, Params),
        currency = encode_currency(genlib_map:get(<<"currency">>, Params))
    }.

encode_bank_card(#domain_BankCard{
    'token'  = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan
}) ->
    base64url:encode(jsx:encode(#{
        <<"token">> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin">> => Bin,
        <<"masked_pan">> => MaskedPan
    })).

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

encode_claim_status(Status) ->
    case Status of
        undefined ->
            undefined;
        <<"pending">> ->
            pending;
        <<"accepted">> ->
            accepted;
        <<"denied">> ->
            denied;
        <<"revoked">> ->
            revoked
    end.

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
                contractor = encode_contractor(maps:get(<<"contractor">>, Modification))
            }};
        <<"ContractTermination">> ->
            {termination, #payproc_ContractTermination{
                reason = encode_reason(maps:get(<<"reason">>, Modification))
            }};
        <<"ContractLegalAgreementBinding">> ->
            {legal_agreement_binding, encode_legal_agreement(maps:get(<<"legalAgreement">>, Modification))};
        % FIXME need swag supprot for template ref
        % <<"ContractAdjustmentCreation">> ->
        %     {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
        %         adjustment_id = maps:get(<<"adjustmentID">>, Modification),
        %         modification = {creation, #payproc_ContractAdjustmentParams{
        %             template = NOT_SUPPORTED
        %         }}
        %     }};
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

encode_legal_agreement(#{<<"id">> := ID, <<"signedAt">> := SignedAt}) ->
    #domain_LegalAgreement{
        signed_at = SignedAt,
        legal_agreement_id = ID
    }.

encode_payout_tool_params(#{
    <<"currency">> := Currency
} = Params) ->
    #payproc_PayoutToolParams{
        currency = encode_currency(Currency),
        tool_info = encode_payout_tool_info(Params)
    }.

encode_payout_tool_info(#{<<"payoutToolType">> := <<"PayoutToolBankAccount">>} = Tool) ->
   {bank_account, encode_bank_account(maps:get(<<"bankAccount">>, Tool))}.

encode_bank_account(#{
    <<"account">> := Account,
    <<"bankName">> := BankName,
    <<"bankPostAccount">> := BankPostAccount,
    <<"bankBik">> := BankBik
}) ->
    #domain_BankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
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
        bank_account = encode_bank_account(maps:get(<<"bankAccount">>, Entity))
    }}.

encode_registered_user(#{<<"email">> := Email}) ->
    #domain_RegisteredUser{email = Email}.

decode_bank_card(Encoded) ->
    #{
        <<"token">> := Token,
        <<"payment_system">> := PaymentSystem,
        <<"bin">> := Bin,
        <<"masked_pan">> := MaskedPan
    } = try capi_utils:base64url_to_map(Encoded)
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end,
    {bank_card, #domain_BankCard{
        'token'  = Token,
        'payment_system' = binary_to_existing_atom(PaymentSystem, utf8),
        'bin' = Bin,
        'masked_pan' = MaskedPan
    }}.

wrap_session(ClientInfo, PaymentSession) ->
    base64url:encode(jsx:encode(#{
        <<"clientInfo">> => ClientInfo,
        <<"paymentSession">> => PaymentSession
    })).

unwrap_session(Encoded) ->
    #{
        <<"clientInfo">> := ClientInfo,
        <<"paymentSession">> := PaymentSession
     } = try capi_utils:base64url_to_map(Encoded)
    catch
        error:badarg ->
            erlang:throw(invalid_payment_session)
    end,
    {ClientInfo, PaymentSession}.

remove_empty_events(DecodedEvents) when is_list(DecodedEvents)->
    lists:filter(
        fun
            (#{<<"changes">> := [_Something | _]}) ->
                true;
            (_) ->
                false
        end,
        DecodedEvents
    ).

decode_event(#payproc_Event{
    id = EventID,
    created_at = CreatedAt,
    payload =  {invoice_changes, InvoiceChanges},
    source =  {invoice_id, InvoiceID} %%@TODO deal with Party source
}) ->
    #{
        <<"id">> => EventID,
        <<"createdAt">> => CreatedAt,
        <<"changes">> => decode_invoice_changes(InvoiceID, InvoiceChanges)
    }.

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
        payload = {invoice_payment_session_interaction_requested,
            #payproc_InvoicePaymentSessionInteractionRequested{
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

decode_payment_change(_, _, _) ->
    undefined.

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{
    payment = Payment,
    % FIXME add to swag and decode adjustments
    adjustments = _Adjustments
}) ->
    decode_payment(InvoiceID, Payment).

decode_payment(InvoiceID, #domain_InvoicePayment{
    id = PaymentID,
    created_at = CreatedAt,
    status = Status,
    payer = #domain_Payer{
        payment_tool = PaymentTool,
        session_id = PaymentSession,
        contact_info = ContactInfo,
        client_info = #domain_ClientInfo{
            ip_address = IP,
            fingerprint = Fingerprint
        }
    },
    cost = #domain_Cash{
        amount = Amount,
        currency = Currency
    }
}) ->

    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"createdAt">> => CreatedAt,
        <<"amount">> => Amount,
        <<"currency">> => decode_currency(Currency),
        <<"contactInfo">> => decode_contact_info(ContactInfo),
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => decode_payment_tool_token(PaymentTool),
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"ip">> => IP,
        <<"fingerprint">> => Fingerprint
    }, decode_payment_status(Status))).

decode_payment_tool_token({bank_card, BankCard}) ->
    encode_bank_card(BankCard).

decode_payment_tool_details({bank_card, #domain_BankCard{
    'payment_system' = PaymentSystem,
    'masked_pan' = MaskedPan
}}) ->
    #{
        <<"detailsType">> => <<"PaymentToolDetailsCardData">>,
        <<"cardNumberMask">> => decode_masked_pan(MaskedPan),
        <<"paymentSystem">> => genlib:to_binary(PaymentSystem)
    }.

-define(MASKED_PAN_MAX_LENGTH, 4).

decode_masked_pan(MaskedPan) ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH}).

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
    logic_error(<<"500">>, <<"timeout">>);
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
    fee = Fee,
    currency_symbolic_code = Currency,
    payment_tool = PaymentTool,
    ip_address = IP,
    fingerprint = Fingerprint,
    phone_number = PhoneNumber,
    email = Email,
    session_id = PaymentSession,
    context = RawContext,
    location_info = Location
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"amount">> => Amount,
        <<"fee">> => Fee,
        <<"currency">> => Currency,
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email">> => Email
        }),
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"ip">> => IP,
        <<"geoLocationInfo">> => decode_geo_location_info(Location),
        <<"fingerprint">> => Fingerprint,
        <<"metadata">> =>  decode_context(RawContext)
    }, decode_stat_payment_status(Status))).

decode_stat_payment_tool_token(PaymentTool) ->
    decode_payment_tool_token(merchstat_to_domain(PaymentTool)).

decode_stat_payment_tool_details(PaymentTool) ->
    decode_payment_tool_details(merchstat_to_domain(PaymentTool)).

decode_stat_payment_status(PaymentStatus) ->
    decode_payment_status(merchstat_to_domain(PaymentStatus)).

merchstat_to_domain({bank_card, #merchstat_BankCard{
    'token'  = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan
}}) ->
    {bank_card, #domain_BankCard{
        'token'  = Token,
        'payment_system' = PaymentSystem,
        'bin' = Bin,
        'masked_pan' = MaskedPan
    }};

merchstat_to_domain({Status, #merchstat_InvoicePaymentPending{}}) ->
    {Status, #domain_InvoicePaymentPending{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentProcessed{}}) ->
    {Status, #domain_InvoicePaymentProcessed{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentCaptured{}}) ->
    {Status, #domain_InvoicePaymentCaptured{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentCancelled{}}) ->
    {Status, #domain_InvoicePaymentCancelled{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentFailed{
    failure = #merchstat_OperationFailure{
        code = Code,
        description = Description
    }
}}) ->
    {Status, #domain_InvoicePaymentFailed{
        failure = {external_failure, #domain_ExternalFailure{
            code = Code,
            description = Description
        }}
    }};

merchstat_to_domain({Status, #merchstat_InvoiceUnpaid{}}) ->
    {Status, #domain_InvoiceUnpaid{}};

merchstat_to_domain({Status, #merchstat_InvoicePaid{}}) ->
    {Status, #domain_InvoicePaid{}};

merchstat_to_domain({Status, #merchstat_InvoiceCancelled{details = Details}}) ->
    {Status, #domain_InvoiceCancelled{details = Details}};

merchstat_to_domain({Status, #merchstat_InvoiceFulfilled{details = Details}}) ->
    {Status, #domain_InvoiceFulfilled{details = Details}}.

decode_invoice(#domain_Invoice{
    id = InvoiceID,
    created_at = CreatedAt,
    status = InvoiceStatus,
    due  = DueDate,
    details = #domain_InvoiceDetails{
        product = Product,
        description = Description
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
        <<"description">> => Description
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

decode_stat_invoice(#merchstat_StatInvoice{
    id = InvoiceID,
    shop_id = ShopID,
    created_at = CreatedAt,
    status = InvoiceStatus,
    product = Product,
    description = Description,
    due  = DueDate,
    amount = Amount,
    currency_symbolic_code = Currency,
    context = RawContext
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
        <<"description">> => Description
    }, decode_stat_invoice_status(InvoiceStatus))).

decode_stat_invoice_status(Status) ->
    decode_invoice_status(merchstat_to_domain(Status)).

decode_context(#'Content'{
    type = <<"application/json">>,
    data = InvoiceContext
}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);

decode_context(undefined) ->
    undefined.

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
    contractor = Contractor,
    valid_since = ValidSince,
    valid_until = ValidUntil,
    status = Status0
}) ->
    Status = decode_contract_status(Status0),
    genlib_map:compact(maps:merge(#{
        <<"id">> => ContractID,
        <<"contractor">> => decode_contractor(Contractor),
        <<"validSince">> => ValidSince,
        <<"validUntil">> => ValidUntil
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
        <<"details">> => decode_payout_tool_info(Info)
    }.

decode_payout_tool_info({bank_account, BankAccount}) ->
    #{
        <<"payoutToolType">> => <<"PayoutToolBankAccount">>,
        <<"bankAccount">> => decode_bank_account(BankAccount)
    }.

decode_bank_account(#domain_BankAccount{
    account = Account,
    bank_name = BankName,
    bank_post_account = BankPostAccount,
    bank_bik = BankBik
}) ->
    #{
        <<"account">> => Account,
        <<"bankName">> => BankName,
        <<"bankPostAccount">> => BankPostAccount,
        <<"bankBik">> => BankBik
    }.

decode_shop(#domain_Shop{
    id = ShopID,
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
        bank_account = BankAccount
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
        <<"bankAccount">> => decode_bank_account(BankAccount)
    }.

decode_registered_user(#domain_RegisteredUser{email = Email}) ->
    #{<<"email">> => Email}.

is_blocked({blocked, _}) ->
    true;
is_blocked({unblocked, _}) ->
    false.

is_suspended({suspended, _}) ->
    true;
is_suspended({active, _}) ->
    false.

decode_stat_response(payments_conversion_stat, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"successfulCount">> => genlib:to_int(maps:get(<<"successful_count">>, Response)),
        <<"totalCount">> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"conversion">> => genlib:to_float(maps:get(<<"conversion">>, Response))
    };

decode_stat_response(payments_geo_stat, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"geoID">> => genlib:to_int(maps:get(<<"city_id">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_response(payments_turnover, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_response(customers_rate_stat, Response) ->
    #{
        <<"uniqueCount">> => genlib:to_int(maps:get(<<"unic_count">>, Response))
    };

decode_stat_response(payments_pmt_cards_stat, Response) ->
    #{
        <<"statType">> => <<"PaymentMethodBankCardStat">>, %% @TODO deal with nested responses decoding
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"totalCount">> =>  genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"paymentSystem">> =>  maps:get(<<"payment_system">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> =>  genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    }.

create_dsl(QueryType, QueryBody, QueryParams) when
    is_atom(QueryType),
    is_map(QueryBody),
    is_map(QueryParams) ->
    Query = maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{}),
    Basic = #{
        <<"query">> => Query
    },
    maps:merge(Basic, genlib_map:compact(QueryParams)).

filter_claims(undefined, Claims) ->
    Claims;
filter_claims(ClaimStatus, Claims) ->
    [Claim ||  Claim = #payproc_Claim{status = {Status, _}} <- Claims, Status =:= ClaimStatus].

decode_claims(Claims) ->
    lists:map(fun decode_claim/1, Claims).

decode_claim(#payproc_Claim{
    id = ID,
    revision = Revision,
    status = Status,
    changeset = ChangeSet
}) ->
    #{
        <<"id">> => ID,
        <<"revision">> => genlib:to_binary(Revision),
        <<"status">> => decode_claim_status(Status),
        <<"changeset">> => decode_party_changeset(ChangeSet)
    }.

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

decode_claim_status({'revoked', _}) ->
    #{
        <<"status">> => <<"ClaimRevoked">>
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
    contractor = Contractor
}}) ->
    #{
        <<"contractModificationType">> => <<"ContractCreation">>,
        <<"contractor">> => decode_contractor(Contractor)
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
    }.


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
        id = CategoryRef
    },
    data = #domain_Category{
        name = Name,
        description = Description
    }
}) ->
    genlib_map:compact(#{
        <<"name">> => Name,
        <<"categoryID">> => CategoryRef,
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

encode_event_filter(#{
    <<"topic">>      := <<"InvoicesTopic">>,
    <<"shopID">>     := ShopID,
    <<"eventTypes">> := EventTypes
}) ->
    {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ShopID,
        types   = ordsets:from_list([
            encode_event_type(invoices, V) || V <- EventTypes
        ])
    }}.

-define(invpaid()      , {paid, #webhooker_InvoicePaid{}}).
-define(invcancelled() , {cancelled, #webhooker_InvoiceCancelled{}}).
-define(invfulfilled() , {fulfilled, #webhooker_InvoiceFulfilled{}}).

-define(pmtprocessed() , {processed, #webhooker_InvoicePaymentProcessed{}}).
-define(pmtcaptured()  , {captured, #webhooker_InvoicePaymentCaptured{}}).
-define(pmtcancelled() , {cancelled, #webhooker_InvoicePaymentCancelled{}}).
-define(pmtfailed()    , {failed, #webhooker_InvoicePaymentFailed{}}).

encode_event_type(invoices, <<"InvoiceCreated">>) ->
    {created, #webhooker_InvoiceCreated{}};
encode_event_type(invoices, <<"InvoicePaid">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invpaid()}};
encode_event_type(invoices, <<"InvoiceCancelled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invcancelled()}};
encode_event_type(invoices, <<"InvoiceFulfilled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invfulfilled()}};
encode_event_type(invoices, <<"PaymentStarted">>) ->
    {payment, {created, #webhooker_InvoicePaymentCreated{}}};
encode_event_type(invoices, <<"PaymentProcessed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtprocessed()}}};
encode_event_type(invoices, <<"PaymentCaptured">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcaptured()}}};
encode_event_type(invoices, <<"PaymentCancelled">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcancelled()}}};
encode_event_type(invoices, <<"PaymentFailed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtfailed()}}}.

decode_event_filter({invoice, #webhooker_InvoiceEventFilter{
    shop_id = ShopID,
    types   = EventTypes
}}) ->
    #{
        <<"topic">>      => <<"InvoicesTopic">>,
        <<"shopID">>     => ShopID,
        <<"eventTypes">> => lists:flatmap(
            fun (V) -> decode_event_type(invoice, V) end, ordsets:to_list(EventTypes)
        )
    }.

decode_event_type(
    invoice,
    {created, #webhooker_InvoiceCreated{}}
) ->
    [<<"InvoiceCreated">>];
decode_event_type(
    invoice,
    {status_changed, #webhooker_InvoiceStatusChanged{value = undefined}}
) ->
    % TODO seems unmaintainable
    [decode_invoice_status_event_type(V) || V <- [
        ?invpaid(),
        ?invcancelled(),
        ?invfulfilled()
    ]];
decode_event_type(
    invoice,
    {status_changed, #webhooker_InvoiceStatusChanged{value = Value}}
) ->
    [decode_invoice_status_event_type(Value)];
decode_event_type(
    invoice,
    {payment, {created, #webhooker_InvoicePaymentCreated{}}}
) ->
    [<<"PaymentStarted">>];
decode_event_type(
    invoice,
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = undefined}}}
) ->
    % TODO seems unmaintainable
    [decode_payment_status_event_type(V) || V <- [
        ?pmtprocessed(),
        ?pmtcaptured(),
        ?pmtcancelled(),
        ?pmtfailed()
    ]];
decode_event_type(
    invoice,
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = Value}}}
) ->
    [decode_payment_status_event_type(Value)].

decode_invoice_status_event_type(?invpaid())      -> <<"InvoicePaid">>;
decode_invoice_status_event_type(?invcancelled()) -> <<"InvoiceCancelled">>;
decode_invoice_status_event_type(?invfulfilled()) -> <<"InvoiceFulfilled">>.

decode_payment_status_event_type(?pmtprocessed()) -> <<"PaymentProcessed">>;
decode_payment_status_event_type(?pmtcaptured())  -> <<"PaymentCaptured">>;
decode_payment_status_event_type(?pmtcancelled()) -> <<"PaymentCancelled">>;
decode_payment_status_event_type(?pmtfailed())    -> <<"PaymentFailed">>.

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
            decode_stat_response(StatType, StatResponse)
    end,
    {ok, {200, [], Resp}};

process_merchant_stat_result(StatType, Result) ->
    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
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
    to_universal_time(genlib_map:get(Key, Req)).

to_universal_time(Tz = undefined) ->
    Tz;
to_universal_time(Tz) ->
    {ok, {Date, Time, Usec, TzOffset}} = rfc3339:parse(Tz),
    TzSec = calendar:datetime_to_gregorian_seconds({Date, Time}),
    %% The following crappy code is a dialyzer workaround
    %% for the wrong rfc3339:parse/1 spec.
    {UtcDate, UtcTime} = calendar:gregorian_seconds_to_datetime(
        case TzOffset of
            _ when is_integer(TzOffset) ->
                TzSec - (60*TzOffset);
            _ ->
                TzSec
        end),
    {ok, Utc} = rfc3339:format({UtcDate, UtcTime, Usec, 0}),
    Utc.

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

process_woody_error(_, external, resource_unavailable, _Details) ->
    {error, reply_5xx(503)};

process_woody_error(_, external, result_unexpected, _Details) ->
    {error, reply_5xx(500)};

process_woody_error(_, external, result_unknown, _Details) ->
    {error, reply_5xx(500)}.


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

collect_events(UserInfo, InvoiceID, Limit, After, ReqCtx) ->
    Context = #{
        invoice_id => InvoiceID,
        user_info => UserInfo,
        request_context => ReqCtx
    },
    get_events(Limit, After, Context).

get_events(Limit, After, Context) ->
    EventRange = #'payproc_EventRange'{
        limit = Limit,
        'after' = After
    },
    service_call(
        invoicing,
        'GetEvents',
        [
            maps:get(user_info, Context),
            maps:get(invoice_id, Context),
            EventRange
        ],
        maps:get(request_context, Context)
    ).

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, [], <<>>}.

process_card_data(ClientInfo0, PaymentTool, Context, ReqCtx) ->
    CardData = get_card_data(PaymentTool),
    Result = service_call(
        cds_storage,
        'PutCardData',
        [CardData],
        ReqCtx
    ),
    case Result of
        {ok, #'PutCardDataResult'{
            session_id = PaymentSession,
            bank_card = BankCard
        }} ->
            Token = encode_bank_card(BankCard),
            PreparedIP = get_prepared_ip(Context),
            ClientInfo = ClientInfo0#{<<"ip_address">> => PreparedIP},

            Session = wrap_session(ClientInfo, PaymentSession),
            Resp = #{
                <<"token">> => Token,
                <<"session">> => Session
            },
            {ok, {201, [], Resp}};
        {exception, Exception} ->
            case Exception of
                #'InvalidCardData'{} ->
                    {ok, {400, [], logic_error(invalidRequest, <<"Card data is invalid">>)}};
                #'KeyringLocked'{} ->
                    {error, reply_5xx(503)}
            end
    end.

get_card_data(PaymentTool) ->
    {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, PaymentTool)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, PaymentTool)),
    #'CardData'{
        pan  = CardNumber,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = genlib_map:get(<<"cardHolder">>, PaymentTool),
        cvv = genlib_map:get(<<"cvv">>, PaymentTool)
    }.

get_prepared_ip(Context) ->
    #{
        ip_address := IP
    } = get_peer_info(Context),
    genlib:to_binary(inet:ntoa(IP)).

% format_now() ->
%     case rfc3339:format(erlang:system_time()) of
%         {ok, Timestamp} ->
%             Timestamp;
%         {error, Error} ->
%             error(Error)
%     end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.
to_universal_time_test() ->
    ?assertEqual(undefined,                         to_universal_time(undefined)),
    ?assertEqual(<<"2017-04-19T13:56:07Z">>,        to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-endif. %%TEST
