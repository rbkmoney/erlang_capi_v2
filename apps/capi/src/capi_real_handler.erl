-module(capi_real_handler).

-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_cds_thrift.hrl").
-include_lib("cp_proto/include/cp_merch_stat_thrift.hrl").
-include_lib("cp_proto/include/cp_proxy_merch_config_thrift.hrl").
-include_lib("cp_proto/include/cp_user_interaction_thrift.hrl").
-include_lib("cp_proto/include/cp_geo_ip_thrift.hrl").

-behaviour(swagger_logic_handler).

%% API callbacks
-export([handle_request/3]).
-export([authorize_api_key/2]).

-spec authorize_api_key(OperationID :: swagger_api:operation_id(), ApiKey :: binary()) ->
    Result :: false | {true, #{binary() => any()}}.

authorize_api_key(OperationID, ApiKey) ->
    capi_auth:auth_api_key(OperationID, ApiKey).

-spec handle_request(
    OperationID :: swagger_api:operation_id(),
    Req :: #{},
    Context :: swagger_api:request_context()
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

handle_request(OperationID, Req, Context) ->
    capi_utils:logtag_process(operation_id, OperationID),
    _ = lager:info("Processing request ~p", [OperationID]),
    ReqContext = create_context(Req),
    try
        process_request(OperationID, Req, Context, ReqContext)
    catch
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(OperationID, Source, Class, Details)
    end.

-spec process_request(
    OperationID :: swagger_api:operation_id(),
    Req :: #{},
    Context :: swagger_api:request_context(),
    ReqCtx :: woody_context:ctx()
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request(OperationID = 'CreateInvoice', Req, Context, ReqCtx) ->
    InvoiceParams = maps:get('InvoiceParams', Req),
    InvoiceContext = jsx:encode(genlib_map:get(<<"metadata">>, InvoiceParams)),
    PartyID = get_party_id(Context),
    Params =  #payproc_InvoiceParams{
        party_id = PartyID,
        details = encode_invoice_details(InvoiceParams),
        cost = encode_cash(InvoiceParams),
        due      = genlib_map:get(<<"dueDate">>, InvoiceParams),
        context  = #'Content'{
            type = <<"application/json">>,
            data = InvoiceContext
        },
        shop_id = genlib_map:get(<<"shopID">>, InvoiceParams)
    },
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
        {ok, InvoiceID} ->
            {ok, #'payproc_InvoiceState'{invoice = Invoice}} = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
            {201, [], decode_invoice(Invoice)};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'CreatePayment', Req, Context, ReqCtx) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    Token = genlib_map:get(<<"paymentToolToken">>, PaymentParams),
    ContactInfo = genlib_map:get(<<"contactInfo">>, PaymentParams),
    PaymentTool = decode_bank_card(Token),

    EncodedSession = genlib_map:get(<<"paymentSession">>, PaymentParams),
    {ClientInfo, PaymentSession} = unwrap_session(EncodedSession),
    Params =  #payproc_InvoicePaymentParams{
        'payer' = #domain_Payer{
            payment_tool = PaymentTool,
            session = PaymentSession,
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
    UserInfo = get_user_info(Context),
    Result = service_call(
        invoicing,
        'StartPayment',
        [UserInfo, InvoiceID, Params],
        ReqCtx
    ),
    case Result of
        {ok, PaymentID} ->
            {ok, Payment} = get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID),
            Resp = decode_payment(InvoiceID, Payment),
            {201, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'CreatePaymentToolToken', Req, Context, ReqCtx) ->
    Params = maps:get('PaymentToolTokenParams', Req),
    ClientInfo0 = maps:get(<<"clientInfo">>, Params),
    PaymentTool = maps:get(<<"paymentTool">>, Params),
    case PaymentTool of
        #{<<"paymentToolType">> := <<"CardData">>} ->
            {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, PaymentTool)),
            CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, PaymentTool)),
            CardData = #'CardData'{
                pan  = CardNumber,
                exp_date = #'ExpDate'{
                    month = Month,
                    year = Year
                },
                cardholder_name = genlib_map:get(<<"cardHolder">>, PaymentTool),
                cvv = genlib_map:get(<<"cvv">>, PaymentTool)
            },
            Result = service_call(
                cds_storage,
                'PutCardData',
                [CardData],
                ReqCtx
            ),
            case Result of
                {ok, #'PutCardDataResult'{
                    session = PaymentSession,
                    bank_card = BankCard
                }} ->
                    Token = encode_bank_card(BankCard),
                    #{
                        ip_address := IP
                    } = get_peer_info(Context),
                    PreparedIP = genlib:to_binary(inet:ntoa(IP)),
                    ClientInfo = ClientInfo0#{<<"ip_address">> => PreparedIP},

                    Session = wrap_session(ClientInfo, PaymentSession),
                    Resp = #{
                        <<"token">> => Token,
                        <<"session">> => Session
                    },
                    {201, [], Resp};
                {exception, Exception} ->
                    process_exception(OperationID, Exception)
            end;
        _ ->
            {400, [], logic_error(
                invalidPaymentTool,
                <<"Specified payment tool is invalid or unsupported">>
            )}
    end;

process_request(OperationID = 'GetInvoiceByID', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_invoice_by_id(ReqCtx, UserInfo, InvoiceID),
    case Result of
        {ok, #'payproc_InvoiceState'{invoice = Invoice}} ->
            Resp = decode_invoice(Invoice),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'FulfillInvoice', Req, Context, ReqCtx) ->
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
            {204, [], <<>>}; %%LIAR!
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'RescindInvoice', Req, Context, ReqCtx) ->
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
            {200, [], #{}};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetInvoiceEvents', Req, Context, ReqCtx) ->
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    EventRange = #'payproc_EventRange'{
        limit = maps:get(limit, Req),
        'after' = maps:get(eventID, Req)
    },
    Result = service_call(
        invoicing,
        'GetEvents',
        [UserInfo, InvoiceID, EventRange],
        ReqCtx
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            Resp = [decode_event(I) || I <- Events],
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetPaymentByID', Req, Context, ReqCtx) ->
    PaymentID = maps:get(paymentID, Req),
    InvoiceID = maps:get(invoiceID, Req),
    UserInfo = get_user_info(Context),
    Result = get_payment_by_id(ReqCtx, UserInfo, InvoiceID, PaymentID),
    case Result of
        {ok, Payment} ->
            Resp = decode_payment(InvoiceID, Payment),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetInvoices', Req, Context, ReqCtx) ->
    Limit = genlib_map:get('limit', Req),
    Offset = genlib_map:get('offset', Req),
    InvoiceStatus = case genlib_map:get('status', Req) of
        undefined -> undefined;
        [Status | _] -> Status
    end,  %%@TODO deal with many statuses
    Query = #{
        <<"merchant_id">> => get_party_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"invoice_id">> =>  genlib_map:get('invoiceID', Req),
        <<"from_time">> => genlib_map:get('fromTime', Req),
        <<"to_time">> => genlib_map:get('toTime', Req),
        <<"invoice_status">> => InvoiceStatus
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
            DecodedInvoices = [decode_invoice(I) || #merchstat_StatInvoice{invoice = I} <- Invoices],
            Resp = #{
                <<"invoices">> => DecodedInvoices,
                <<"totalCount">> => TotalCount
            },
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetPaymentConversionStats', Req, Context, ReqCtx) ->
    StatType = payments_conversion_stat,
    Result = call_merchant_stat(StatType, Req, Context, ReqCtx),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;


process_request(OperationID = 'GetPaymentRevenueStats', Req, Context, ReqCtx) ->
    StatType = payments_turnover,
    Result = call_merchant_stat(StatType, Req, Context, ReqCtx),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetPaymentGeoStats', Req, Context, ReqCtx) ->
    StatType = payments_geo_stat,
    Result = call_merchant_stat(StatType, Req, Context, ReqCtx),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetPaymentRateStats', Req, Context, ReqCtx) ->
    StatType = customers_rate_stat,
    Result = call_merchant_stat(StatType, Req, Context, ReqCtx),

    case Result of
        {ok, #merchstat_StatResponse{data = {records, S}}} ->
            Resp = case S of
                [] ->
                    #{
                        <<"uniqueCount">> => 0
                    };
                [StatResponse] ->
                    decode_stat_response(StatType, StatResponse)
            end,
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetPaymentMethodStats', Req, Context, ReqCtx) ->
    case maps:get(paymentMethod, Req) of
        bankCard ->
            StatType = payments_pmt_cards_stat,
            Result = call_merchant_stat(StatType, Req, Context, ReqCtx),
            case Result of
                {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
                    Resp = [decode_stat_response(StatType, S) || S <- Stats],
                    {200, [], Resp};
                {exception, Exception} ->
                    process_exception(OperationID, Exception)
            end
    end;

process_request(OperationID = 'GetLocationsNames', Req, _Context, ReqCtx) ->
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
            {200, [], PreparedLocationNames};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'CreateShop', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Params = maps:get('ShopParams', Req),

    Proxy = populate_proxy_options(genlib_map:get(<<"callbackUrl">>, Params), ReqCtx),

    ShopParams = #payproc_ShopParams{
        category =  encode_category_ref(genlib_map:get(<<"categoryID">>, Params)),
        details = encode_shop_details(genlib_map:get(<<"details">>, Params)),
        contract_id = genlib_map:get(<<"contractID">>, Params),
        payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params),
        proxy = Proxy
    },

    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'CreateShop',
                [UserInfo, PartyID, ShopParams],
                ReqCtx
            )
        end
    ),

    case Result of
        {ok, R} ->
            {202, [], decode_claim_result(R)};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'ActivateShop', Req, Context, ReqCtx) ->
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
            {204, [], <<>>};
        {exception, #payproc_InvalidShopStatus{
            status = {suspension, {active, _}}
        }} ->
            {204, [], <<>>};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'SuspendShop', Req, Context, ReqCtx) ->
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
            {204, [], <<>>};
        {exception, #payproc_InvalidShopStatus{
            status = {suspension, {suspended, _}}
        }} ->
            {204, [], <<>>};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'UpdateShop', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    Params = maps:get('UpdateShopParams', Req),

    Proxy = populate_proxy_options(genlib_map:get(<<"callbackUrl">>, Params), ReqCtx),

    ShopUpdate = #payproc_ShopUpdate{
        category = encode_category_ref(genlib_map:get(<<"categoryID">>, Params)),
        details =  encode_shop_details(genlib_map:get(<<"details">>, Params)),
        contract_id = genlib_map:get(<<"contractID">>, Params),
        payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params),
        proxy = Proxy
    },

    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'UpdateShop',
                [UserInfo, PartyID, ShopID, ShopUpdate],
                ReqCtx
            )
        end
    ),

    case Result of
        {ok, R} ->
            {202, [], decode_claim_result(R)};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetShops', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    case Result of
        {ok, #domain_Party{shops = Shops}} ->
            Resp = decode_shops_map(Shops, ReqCtx),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetShopByID', Req, Context, ReqCtx) ->
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
            Resp = decode_shop(Shop, ReqCtx),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetContracts', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = get_my_party(Context, ReqCtx, UserInfo, PartyID),

    case Result of
        {ok, #domain_Party{
            contracts = Contracts
        }} ->
            Resp = decode_contracts_map(Contracts),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'CreateContract', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Params = maps:get('ContractParams', Req),

    ContractParams = #payproc_ContractParams{
        contractor = encode_contractor(genlib_map:get(<<"contractor">>, Params)),
        payout_tool_params = encode_payout_tool_params(genlib_map:get(<<"payoutToolParams">>, Params))
    },
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'CreateContract',
                [UserInfo, PartyID, ContractParams],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, R} ->
            {202, [], decode_claim_result(R)};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetContractByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, Contract} ->
            Resp = decode_contract(Contract),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetPayoutTools', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),

    Result = get_contract_by_id(Context, ReqCtx, UserInfo, PartyID, ContractID),
    case Result of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            Resp = [decode_payout_tool(P) || P <- PayoutTools],
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'CreatePayoutTool', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ContractID = maps:get('contractID', Req),
    Params = maps:get('PayoutToolParams', Req),

    PayoutToolParams = encode_payout_tool_params(Params),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'CreatePayoutTool',
                [UserInfo, PartyID, ContractID, PayoutToolParams],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, R} ->
            {202, [], decode_claim_result(R)};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetClaimsByStatus', Req, Context, ReqCtx) ->
    pending = maps:get(claimStatus, Req), %% @TODO think about other claim statuses here
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),

    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetPendingClaim',
                [UserInfo, PartyID],
                ReqCtx
            )
        end
    ),
    case Result of
        {exception, #payproc_ClaimNotFound{}} ->
            {200, [], []};
        {ok, Claim} ->
            Resp = decode_claim(Claim, ReqCtx),
            {200, [], [Resp]}; %% pretending to have more than one pending claim at the same time
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetClaimByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimID = maps:get(claimID, Req),
    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'GetClaim',
                [UserInfo, PartyID, ClaimID],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, Claim} ->
            Resp = decode_claim(Claim, ReqCtx),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'RevokeClaimByID', Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    ClaimID = maps:get(claimID, Req),
    Params = maps:get('Reason', Req),
    Reason = maps:get(<<"reason">>, Params),

    Result = prepare_party(
        Context,
        ReqCtx,
        fun () ->
            service_call(
                party_management,
                'RevokeClaim',
                [UserInfo, PartyID, ClaimID, Reason],
                ReqCtx
            )
        end
    ),
    case Result of
        {ok, _} ->
            {200, [], #{}};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'SuspendMyParty', _Req, Context, ReqCtx) ->
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
            {204, [], <<>>};
        {exception, #payproc_InvalidPartyStatus{
            status = {suspension, {suspended, _}}
        }} ->
            {204, [], <<>>};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'ActivateMyParty', _Req, Context, ReqCtx) ->
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
            {204, [], <<>>};
        {exception, #payproc_InvalidPartyStatus{
            status = {suspension, {active, _}}
        }} ->
            {204, [], <<>>};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(OperationID = 'GetMyParty', _Req, Context, ReqCtx) ->
    UserInfo = get_user_info(Context),
    PartyID = get_party_id(Context),
    Result = get_my_party(Context, ReqCtx, UserInfo, PartyID),
    case Result of
        {ok, Party} ->
            Resp = decode_party(Party),
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(_OperationID = 'GetCategories', _Req, Context, ReqCtx) ->
    _ = get_user_info(Context),
    _ = get_party_id(Context),
    {ok, Categories} = capi_domain:get_categories(ReqCtx),
    Resp = [decode_category(C) || C <- Categories],
    {200, [], Resp};

process_request(_OperationID = 'GetCategoryByRef', Req, Context0, ReqCtx) ->
    _ = get_user_info(Context0),
    _ = get_party_id(Context0),
    CategoryID = maps:get(categoryID, Req),
    case get_category_by_id(genlib:to_int(CategoryID), ReqCtx) of
        {ok, Category} ->
            Resp = decode_category(Category),
            {200, [], Resp};
        {error, not_found} ->
            {404, [], general_error(<<"Category not found">>)}
    end;

process_request(OperationID = 'GetAccountByID', Req, Context, ReqCtx) ->
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
            {200, [], Resp};
        {exception, Exception} ->
            process_exception(OperationID, Exception)
    end;

process_request(_OperationID, _Req, _Context, _ReqCtx) ->
    {501, [], <<"Not implemented">>}.

%%%

service_call(ServiceName, Function, Args, Context) ->
    cp_proto:call_service(ServiceName, Function, Args, Context, capi_woody_event_handler).

create_context(Req) ->
    RequestID = maps:get('X-Request-ID', Req),
    woody_context:new(genlib:to_binary(RequestID)).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

limit_exceeded_error(Limit) ->
    logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit])).

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month,  Year] = binary:split(ExpDate, <<"/">>),
    {genlib:to_int(Month), 2000 + genlib:to_int(Year)}.

get_user_info(Context) ->
    #payproc_UserInfo{
        id = get_party_id(Context),
        type = {external_user, #payproc_ExternalUser{}}
    }.

get_party_id(#{
    auth_context := AuthContext
}) ->
    maps:get(<<"sub">>, AuthContext).

get_party_params(#{
    auth_context := AuthContext
}) ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = maps:get(<<"email">>, AuthContext)
        }
    }.

get_peer_info(#{peer := Peer}) -> Peer.

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

encode_shop_details(undefined) ->
    undefined;

encode_shop_details(Details = #{
    <<"name">> := Name
}) ->
    #domain_ShopDetails{
        name = Name,
        description = genlib_map:get(<<"description">>, Details),
        location = encode_shop_location(genlib_map:get(<<"location">>, Details))
    }.

encode_shop_location(undefined) ->
    undefined;

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

encode_contractor(undefined) ->
    undefined;

encode_contractor(#{
    <<"bankAccount">> := BankAccount,
    <<"legalEntity">> := Entity
}) ->
    #domain_Contractor{
        entity = encode_legal_entity(Entity),
        bank_account = encode_bank_account(BankAccount)
    }.

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
        representative_document = maps:get(<<"representativeDocument">>, Entity)
    }}.

decode_bank_card(Encoded) ->
    #{
        <<"token">> := Token,
        <<"payment_system">> := PaymentSystem,
        <<"bin">> := Bin,
        <<"masked_pan">> := MaskedPan
    } = jsx:decode(base64url:decode(Encoded), [return_maps]),
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
    } = jsx:decode(base64url:decode(Encoded), [return_maps]),
    {ClientInfo, PaymentSession}.

decode_event(#'payproc_Event'{
    'id' = EventID,
    'created_at' = CreatedAt,
    'payload' =  {'invoice_event', InvoiceEvent},
    'source' =  {'invoice', InvoiceID} %%@TODO deal with Party source
}) ->
    {EventType, EventBody} = decode_invoice_event(InvoiceID, InvoiceEvent),
    maps:merge(#{
        <<"id">> => EventID,
        <<"createdAt">> => CreatedAt,
        <<"eventType">> => EventType
    }, EventBody).

decode_invoice_event(_, {
    invoice_created,
    #payproc_InvoiceCreated{invoice = Invoice}
}) ->
    {<<"EventInvoiceCreated">>, #{
        <<"invoice">> => decode_invoice(Invoice)
    }};

decode_invoice_event(_, {
    invoice_status_changed,
    #payproc_InvoiceStatusChanged{status = {Status, _}}
}) ->
    {<<"EventInvoiceStatusChanged">>, #{
        <<"status">> => genlib:to_binary(Status)
    }};

decode_invoice_event(InvoiceID, {invoice_payment_event, Event}) ->
    decode_payment_event(InvoiceID, Event).

decode_payment_event(InvoiceID, {
    invoice_payment_started,
    #'payproc_InvoicePaymentStarted'{payment = Payment}
}) ->
    {<<"EventPaymentStarted">>, #{
        <<"payment">> => decode_payment(InvoiceID, Payment)
    }};

decode_payment_event(_, {
    invoice_payment_bound,
    #'payproc_InvoicePaymentBound'{payment_id = PaymentID}
}) ->
    {<<"EventPaymentBound">>, #{
        <<"paymentID">> => PaymentID
    }};

decode_payment_event(_, {
    invoice_payment_interaction_requested,
    #'payproc_InvoicePaymentInteractionRequested'{
        payment_id = PaymentID,
        interaction = Interaction
    }
}) ->
    {<<"EventInvoicePaymentInteractionRequested">>, #{
        <<"paymentID">> => PaymentID,
        <<"userInteraction">> => decode_user_interaction(Interaction)
    }};

decode_payment_event(_, {
    invoice_payment_status_changed,
    #'payproc_InvoicePaymentStatusChanged'{payment_id = PaymentID, status = {Status, _}}
}) ->
    {<<"EventPaymentStatusChanged">>, #{
        <<"paymentID">> => PaymentID,
        <<"status">> => genlib:to_binary(Status)
    }}.

decode_payment(InvoiceID, #domain_InvoicePayment{
    id = PaymentID,
    created_at = CreatedAt,
    status = Status,
    payer = #domain_Payer{
        payment_tool = PaymentTool,
        session = PaymentSession,
        contact_info = ContactInfo
    }
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"createdAt">> => CreatedAt,
        <<"paymentToolToken">> => decode_payment_tool_token(PaymentTool),
        <<"contactInfo">> => decode_contact_info(ContactInfo),
        <<"paymentSession">> => PaymentSession
    }, decode_payment_status(Status))).

decode_payment_tool_token({bank_card, BankCard}) ->
    encode_bank_card(BankCard).

%% @TODO deal with an empty map here
decode_contact_info(#domain_ContactInfo{
    phone_number = PhoneNumber,
    email = Email
}) ->
    #{
        <<"phone_number">> => PhoneNumber,
        <<"email">> => Email
    }.

decode_payment_status({Status, StatusInfo}) ->
    Error = case StatusInfo of
        #domain_InvoicePaymentFailed{
            failure = #domain_OperationFailure{
                code = Code,
                description = Description
            }
        } ->
            #{
                <<"code">> => Code,
                <<"description">> => Description
            };
        _ ->
            undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_invoice(#domain_Invoice{
    id = InvoiceID,
    created_at = CreatedAt, %%@TODO add it to the swagger spec
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

decode_context(#'Content'{
    type = <<"application/json">>,
    data = InvoiceContext
}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]).

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

decode_shops_map(Shops, ReqCtx) ->
    decode_map(Shops, fun (S) -> decode_shop(S, ReqCtx) end).

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
    #{
        <<"id">> => ID,
        <<"params">> => decode_payout_tool_params(Currency, Info)
    }.

decode_payout_tool_params(Currency, Info) ->
    Basic = #{
        <<"currency">> => decode_currency(Currency)
    },
    maps:merge(Basic, decode_payout_tool_info(Info)).

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
    account = ShopAccount,
    contract_id = ContractID,
    payout_tool_id = PayoutToolID,
    proxy = Proxy
}, ReqCtx) ->
    genlib_map:compact(#{
        <<"id">> => ShopID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension),
        <<"categoryID">> => CategoryRef,
        <<"details">> => decode_shop_details(ShopDetails),
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID,
        <<"account">> => decode_shop_account(ShopAccount),
        <<"callbackHandler">> => decode_callback_handler(get_proxy_options(Proxy, ReqCtx))
    }).

decode_callback_handler(undefined) ->
    undefined;

decode_callback_handler(#proxy_merch_config_MerchantProxyConfiguration{
    callback_url  = CallbackUrl,
    pub_key = PubKey
}) ->
    #{
        <<"url">> => CallbackUrl,
        <<"publicKey">> => PubKey
    }.

decode_shop_details(undefined) ->
    undefined;

decode_shop_details(#domain_ShopDetails{
    name = Name,
    description = Description,
    location = Location
}) ->
    genlib_map:compact(#{
      <<"name">> => Name,
      <<"description">> => Description,
      <<"location">> => decode_shop_location(Location)
    }).

decode_shop_location(undefined) ->
    undefined;

decode_shop_location({url, Location}) ->
    #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => Location
    }.

decode_contractor(undefined) ->
    undefined;

decode_contractor(#domain_Contractor{
    bank_account = BankAccount,
    entity = LegalEntity
}) ->
    #{
        <<"bankAccount">> => decode_bank_account(BankAccount),
        <<"legalEntity">> => decode_legal_entity(LegalEntity)
    }.

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
        representative_document = RepresentativeDocument
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
        <<"representativeDocument">> => RepresentativeDocument
    }.

is_blocked({blocked, _}) ->
    true;
is_blocked({unblocked, _}) ->
    false.

is_suspended({suspended, _}) ->
    true;
is_suspended({active, _}) ->
    false.

decode_suspension({suspended, _}) ->
    #{<<"suspensionType">> => <<"suspended">>};

decode_suspension({active, _}) ->
    #{<<"suspensionType">> => <<"active">>}.

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
        <<"geoID">> => genlib:to_int(maps:get(<<"geoID">>, Response)),
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

decode_claim_result(#payproc_ClaimResult{id = ClaimID}) ->
    #{<<"claimID">> => ClaimID}.

decode_claim(#payproc_Claim{
    id = ID,
    status = Status,
    changeset = ChangeSet
}, ReqCtx) ->
    #{
        <<"id">> => ID,
        <<"status">> => decode_claim_status(Status),
        <<"changeset">> => decode_party_changeset(ChangeSet, ReqCtx)
    }.

decode_claim_status({'pending', _}) ->
    #{
        <<"status">> => <<"ClaimPending">>
    };
decode_claim_status({'accepted', _}) ->
    #{
        <<"status">> =><<"ClaimAccepted">>
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
        <<"status">> =><<"ClaimRevoked">>
    }.

decode_party_changeset(PartyChangeset, ReqCtx) ->
    [decode_party_modification(PartyModification, ReqCtx) || PartyModification <- PartyChangeset].

decode_party_modification({suspension, Suspension}, _ReqCtx) ->
    #{
        <<"partyModificationType">> => <<"PartySuspension">>,
        <<"details">> => decode_suspension(Suspension)
    };

decode_party_modification({contract_creation, Contract}, _ReqCtx) ->
    #{
        <<"partyModificationType">> => <<"ContractCreation">>,
        <<"contract">> => decode_contract(Contract)
    };

decode_party_modification({
    contract_modification,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = Modification
    }
}, _ReqCtx) ->
    maps:merge(#{
        <<"partyModificationType">> => <<"ContractModification">>,
        <<"contractID">> => ContractID
    }, decode_contract_modification(Modification));

decode_party_modification({shop_creation, Shop}, ReqCtx) ->
    #{
        <<"partyModificationType">> => <<"ShopCreation">>,
        <<"shop">> => decode_shop(Shop, ReqCtx)
    };

decode_party_modification({
    shop_modification,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = ShopModification
    }
}, ReqCtx) ->
    maps:merge(#{
        <<"partyModificationType">> => <<"ShopModification">>,
        <<"shopID">> => ShopID
    }, decode_shop_modification(ShopModification, ReqCtx)).

decode_contract_modification({termination, #payproc_ContractTermination{
    terminated_at = TerminatedAt,
    reason = Reason
}}) ->
    genlib_map:compact(#{
        <<"contractModificationType">> => <<"ContractTermination">>,
        <<"terminatedAt">> => TerminatedAt,
        <<"reason">> => Reason
    });

decode_contract_modification({payout_tool_creation, PayoutTool}) ->
    #{
        <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
        <<"payoutTool">> => decode_payout_tool(PayoutTool)
    };

decode_contract_modification(_) ->
    #{}. %% Fiding adjustments and legal agreements


decode_shop_modification({suspension, Suspension}, _ReqCtx) ->
    #{
        <<"shopModificationType">> => <<"ShopSuspension">>,
        <<"details">> => decode_suspension(Suspension)
    };

decode_shop_modification({
    update,
    #payproc_ShopUpdate{
        category = Category,
        details = Details,
        contract_id = ContractID,
        payout_tool_id = PayoutToolID,
        proxy = Proxy
    }
}, ReqCtx) ->
    #{
        <<"shopModificationType">> => <<"ShopUpdate">>,
        <<"details">> => genlib_map:compact(#{
            <<"categoryID">> => decode_category_ref(Category),
            <<"details">> => decode_shop_details(Details),
            <<"contractID">> => ContractID,
            <<"payoutToolID">> => PayoutToolID,
            <<"callbackHandler">> => decode_callback_handler(get_proxy_options(Proxy, ReqCtx))
        })
    };

decode_shop_modification({
    account_created,
    #payproc_ShopAccountCreated{
        account = Account
    }
}, _ReqCtx) ->
    #{
        <<"shopModificationType">> => <<"ShopAccountCreation">>,
        <<"account">> => decode_shop_account(Account)
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

decode_category_ref(undefined) ->
    undefined;

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
        <<"guaranteeID">> => genlib:to_binary(GuaranteeID),
        <<"settlementID">> => genlib:to_binary(SettlementID),
        <<"currency">> => decode_currency(Currency)
    }.

decode_account_state(#payproc_AccountState{
    account_id = AccountID,
    own_amount = OwnAmount,
    available_amount = AvailableAmount,
    currency = Currency
}) ->
    #{
        <<"id">> => genlib:to_binary(AccountID),
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

encode_stat_request(Dsl) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl));

encode_stat_request(Dsl) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl
    }.

create_stat_dsl(StatType, Req, Context) ->
    FromTime = genlib_map:get('fromTime', Req),
    ToTime = genlib_map:get('toTime', Req),
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

call_merchant_stat(StatType, Req, Context, ReqCtx) ->
    Dsl = create_stat_dsl(StatType, Req, Context),
    service_call(
        merchant_stat,
        'GetStatistics',
        [encode_stat_request(Dsl)],
        ReqCtx
    ).

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

process_exception(_, #payproc_InvalidUser{}) ->
    {400, [], logic_error(invalidUser, <<"Ivalid user">>)};

process_exception(_, #'InvalidRequest'{errors = Errors}) ->
    {400, [], logic_error(invalidRequest, format_request_errors(Errors))};

process_exception(_, #payproc_UserInvoiceNotFound{}) ->
    {404, [], general_error(<<"Invoice not found">>)};

process_exception(_, #payproc_ClaimNotFound{}) ->
    {404, [], general_error(<<"Claim not found">>)};

process_exception(_, #payproc_ContractNotFound{}) ->
    {404, [], general_error(<<"Contract not found">>)};

process_exception(_,  #payproc_InvalidInvoiceStatus{}) ->
    {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)};

process_exception(_,  #payproc_InvalidPartyStatus{}) ->
    {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)};

process_exception(_, #payproc_InvoicePaymentPending{}) ->
    {400, [], logic_error(invalidPaymentStatus, <<"Invalid payment status">>)};

process_exception(_, #payproc_InvalidShopStatus{}) ->
    {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)};

process_exception(_, #payproc_InvalidContractStatus{}) ->
    {400, [], logic_error(invalidContractStatus, <<"Invalid contract status">>)};

process_exception(_, #payproc_PartyNotFound{}) ->
    {404, [],  general_error(<<"Party not found">>)};

process_exception(_,  #'InvalidCardData'{}) ->
    {400, [], logic_error(invalidRequest, <<"Card data is invalid">>)};

process_exception(_, #'KeyringLocked'{}) ->
    {503, [], <<"">>};

process_exception(_, #payproc_EventNotFound{}) ->
    {404, [], general_error(<<"Event not found">>)};

process_exception(_, #payproc_ShopNotFound{}) ->
    {404, [], general_error(<<"Shop not found">>)};

process_exception(_, #payproc_InvoicePaymentNotFound{}) ->
    {404, [], general_error(<<"Payment not found">>)};

process_exception(_, #merchstat_DatasetTooBig{limit = Limit}) ->
    {400, [], limit_exceeded_error(Limit)}.

format_request_errors([]) ->
    <<>>;

format_request_errors(Errors) ->
    genlib_string:join(<<"\n">>, Errors).

process_woody_error(_, external, resource_unavailable, _Details) ->
    {503, [], <<
        "Service is unavailable for a moment.\n"
        "We are sorry."
    >>};

process_woody_error(_, external, result_unexpected, _Details) ->
    {500, [], <<
        "We've tripped over some bug in our code or something.\n"
        "We are sorry, we're working on fixing it."
    >>};

process_woody_error(_, external, result_unknown, _Details) ->
    {500, [], <<
        "Outcome of the operation is unknown.\n"
        "We've not managed to handle it in alotted time.\n"
        "We are so sorry."
    >>}.

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

populate_proxy_options(undefined, _ReqCtx) ->
    undefined;

populate_proxy_options(CallbackUrl, ReqCtx) ->
    Proxy = get_merchant_proxy(ReqCtx),
    {ok, ProxyOptions} = create_options(CallbackUrl, ReqCtx),
    Proxy#domain_Proxy{additional = ProxyOptions}.

get_merchant_proxy(ReqCtx) ->
    {ok, Globals} = capi_domain:get({globals, #domain_GlobalsRef{}}, ReqCtx),
    #domain_GlobalsObject{
        data = #domain_Globals{
            common_merchant_proxy = ProxyRef
        }
    } = Globals,
    #domain_Proxy{ref = ProxyRef}.

create_options(CallbackUrl, ReqCtx) ->
    Params = #proxy_merch_config_MerchantProxyParams{
        callback_url = CallbackUrl
    },
    service_call(
        merchant_config,
        'CreateOptions',
        [Params],
        ReqCtx
    ).

get_proxy_options(#domain_Proxy{additional = ProxyOptions}, ReqCtx) ->
    {ok, Result} = render_options(ProxyOptions, ReqCtx),
    Result;

get_proxy_options(undefined, _ReqCtx) ->
    undefined.

render_options(ProxyOptions, ReqCtx) ->
    service_call(
        merchant_config,
        'RenderOptions',
        [ProxyOptions],
        ReqCtx
    ).
