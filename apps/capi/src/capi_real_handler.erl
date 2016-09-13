-module(capi_real_handler).

-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_cds_thrift.hrl").
-include_lib("cp_proto/include/cp_merch_stat_thrift.hrl").

-behaviour(swagger_logic_handler).

%% API callbacks
-export([handle_request/3]).
-export([authorize_api_key/2]).

-spec authorize_api_key(OperationID :: swagger_api:operation_id(), ApiKey :: binary()) ->
    Result :: false | {true, #{binary() => any()}}.

authorize_api_key(OperationID, ApiKey) -> capi_auth:auth_api_key(OperationID, ApiKey).

-spec handle_request(OperationID :: swagger_api:operation_id(), Req :: #{}, Context :: #{}) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

handle_request(OperationID, Req, Context) ->
    capi_utils:logtag_process(operation_id, OperationID),
    _ = lager:info("Processing request ~p", [OperationID]),
    process_request(OperationID, Req, Context).

-spec process_request(OperationID :: swagger_api:operation_id(), Req :: #{}, Context :: #{}) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request(OperationID = 'CreateInvoice', Req, Context) ->
    InvoiceParams = maps:get('CreateInvoiceArgs', Req),
    RequestID = maps:get('X-Request-ID', Req),
    InvoiceContext = jsx:encode(genlib_map:get(<<"context">>, InvoiceParams)),
    Params =  #'payproc_InvoiceParams'{
        description = genlib_map:get(<<"description">>, InvoiceParams),
        product  = genlib_map:get(<<"product">>, InvoiceParams),
        amount   = genlib_map:get(<<"amount">>, InvoiceParams),
        due      = genlib_map:get(<<"dueDate">>, InvoiceParams),
        currency = #'domain_CurrencyRef'{symbolic_code = genlib_map:get(<<"currency">>, InvoiceParams)},
        context  = InvoiceContext,
        shop_id = genlib_map:get(<<"shopID">>, InvoiceParams)
    },
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(
        invoicing,
        'Create',
        [UserInfo, Params],
        create_context(RequestID)
    ),
    case Result of
        {ok, InvoiceID} ->
            Resp = #{
                <<"id">> => InvoiceID
            },
            {201, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'CreatePayment', Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('CreatePaymentArgs', Req),
    RequestID = maps:get('X-Request-ID', Req),
    Token = genlib_map:get(<<"paymentToolToken">>, PaymentParams),
    PaymentTool = decode_bank_card(Token),
    EncodedSession = genlib_map:get(<<"paymentSession">>, PaymentParams),
    {ClientInfo, PaymentSession} = unwrap_session(EncodedSession),
    Params =  #payproc_InvoicePaymentParams{
        'payer' = #domain_Payer{
            payment_tool = PaymentTool,
            session = PaymentSession,
            client_info = #domain_ClientInfo{
                fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
                ip_address = maps:get(<<"ipAddress">>, ClientInfo)
            }
        }
    },
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(invoicing,
        'StartPayment',
        [UserInfo, InvoiceID, Params],
        create_context(RequestID)
    ),
    case Result of
        {ok, PaymentID} ->
            Resp = #{
                <<"id">> => PaymentID
            },
            {201, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'CreatePaymentToolToken', Req, _Context) ->
    Params = maps:get('CreatePaymentToolTokenArgs', Req),
    RequestID = maps:get('X-Request-ID', Req),
    ClientInfo = maps:get(<<"clientInfo">>, Params),
    PaymentTool = maps:get(<<"paymentTool">>, Params),
    case PaymentTool of
        #{<<"paymentToolType">> := <<"cardData">>} ->
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
            {Result, _NewContext} = service_call(
                cds_storage,
                'PutCardData',
                [CardData],
                create_context(RequestID)
            ),
            case Result of
                {ok, #'PutCardDataResult'{
                    session = PaymentSession,
                    bank_card = BankCard
                }} ->
                    Token = encode_bank_card(BankCard),
                    Session = wrap_session(ClientInfo, PaymentSession),
                    Resp = #{
                        <<"token">> => Token,
                        <<"session">> => Session
                    },
                    {201, [], Resp};
                Error ->
                    process_request_error(OperationID, Error)
            end;
        _ ->
            {400, [], logic_error(wrong_payment_tool, <<"">>)}
    end;

process_request(OperationID = 'GetInvoiceByID', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    RequestID = maps:get('X-Request-ID', Req),
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(
        invoicing,
        'Get',
        [UserInfo, InvoiceID],
        create_context(RequestID)
    ),
    case Result of
        {ok, #'payproc_InvoiceState'{invoice = #domain_Invoice{
            'id' = InvoiceID,
            'created_at' = _CreatedAt,
            'status' = {Status, _},
            'due' = DueDate,
            'product'= Product,
            'description' = Description,
            'cost' = #domain_Funds{
                amount = Amount,
                currency = #domain_Currency{
                    symbolic_code = Currency
                }
            },
            'context' = RawInvoiceContext,
            'shop_id' = ShopID
        }}} ->
         %%%   InvoiceContext = jsx:decode(RawInvoiceContext, [return_maps]), @TODO deal with non json contexts
            InvoiceContext = #{
                <<"context">> => RawInvoiceContext
            },
            Resp = #{
                <<"id">> => InvoiceID,
                <<"amount">> => Amount,
                <<"context">> => InvoiceContext,
                <<"currency">> => Currency,
                <<"description">> => Description,
                <<"dueDate">> => DueDate,
                <<"product">> => Product,
                <<"status">> => genlib:to_binary(Status),
                <<"shopID">> => ShopID
            },
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetInvoiceEvents', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    _EventID = maps:get(eventID, Req),
    RequestID = maps:get('X-Request-ID', Req),
    UserInfo = get_user_info(Context),
    EventRange = #'payproc_EventRange'{
        limit = maps:get(limit, Req),
        'after' = maps:get(eventID, Req)
    },
    {Result, _NewContext} = service_call(
        invoicing,
        'GetEvents',
        [UserInfo, InvoiceID, EventRange],
        create_context(RequestID)
    ),
    case Result of
        {ok, Events} when is_list(Events) ->
            Resp = [decode_event(I) || I <- Events],
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetPaymentByID', Req, Context) ->
    PaymentID = maps:get(paymentID, Req),
    InvoiceID = maps:get(invoiceID, Req),
    RequestID = maps:get('X-Request-ID', Req),
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(
        invoicing,
        'GetPayment',
        [UserInfo, PaymentID],
        create_context(RequestID)
    ),
    case Result of
        {ok, Payment} ->
            Resp = decode_payment(InvoiceID, Payment),
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetInvoices', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),
    Limit = genlib_map:get('limit', Req),
    Offset = genlib_map:get('offset', Req),
    InvoiceStatus = case genlib_map:get('status', Req) of
        undefined -> undefined;
        [Status] -> Status
    end,  %%@TODO deal with many statuses
    Query = #{
        <<"merchant_id">> => get_merchant_id(Context),
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
    {Result, _NewContext} = service_call(
        merchant_stat,
        'GetInvoices',
        [encode_stat_request(Dsl)],
        create_context(RequestID)
    ),
    case Result of
        {ok, #merchstat_StatResponse{data = {'invoices', Invoices}, total_count = TotalCount}} ->
            DecodedInvoices = [decode_invoice(I) || #merchstat_StatInvoice{invoice = I} <- Invoices],
            Resp = #{
                <<"invoices">> => DecodedInvoices,
                <<"totalCount">> => TotalCount
            },
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetPaymentConversionStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = payments_conversion_stat,
    Result = call_merchant_stat(StatType, Req, Context, RequestID),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;


process_request(OperationID = 'GetPaymentRevenueStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = payments_turnover,
    Result = call_merchant_stat(StatType, Req, Context, RequestID),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetPaymentGeoStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = payments_geo_stat,
    Result = call_merchant_stat(StatType, Req, Context, RequestID),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetPaymentRateStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = customers_rate_stat,
    Result = call_merchant_stat(StatType, Req, Context, RequestID),

    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
        Error ->
            process_request_error(OperationID, Error)
    end;

process_request(OperationID = 'GetMyParty', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),
    prepare_party(OperationID, RequestID, Context, fun(RequestContext) ->
        UserInfo = get_user_info(Context),
        PartyID = get_merchant_id(Context),
        {Result, _} = service_call(
            party_management,
            'Get',
            [UserInfo, PartyID],
            RequestContext
        ),
        case Result of
            {ok, #payproc_PartyState{
                party = Party
            }} ->
                Resp = decode_party(Party),
                {200, [], Resp};
            Error ->
                process_request_error(OperationID, Error)
        end
    end);

process_request(_OperationID, _Req, _Context) ->
    {501, [], <<"Not implemented">>}.

%%%

service_call(ServiceName, Function, Args, Context) ->
    {Result, Context} = cp_proto:call_service_safe(ServiceName, Function, Args, Context),
    _ = log_service_call_result(Result),
    {Result, Context}.

log_service_call_result(Result) ->
    _ = lager:debug("Service call result ~p", [Result]),
    log_service_call_result_(Result).

log_service_call_result_({ok, _}) ->
    lager:info("Service call result success");

log_service_call_result_({exception, Exception}) ->
    lager:error("Service call result exception ~p", [Exception]);

log_service_call_result_(_) ->
    ok.

create_context(ID) ->
    woody_client:new_context(genlib:to_binary(ID), capi_woody_event_handler).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

limit_exceeded_error(Limit) ->
    logic_error(limit_exceeded, io_lib:format("Max limit: ~p", [Limit])).

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month,  Year] = binary:split(ExpDate, <<"/">>),
    {genlib:to_int(Month), 2000 + genlib:to_int(Year)}.

get_user_info(Context) ->
    #payproc_UserInfo{
        id = get_merchant_id(Context)
    }.

get_merchant_id(Context) ->
    maps:get(<<"sub">>, Context).

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
    #{
        <<"id">> => EventID,
        <<"createdAt">> => CreatedAt,
        <<"eventType">> => EventType,
        <<"eventBody">> => EventBody
    }.

decode_invoice_event(_, {
    invoice_created,
    #payproc_InvoiceCreated{invoice = Invoice}
}) ->
    {<<"invoiceCreated">>, #{
        <<"invoice">> => decode_invoice(Invoice)
    }};

decode_invoice_event(_, {
    invoice_status_changed,
    #payproc_InvoiceStatusChanged{status = {Status, _}}
}) ->
    {<<"invoiceStatusChanged">>, #{
        <<"status">> => genlib:to_binary(Status)
    }};

decode_invoice_event(InvoiceID, {invoice_payment_event, Event}) ->
    decode_payment_event(InvoiceID, Event).

decode_payment_event(InvoiceID, {
    invoice_payment_started,
    #'payproc_InvoicePaymentStarted'{payment = Payment}
}) ->
    {<<"paymentStarted">>, #{
        <<"payment">> => decode_payment(InvoiceID, Payment)
    }};

decode_payment_event(_, {
    invoice_payment_bound,
    #'payproc_InvoicePaymentBound'{payment_id = PaymentID}
}) ->
    {<<"paymentBound">>, #{
        <<"paymentID">> => PaymentID
    }};

decode_payment_event(_, {
    invoice_payment_status_changed,
    #'payproc_InvoicePaymentStatusChanged'{payment_id = PaymentID, status = {Status, _}}
}) ->
    {<<"paymentStatusChanged">>, #{
        <<"paymentID">> => PaymentID,
        <<"status">> => genlib:to_binary(Status)
    }}.

decode_payment(InvoiceID, #domain_InvoicePayment{
    'id' = PaymentID,
    'created_at' = CreatedAt,
    'status' = {Status, _},
    'payer' = #domain_Payer{
        payment_tool = {
            'bank_card',
            BankCard
        }
    }
}) ->
    #{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"createdAt">> => CreatedAt,
        <<"status">> => genlib:to_binary(Status),
        <<"paymentToolToken">> => encode_bank_card(BankCard)
    }.

decode_invoice(#domain_Invoice{
    'id' = InvoiceID,
    'created_at' = _CreatedAt, %%@TODO add it to the swagger spec
    'status' = {Status, _},
    'due'  = DueDate,
    'product' = Product,
    'description' = Description,
    'cost' = #domain_Funds{
        amount = Amount,
        currency = #domain_Currency{
            symbolic_code = Currency
        }
    },
    'context' = RawContext,
    'shop_id' = ShopID
}) ->
   %%% Context = jsx:decode(RawContext, [return_maps]), %%@TODO deal with non json contexts
    Context = #{
        <<"context">> => RawContext
    },
    genlib_map:compact(#{
        <<"id">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"amount">> => Amount,
        <<"currency">> => Currency,
        <<"context">> => Context,
        <<"dueDate">> => DueDate,
        <<"status">> => genlib:to_binary(Status),
        <<"product">> => Product,
        <<"description">> => Description
    }).

decode_party(#domain_Party{
    id = PartyID,
    blocking = Blocking,
    suspension = Suspension,
    shops = Shops
}) ->
    PreparedShops = maps:fold(
        fun(_, Shop, Acc) -> [decode_shop(Shop) | Acc] end,
        [],
        Shops
    ),
    #{
        <<"partyID">> => PartyID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension),
        <<"shops">> => PreparedShops
    }.

decode_shop(#domain_Shop{
    id = ShopID,
    blocking = Blocking,
    suspension = Suspension,
    category  = #domain_CategoryObject{
        ref = #domain_CategoryRef{
            id = CategoryRef
        }
    },
    details  = ShopDetails,
    contractor = Contractor,
    contract  = ShopContract
}) ->
    #{
        <<"shopID">> => ShopID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension),
        <<"categoryRef">> => CategoryRef,
        <<"shopDetails">> => decode_shop_details(ShopDetails),
        <<"contractor">> => decode_contractor(Contractor),
        <<"contract">> => decode_shop_contract(ShopContract)
    }.

decode_shop_details(#domain_ShopDetails{
    name = Name,
    description = Description,
    location = Location
}) ->
    #{
      <<"name">> => Name,
      <<"description">> => Description,
      <<"location">> => Location
    }.

decode_contractor(#domain_Contractor{
    registered_name = RegisteredName,
    legal_entity = _LegalEntity
}) ->
    #{
        <<"registeredName">> => RegisteredName,
        <<"legalEntity">> => <<"dummy_entity">> %% @TODO Fix legal entity when thrift is ready
    }.

decode_shop_contract(#domain_ShopContract{
    number = Number,
    system_contractor = #domain_ContractorObject{
        ref = #domain_ContractorRef{
            id = ContractorRef
        }
    },
    concluded_at = ConcludedAt,
    valid_since = ValidSince,
    valid_until = ValidUntil,
    terminated_at = _TerminatedAt %% @TODO show it to the client?
}) ->
    #{
        <<"number">> => Number,
        <<"systemContractorRef">> => ContractorRef,
        <<"concludedAt">> => ConcludedAt,
        <<"validSince">> => ValidSince,
        <<"validUntil">> => ValidUntil
    }.

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
        <<"cityName">> => maps:get(<<"city_name">>, Response),
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
        <<"merchant_id">> => get_merchant_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"from_time">> => FromTime,
        <<"to_time">> => ToTime,
        <<"split_interval">> => SplitInterval
    },
    create_dsl(StatType, Query, #{}).

call_merchant_stat(StatType, Req, Context, RequestID) ->
    Dsl = create_stat_dsl(StatType, Req, Context),
    {Result, _NewContext} = service_call(
        merchant_stat,
        'GetStatistics',
        [encode_stat_request(Dsl)],
        create_context(RequestID)
    ),
    Result.

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

process_request_error(_, {exception, #payproc_InvalidUser{}}) ->
    {400, [], logic_error(invalid_user, <<"Ivalid user">>)};

process_request_error(_, {exception, #'InvalidRequest'{}}) ->
    {400, [], logic_error(invalid_request, <<"Request can't be processed">>)};

process_request_error(_, {exception, #payproc_UserInvoiceNotFound{}} ) ->
    {404, [], general_error(<<"Invoice not found">>)};

process_request_error(_,  {exception, #payproc_InvalidInvoiceStatus{}} ) ->
    {400, [], logic_error(invalid_invoice_status, <<"Invalid invoice status">>)};

process_request_error(_, {exception, #payproc_InvoicePaymentPending{}}) ->
    {400, [], logic_error(invalid_payment_status, <<"Invalid payment status">>)};

process_request_error(_,  {exception, #'InvalidCardData'{}}) ->
    {400, [], logic_error(invalid_request, <<"Card data is invalid">>)};

process_request_error(_, {exception, #'KeyringLocked'{}}) ->
    {503, [], <<"">>};

process_request_error(_, {exception, #payproc_EventNotFound{}}) ->
    {404, [], general_error(<<"Event not found">>)};

process_request_error(_, {exception, #payproc_InvoicePaymentNotFound{}} ) ->
    {404, [], general_error(<<"Payment not found">>)};

process_request_error(_, {exception, #merchstat_DatasetTooBig{limit = Limit}}) ->
    {400, [], limit_exceeded_error(Limit)}.


prepare_party(OperationID, RequestID, Context, ProcessReqFun) ->
    {Result, RequestContext} = create_party(RequestID, Context),
    case Result of
        ok ->
            ProcessReqFun(RequestContext);
        Error ->
            process_request_error(OperationID, Error)
    end.

create_party(RequestID, Context) ->
    PartyID = get_merchant_id(Context),
    UserInfo = get_user_info(Context),
    {Result, RequestContext} = service_call(
        party_management,
        'Create',
        [UserInfo, PartyID],
        create_context(RequestID)
    ),
    R = case Result of
        ok ->
            ok;
        {exception, #payproc_PartyExists{}} ->
            ok;
        Error ->
            Error
    end,
    {R, RequestContext}.

