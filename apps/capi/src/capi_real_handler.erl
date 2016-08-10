-module(capi_real_handler).

-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_cds_thrift.hrl").
-include_lib("cp_proto/include/cp_merch_stat_thrift.hrl").

-behaviour(swagger_logic_handler).

%% API callbacks
-export([handle_request/3]).
-export([authorize_api_key/2]).

-spec authorize_api_key(ApiKey :: binary(), OperationID :: atom()) -> Result :: boolean() | {boolean(), #{binary() => any()}}.
authorize_api_key(ApiKey, OperationID) -> capi_auth:auth_api_key(ApiKey, OperationID).

-spec handle_request(OperationID :: atom(), Req :: #{}, Context :: #{}) -> {Code :: integer, Headers :: [], Response :: #{}}.
handle_request('CreateInvoice', Req, Context) ->
    InvoiceParams = maps:get('CreateInvoiceArgs', Req),
    RequestID = maps:get('X-Request-ID', Req),
    InvoiceContext = jsx:encode(genlib_map:get(<<"context">>, InvoiceParams)),
    Params =  #'payproc_InvoiceParams'{
        description = genlib_map:get(<<"description">>, InvoiceParams),
        product  = genlib_map:get(<<"product">>, InvoiceParams),
        amount   = genlib_map:get(<<"amount">>, InvoiceParams),
        due      = encode_datetime(genlib_map:get(<<"dueDate">>, InvoiceParams)),
        currency = #'domain_CurrencyRef'{symbolic_code = genlib_map:get(<<"currency">>, InvoiceParams)},
        context  = InvoiceContext
    },
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(invoicing, 'Create', [UserInfo, Params], create_context(RequestID)), %%@TODO deal with bad request
    case Result of
        {ok, InvoiceID} ->
            Resp = #{
                <<"id">> => InvoiceID
            },
            {201, [], Resp};
        _Error ->
            {500, [], <<"">>}
    end;

handle_request('CreatePayment', Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('CreatePaymentArgs', Req),
    RequestID = maps:get('X-Request-ID', Req),
    Token = genlib_map:get(<<"paymentToolToken">>, PaymentParams),
    PaymentTool = decode_bank_card(Token),
    PaymentSession = genlib_map:get(<<"paymentSession">>, PaymentParams),
    Params =  #payproc_InvoicePaymentParams{
        'payer' = #domain_Payer{
            payment_tool = PaymentTool,
            session = PaymentSession,
            client_info = #domain_ClientInfo{}
        },
        payment_tool = PaymentTool,
        session = PaymentSession
    },
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(invoicing, 'StartPayment', [UserInfo, InvoiceID, Params], create_context(RequestID)),
    case Result of
        {ok, PaymentID} ->
            Resp = #{
                <<"id">> => PaymentID
            },
            {201, [], Resp};
        _Error ->


            {500, [], <<"">>}
    end;

handle_request('CreatePaymentToolToken', Req, _Context) ->
    Params = maps:get('PaymentTool', Req),
    RequestID = maps:get('X-Request-ID', Req),
    case Params of
        #{<<"paymentToolType">> := <<"cardData">>} ->
            {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, Params)),
            CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, Params)), %%@FIXME what if is is undefined?
            CardData = #'CardData'{
                pan  = CardNumber,
                exp_date = #'ExpDate'{
                    month = Month,
                    year = Year
                },
                cardholder_name = genlib_map:get(<<"cardHolder">>, Params),
                cvv = genlib_map:get(<<"cvv">>, Params)
            },
          %%  UserInfo = get_user_info(Context),
            {Result, _NewContext} = service_call(
                cds_storage,
                'PutCardData',
                [CardData],
                create_context(RequestID)
            ),
            case Result of
                {ok, #'PutCardDataResult'{
                    session = Session,
                    bank_card = BankCard
                }} ->
                    Token = encode_bank_card(BankCard),
                    Resp = #{
                        <<"token">> => Token,
                        <<"session">> => Session
                    },
                    {201, [], Resp};
                _Error ->
                    {500, [], <<"">>}
            end;
        _ ->
            [400, [], logic_error(<<"wrong_payment_tool">>, <<"">>)]
    end;

handle_request('GetInvoiceByID', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    RequestID = maps:get('X-Request-ID', Req),
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(invoicing, 'Get', [UserInfo, InvoiceID], create_context(RequestID)),
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
            'context' = RawInvoiceContext
        }}} ->
            InvoiceContext = jsx:decode(RawInvoiceContext, [return_maps]),
            Resp = #{
                <<"id">> => InvoiceID,
                <<"amount">> => Amount,
                <<"context">> => InvoiceContext,
                <<"currency">> => Currency,
                <<"description">> => Description,
                <<"dueDate">> => DueDate,
                <<"product">> => Product,
                <<"status">> => genlib:to_binary(Status),
                <<"shopID">> => <<"42">> %%@TODO use a real one
            }, %%@TODO where is the shopID?
            {200, [], Resp};
        _Error ->
            {500, [], <<"">>}
    end;

handle_request('GetInvoiceEvents', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    _EventID = maps:get(eventID, Req),
    RequestID = maps:get('X-Request-ID', Req),
    UserInfo = get_user_info(Context),
    EventRange = #'payproc_EventRange'{
        limit = maps:get(limit, Req),
        'after' = maps:get(eventID, Req)
    },
    {Result, _NewContext} = service_call(invoicing, 'GetEvents', [UserInfo, InvoiceID, EventRange], create_context(RequestID)),
    case Result of
        {ok, Events} when is_list(Events) ->
            Resp = [decode_event(I) || I <- Events],
            {200, [], Resp};
        _Error ->
            {500, [], <<"">>}
    end;

handle_request('GetPaymentByID', Req, Context) ->
    PaymentID = maps:get(paymentID, Req),
    RequestID = maps:get('X-Request-ID', Req),
    UserInfo = get_user_info(Context),
    {Result, _NewContext} = service_call(invoicing, 'GetPayment', [UserInfo, PaymentID], create_context(RequestID)),
    case Result of
        {ok, Payment} ->
            Resp = decode_payment(Payment),
            {200, [], Resp};
       _Error ->
            {500, [], <<"">>}
    end;

handle_request('GetInvoices', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),
    Limit = genlib_map:get('limit', Req),
    Offset = genlib_map:get('offset', Req),
    [InvoiceStatus] = genlib_map:get('status', Req),  %%@TODO deal with many statuses
    Query = #{
        <<"merchant_id">> => get_merchant_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"invoice_id">> =>  genlib_map:get('invoiceID', Req),
        <<"from_time">> => encode_datetime(genlib_map:get('fromTime', Req)),
        <<"to_time">> => encode_datetime(genlib_map:get('fromTime', Req)),
        <<"invoice_status">> => InvoiceStatus
    },
    QueryParams = #{
        <<"size">> => Limit,
        <<"from">> => Offset
    },
    Dsl = create_dsl(invoices, Query, QueryParams),
    {Result, _NewContext} = service_call(merchant_stat, 'GetInvoices', [encode_stat_request(Dsl)], create_context(RequestID)),
    case Result of
        {ok, Invoices} ->
            Resp = [decode_invoice(I) || #merchstat_StatInvoice{invoice = I} <- Invoices],
            {200, [], Resp};
       _Error ->
            {500, [], <<"">>}
    end;

handle_request('GetPaymentConversionStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = payments_conversion_stat,
    Dsl = create_stat_dsl(StatType, Req, Context),
    {Result, _NewContext} = service_call(merchant_stat, 'GetStat', [encode_stat_request(Dsl)], create_context(RequestID)),
    case Result of
        {ok, Stats} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
       _Error ->
            {500, [], <<"">>}
    end;


handle_request('GetPaymentRevenueStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = payments_turnover,
    Dsl = create_stat_dsl(StatType, Req, Context),
    {Result, _NewContext} = service_call(merchant_stat, 'GetStat', [encode_stat_request(Dsl)], create_context(RequestID)),
    case Result of
        {ok, Stats} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
       _Error ->
            {500, [], <<"">>}
    end;

handle_request('GetPaymentGeoStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = payments_geo_stat,
    Dsl = create_stat_dsl(StatType, Req, Context),
    {Result, _NewContext} = service_call(merchant_stat, 'GetStat', [encode_stat_request(Dsl)], create_context(RequestID)),
    case Result of
        {ok, Stats} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
       _Error ->
            {500, [], <<"">>}
    end;

handle_request('GetPaymentRateStats', Req, Context) ->
    RequestID = maps:get('X-Request-ID', Req),

    StatType = customers_rate_stat,
    Dsl = create_stat_dsl(StatType, Req, Context),
    {Result, _NewContext} = service_call(merchant_stat, 'GetStat', [encode_stat_request(Dsl)], create_context(RequestID)),
    case Result of
        {ok, Stats} ->
            Resp = [decode_stat_response(StatType, S) || S <- Stats],
            {200, [], Resp};
       _Error ->
            {500, [], <<"">>}
    end;

handle_request(_OperationID, _Req, _Context) ->
    {501, [], <<"Not implemented">>}.

%%%

service_call(ServiceName, Function, Args, Context) ->
    ServiceUrl = get_service_base_url(ServiceName),
    {ServiceName, Path, Service} = cp_proto:get_service_spec(ServiceName),
    Url = iolist_to_binary([ServiceUrl, Path]),
    Request = {Service, Function, Args},
    {_Result, _ContextNext} = woody_client:call_safe(Context, Request, #{url => Url}).

create_context(ID) ->
    woody_client:new_context(genlib:to_binary(ID), capi_woody_event_handler).

get_service_base_url(invoicing) ->
    genlib_app:env(capi, hg_url);
get_service_base_url(cds_storage) ->
    genlib_app:env(capi, cds_url).


encode_datetime(undefined) ->
    undefined;

encode_datetime(RawDateTime) ->
    {ok, DateTime} = rfc3339:parse(RawDateTime),
    {ok, Formatted} = rfc3339:format(DateTime),
    Formatted.

logic_error(Code, Message) ->
    #{code => Code, message => Message}.

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
    base64:encode(jsx:encode(#{
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

decode_event(#'payproc_Event'{
    'id' = EventID,
    'created_at' = CreatedAt,
    'payload' = Payload
}) ->
    case Payload of
        {'invoice_event', {Key, InvoiceEvent}} ->
            EventBody = decode_invoice_event(InvoiceEvent),
            maps:put(#{
                <<"id">> => EventID,
                <<"createdAt">> => CreatedAt,
                <<"eventType">> => genlib:to_binary(Key),
                <<"eventBody">> => EventBody
            });
        _ -> #{}
    end.

decode_invoice_event(#payproc_InvoiceCreated{invoice = Invoice}) ->
    #{
        <<"invoice">> => Invoice
    };

decode_invoice_event(#payproc_InvoiceStatusChanged{status = {Status, _}}) ->
    #{
        <<"status">> => Status
    };
decode_invoice_event({'invoice_payment_started', #'payproc_InvoicePaymentStarted'{payment = Payment}}) ->
    #{
        <<"invoicePaymentStarted">> => decode_payment(Payment)
    };

decode_invoice_event({
    'invoice_payment_bound',
    #'payproc_InvoicePaymentBound'{
        payment_id = PaymentID,
        trx = #domain_TransactionInfo{
            id = TrxID,
            timestamp = TrxTimestamp
        }
    }
}) ->
    #{
        <<"invoicePaymentBound">> => #{
            <<"paymentID">> => PaymentID,
            <<"trx">> => #{
                <<"id">> => TrxID,
                <<"timestamp">> => TrxTimestamp
            }
        }
    };

decode_invoice_event({
    'invoice_payment_status_changed',
    #'payproc_InvoicePaymentStatusChanged'{payment_id = PaymentID, status = {Status, _}}
}) ->
    #{
        <<"invoicePaymentStatusChanged">> => #{
            <<"paymentID">> => PaymentID,
            <<"status">> => Status
        }

    }.

decode_payment(#domain_InvoicePayment{
    'id' = PaymentID,
    'created_at' = CreatedAt,
    'status' = {Status, _},
    'payer' = #domain_Payer{
        payment_tool = {
            'bank_card',
            #domain_BankCard{
                token = PaymentToolToken
            }
        }
    }
}) ->
    #{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => <<"42">>, %%@TODO get a real one
        <<"createdAt">> => CreatedAt,
        <<"status">> => genlib:to_binary(Status),
        <<"paymentToolToken">> => PaymentToolToken
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
    'context' = RawContext
}) ->
    Context = jsx:decode(RawContext, [return_maps]),
    #{
        <<"id">> => InvoiceID,
        <<"shopID">> => 42, %%TODO get the shop id!
        <<"amount">> => Amount,
        <<"currency">> => Currency,
        <<"context">> => Context,
        <<"dueDate">> => DueDate,
        <<"status">> => genlib:to_binary(Status),
        <<"product">> => Product,
        <<"description">> => Description
    }.

decode_stat_response(payments_conversion_stat, Response) ->
    #{
        <<"offset">> => maps:get(<<"offset">>, Response),
        <<"successfulCount">> => maps:get(<<"successful_count">>, Response),
        <<"totalCount">> => maps:get(<<"total_count">>, Response),
        <<"conversion">> => maps:get(<<"conversion">>, Response)
    };

decode_stat_response(payments_geo_stat, Response) ->
    #{
        <<"offset">> => maps:get(<<"offset">>, Response),
        <<"cityName">> => maps:get(<<"city_name">>, Response),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => maps:get(<<"amount_with_fee">>, Response),
        <<"revenue">> => maps:get(<<"amount_without_fee">>, Response)
    };

decode_stat_response(payments_turnover, Response) ->
    #{
        <<"offset">> => maps:get(<<"offset">>, Response),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => maps:get(<<"amount_with_fee">>, Response),
        <<"revenue">> => maps:get(<<"amount_without_fee">>, Response)
    };

decode_stat_response(customers_rate_stat, Response) ->
    #{
        <<"uniqueCount">> => maps:get(<<"unic_count">>, Response)
    }.

create_dsl(QueryType, QueryBody, QueryParams) when
    is_atom(QueryType),
    is_map(QueryBody),
    is_map(QueryParams) ->
    Query = maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{}),
    Basic = #{
        <<"query">> => Query
    },
    maps:fold(
        fun(Key, Value, Acc) ->
            maps:put(Key, Value, Acc)
        end,
        Basic,
        genlib_map:compact(QueryParams)
    ).

encode_stat_request(Dsl) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl));

encode_stat_request(Dsl) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl
    }.

create_stat_dsl(StatType, Req, Context) ->
    SplitUnit = genlib_map:get('splitUnit', Req),
    SplitSize = genlib_map:get('splitSize', Req),

    Query = #{
        <<"merchant_id">> => get_merchant_id(Context),
        <<"shop_id">> => genlib_map:get('shopID', Req),
        <<"from_time">> => encode_datetime(genlib_map:get('fromTime', Req)),
        <<"to_time">> => encode_datetime(genlib_map:get('fromTime', Req)),
        <<"split_interval">> => get_split_interval(SplitUnit, SplitSize)
    },
    create_dsl(StatType, Query, #{}).

get_split_interval(SplitUnit, minute) ->
    SplitUnit * 60;

get_split_interval(SplitUnit, hour) ->
    SplitUnit * 60 * 60;

get_split_interval(SplitUnit, day) ->
    SplitUnit * 60 * 60 * 24;

get_split_interval(SplitUnit, week) ->
    SplitUnit * 60 * 60 * 24 * 7;

get_split_interval(SplitUnit, month) ->
    SplitUnit * 60 * 60 * 24 * 30;

get_split_interval(SplitUnit, year) ->
    SplitUnit * 60 * 60 * 24 * 365.
