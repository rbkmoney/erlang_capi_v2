-module(capi_handler_utils).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([general_error/2]).
-export([logic_error/1]).
-export([logic_error/2]).
-export([server_error/1]).
-export([format_request_errors/1]).

-export([assert_party_accessible/2]).
-export([run_if_party_accessible/3]).

-export([service_call_with/3]).
-export([service_call/2]).
-export([map_service_result/1]).

-export([get_auth_context/1]).
-export([get_woody_context/1]).
-export([get_user_info/1]).
-export([get_subject_id/1]).
-export([get_user_id/1]).
-export([get_party_id/1]).

-export([issue_access_token/2]).
-export([merge_and_compact/2]).
-export([get_time/2]).
-export([get_split_interval/2]).
-export([get_time_diff/2]).
-export([collect_events/5]).

-export([unwrap_payment_session/1]).
-export([wrap_payment_session/2]).

-export([get_invoice_by_id/2]).
-export([get_payment_by_id/3]).
-export([get_refund_by_id/4]).
-export([get_payment_methods/3]).

-export([create_dsl/3]).
-export([emplace_token_provider_data/3]).

-type processing_context() :: capi_handler:processing_context().
-type response() :: capi_handler:response().
-type entity() ::
    dmsl_domain_thrift:'Invoice'()
    | dmsl_payment_processing_thrift:'Customer'()
    | dmsl_domain_thrift:'InvoiceTemplate'().
-type token_source() :: capi_auth:token_spec() | entity().

-spec general_error(cowboy:http_status(), binary()) -> response().
general_error(Code, Message) ->
    create_error_resp(Code, #{<<"message">> => genlib:to_binary(Message)}).

-spec logic_error(term()) -> response().
logic_error(invalidPaymentToolToken) ->
    logic_error(invalidPaymentToolToken, <<"Specified payment tool token is invalid">>).

-spec logic_error
    (term(), io_lib:chars() | binary()) -> response();
    (term(), {binary(), binary() | undefined}) -> response().
logic_error(externalIDConflict, {ID, undefined}) ->
    logic_error(externalIDConflict, {ID, <<"undefined">>});
logic_error(externalIDConflict, {ID, ExternalID}) ->
    Data = #{
        <<"externalID">> => ExternalID,
        <<"id">> => ID,
        <<"message">> => <<"This 'externalID' has been used by another request">>
    },
    create_error_resp(409, Data);
logic_error(externalIDConflict, ExternalID) ->
    Data = #{
        <<"externalID">> => ExternalID,
        <<"message">> => <<"This 'externalID' has been used by another request">>
    },
    create_error_resp(409, Data);
logic_error(Code, Message) ->
    Data = #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)},
    create_error_resp(400, Data).

create_error_resp(Code, Data) ->
    create_error_resp(Code, #{}, Data).

create_error_resp(Code, Headers, Data) ->
    {Code, Headers, Data}.

-spec server_error(integer()) -> {integer(), #{}, <<>>}.
server_error(Code) when Code >= 500 andalso Code < 600 ->
    {Code, #{}, <<>>}.

-spec format_request_errors(list()) -> binary().
format_request_errors([]) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

%%%

% Нужно быть аккуратным с флагами их порядок влияет на порядок аргументов при вызове функций!
% обычно параметры идут в порядке [user_info, party_id],
% но это зависит от damsel протокола
-spec service_call_with(list(atom()), {atom(), atom(), tuple()}, processing_context()) -> woody:result().
service_call_with(Flags, Call, Context) ->
    % реверс тут чтобы в флагах писать порядок аналогично вызову функций
    service_call_with_(lists:reverse(Flags), Call, Context).

service_call_with_([user_info | T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(T, {ServiceName, Function, append_to_tuple(get_user_info(Context), Args)}, Context);
service_call_with_([party_id | T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(T, {ServiceName, Function, append_to_tuple(get_party_id(Context), Args)}, Context);
service_call_with_([], Call, Context) ->
    service_call(Call, Context).

-spec service_call({atom(), atom(), tuple()}, processing_context()) -> woody:result().
service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    capi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

-spec map_service_result({ok, Result} | {exception, woody_error:business_error()}) -> Result | undefined.
map_service_result({ok, Result}) ->
    Result;
map_service_result({exception, _}) ->
    undefined.

-spec get_auth_context(processing_context()) -> any().
get_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

-spec get_woody_context(processing_context()) -> any().
get_woody_context(#{woody_context := WoodyContext}) ->
    WoodyContext.

-spec get_user_info(processing_context()) -> dmsl_payment_processing_thrift:'UserInfo'().
get_user_info(Context) ->
    #payproc_UserInfo{
        id = get_user_id(Context),
        type = {external_user, #payproc_ExternalUser{}}
    }.

-spec get_subject_id(processing_context()) -> binary().
get_subject_id(Context) ->
    capi_auth:get_subject_id(get_auth_context(Context)).

-spec get_user_id(processing_context()) -> binary().
get_user_id(Context) ->
    %% TODO Replace when user ids finally stop being equal to party ids
    %% capi_auth:get_user_id(get_auth_context(Context)).
    get_subject_id(Context).

-spec get_party_id(processing_context()) -> binary().
get_party_id(Context) ->
    %% TODO Replace when user ids finally stop being equal to party ids
    %% capi_auth:get_party_id(get_auth_context(Context)).
    get_subject_id(Context).

%% Utils

-spec append_to_tuple(any(), tuple()) -> tuple().
append_to_tuple(Item, Tuple) ->
    list_to_tuple([Item | tuple_to_list(Tuple)]).

-spec issue_access_token(token_source(), processing_context()) -> map().
issue_access_token(#domain_Invoice{} = Invoice, ProcessingContext) ->
    TokenSpec = #{
        party => Invoice#domain_Invoice.owner_id,
        scope => {invoice, Invoice#domain_Invoice.id},
        shop => Invoice#domain_Invoice.shop_id
    },
    issue_access_token(TokenSpec, ProcessingContext);
issue_access_token(#payproc_Customer{} = Customer, ProcessingContext) ->
    TokenSpec = #{
        party => Customer#payproc_Customer.owner_id,
        scope => {customer, Customer#payproc_Customer.id},
        shop => Customer#payproc_Customer.shop_id
    },
    issue_access_token(TokenSpec, ProcessingContext);
issue_access_token(#domain_InvoiceTemplate{} = InvoiceTpl, ProcessingContext) ->
    TokenSpec = #{
        party => InvoiceTpl#domain_InvoiceTemplate.owner_id,
        scope => {invoice_template, InvoiceTpl#domain_InvoiceTemplate.id},
        shop => InvoiceTpl#domain_InvoiceTemplate.shop_id
    },
    issue_access_token(TokenSpec, ProcessingContext);
issue_access_token(TokenSpec, ProcessingContext) ->
    #{
        <<"payload">> =>
            capi_auth:issue_access_token(TokenSpec, get_woody_context(ProcessingContext))
    }.

-spec merge_and_compact(map(), map()) -> map().
merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

-spec get_time(term(), map()) -> TimestampUTC :: binary() | undefined.
get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            capi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

-spec get_split_interval(integer(), atom()) -> integer().
get_split_interval(SplitSize, minute) -> SplitSize * 60;
get_split_interval(SplitSize, hour) -> get_split_interval(SplitSize, minute) * 60;
get_split_interval(SplitSize, day) -> get_split_interval(SplitSize, hour) * 24;
get_split_interval(SplitSize, week) -> get_split_interval(SplitSize, day) * 7;
get_split_interval(SplitSize, month) -> get_split_interval(SplitSize, day) * 30;
get_split_interval(SplitSize, year) -> get_split_interval(SplitSize, day) * 365.

-spec get_time_diff(binary(), binary()) -> integer().
get_time_diff(From, To) ->
    UnixFrom = genlib_rfc3339:parse(From, second),
    UnixTo = genlib_rfc3339:parse(To, second),
    UnixTo - UnixFrom.

-spec collect_events(
    integer(),
    integer(),
    fun((_) -> {exception, _} | {ok, _}),
    fun((_, _) -> false | {true, #{binary() => binary() | [any()] | integer()}}),
    undefined
) -> {ok, _} | {exception, _}.
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
                    [Ev | Acc];
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

-spec unwrap_payment_session(binary()) -> {map(), binary()}.
unwrap_payment_session(Encoded) ->
    #{
        <<"clientInfo">> := ClientInfo,
        <<"paymentSession">> := PaymentSession
    } =
        try
            capi_utils:base64url_to_map(Encoded)
        catch
            error:badarg ->
                erlang:throw(invalid_payment_session)
        end,
    {ClientInfo, PaymentSession}.

-spec wrap_payment_session(map(), binary()) -> binary().
wrap_payment_session(ClientInfo, PaymentSession) ->
    capi_utils:map_to_base64url(#{
        <<"clientInfo">> => ClientInfo,
        <<"paymentSession">> => PaymentSession
    }).

-spec get_invoice_by_id(binary(), processing_context()) -> woody:result().
get_invoice_by_id(InvoiceID, Context) ->
    EventRange = #payproc_EventRange{},
    Args = {InvoiceID, EventRange},
    service_call_with([user_info], {invoicing, 'Get', Args}, Context).

-spec get_payment_by_id(binary(), binary(), processing_context()) -> woody:result().
get_payment_by_id(InvoiceID, PaymentID, Context) ->
    service_call_with([user_info], {invoicing, 'GetPayment', {InvoiceID, PaymentID}}, Context).

-spec get_refund_by_id(binary(), binary(), binary(), processing_context()) -> woody:result().
get_refund_by_id(InvoiceID, PaymentID, RefundID, Context) ->
    service_call_with([user_info], {invoicing, 'GetPaymentRefund', {InvoiceID, PaymentID, RefundID}}, Context).

-spec get_payment_methods(atom(), tuple(), processing_context()) -> woody:result().
get_payment_methods(ServiceName, Args, Context) ->
    case service_call_with([user_info], {ServiceName, 'ComputeTerms', Args}, Context) of
        {ok, #domain_TermSet{payments = undefined}} ->
            {ok, []};
        {ok, #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = {value, PaymentMethodRefs}
            }
        }} ->
            {ok, PaymentMethodRefs};
        Error ->
            Error
    end.

-spec create_dsl(atom(), map(), map()) -> map().
create_dsl(QueryType, QueryBody, QueryParams) ->
    merge_and_compact(
        #{<<"query">> => maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{})},
        QueryParams
    ).

-spec assert_party_accessible(binary(), binary()) -> ok.
assert_party_accessible(PartyID, PartyID) ->
    ok;
assert_party_accessible(_UserID, _PartyID) ->
    throw(party_inaccessible).

-spec run_if_party_accessible(binary(), binary(), function()) -> woody:result().
run_if_party_accessible(UserID, PartyID, Fun) ->
    try
        assert_party_accessible(UserID, PartyID),
        Fun()
    catch
        throw:party_inaccessible ->
            {ok, general_error(404, <<"Party not found">>)}
    end.

-spec emplace_token_provider_data(entity(), list(), processing_context()) -> list().
emplace_token_provider_data(#domain_Invoice{} = Invoice, PaymentMethods, Context) ->
    InvoiceID = Invoice#domain_Invoice.id,
    PartyID = Invoice#domain_Invoice.owner_id,
    ShopID = Invoice#domain_Invoice.shop_id,
    TokenProviderData = maps:merge(
        #{<<"orderID">> => InvoiceID},
        construct_token_provider_data(PartyID, ShopID, Context)
    ),
    emplace_token_provider_data(PaymentMethods, TokenProviderData);
emplace_token_provider_data(#domain_InvoiceTemplate{} = InvoiceTemplate, PaymentMethods, Context) ->
    PartyID = InvoiceTemplate#domain_InvoiceTemplate.owner_id,
    ShopID = InvoiceTemplate#domain_InvoiceTemplate.shop_id,
    TokenProviderData = construct_token_provider_data(PartyID, ShopID, Context),
    emplace_token_provider_data(PaymentMethods, TokenProviderData);
emplace_token_provider_data(#payproc_Customer{} = Customer, PaymentMethods, Context) ->
    PartyID = Customer#payproc_Customer.owner_id,
    ShopID = Customer#payproc_Customer.shop_id,
    TokenProviderData = construct_token_provider_data(PartyID, ShopID, Context),
    emplace_token_provider_data(PaymentMethods, TokenProviderData).

emplace_token_provider_data(PaymentMethods, TokenProviderData) ->
    lists:map(
        fun
            (#{<<"tokenProviders">> := _Providers} = PaymentMethod) ->
                PaymentMethod#{<<"tokenProviderData">> => TokenProviderData};
            (PaymentMethod) ->
                PaymentMethod
        end,
        PaymentMethods
    ).

construct_token_provider_data(PartyID, ShopID, Context) ->
    {ok, ShopContract} = capi_party:get_shop_contract(PartyID, ShopID, Context),
    ShopName = ShopContract#payproc_ShopContract.shop#domain_Shop.details#domain_ShopDetails.name,
    PiRef = ShopContract#payproc_ShopContract.contract#domain_Contract.payment_institution,
    {ok, Pi} = capi_domain:get_payment_institution(PiRef, Context),
    Realm = Pi#domain_PaymentInstitution.realm,
    MerchantID = capi_merchant_id:encode(Realm, PartyID, ShopID),
    #{
        <<"merchantID">> => MerchantID,
        <<"merchantName">> => ShopName,
        <<"realm">> => genlib:to_binary(Realm)
    }.
