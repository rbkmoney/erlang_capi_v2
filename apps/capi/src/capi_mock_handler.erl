-module(capi_mock_handler).

-behaviour(swagger_logic_handler).
-behaviour(gen_server).

%% API callbacks
-export([start_link/0]).
-export([handle_request/3]).
-export([authorize_api_key/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    tid :: ets:tid(),
    last_id = 0 ::integer()
}).

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec authorize_api_key(OperationID :: swagger_api:operation_id(), ApiKey :: binary()) ->
    Result :: false | {true, #{binary() => any()}}.

authorize_api_key(OperationID, ApiKey) -> capi_auth:auth_api_key(OperationID, ApiKey).

-spec handle_request(OperationID :: swagger_api:operation_id(), Req :: #{}, Context :: #{}) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

handle_request(OperationID = 'CreateInvoice', Req, _Context) ->
    _ = lager:info("Processing operation ~p", [OperationID]),
    InvoiceParams = maps:get('CreateInvoiceArgs', Req),
    ID = new_id(),
    Invoice = #{
        <<"id">> => ID,
        <<"amount">> => maps:get(<<"amount">>, InvoiceParams),
        <<"context">> => maps:get(<<"context">>, InvoiceParams),
        <<"currency">> => maps:get(<<"currency">>, InvoiceParams),
        <<"description">> => maps:get(<<"description">>, InvoiceParams),
        <<"dueDate">> => maps:get(<<"dueDate">>, InvoiceParams),
        <<"product">> => maps:get(<<"product">>, InvoiceParams),
        <<"shopID">> => maps:get(<<"shopID">>, InvoiceParams)
    },
    put_data(ID, invoice, Invoice),
    Resp = #{
        <<"id">> => ID
    },
    {201, [], Resp};

handle_request(OperationID = 'CreatePayment', Req, _Context) ->
    _ = lager:info("Processing operation ~p", [OperationID]),
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('CreatePaymentArgs', Req),
    PaymentSession = maps:get(<<"paymentSession">>, PaymentParams),
    case exhaust_session(PaymentSession) of
        ok ->
            PaymentID = new_id(),
            Payment = #{
                <<"id">> => PaymentID ,
                <<"invoiceID">> => InvoiceID,
                <<"createdAt">> => <<"2016-12-12 17:00:00">>,
                <<"status">> => <<"pending">>,
                <<"paymentToolToken">> => maps:get(<<"paymentToolToken">>, PaymentParams)
            },
            put_data(PaymentID, payment, Payment),
            Resp = #{
                <<"id">> => PaymentID
            },
            add_delayed_fake_payment(PaymentID),
            {201, [], Resp};
        {error, expired} ->
            Resp = logic_error(<<"expired_session">>, <<"Payment session is not valid">>),
            {400, [], Resp}
    end;

handle_request(OperationID = 'CreatePaymentToolToken', Req, _Context) ->
    _ = lager:info("Processing operation ~p", [OperationID]),
    Params = maps:get('CreatePaymentToolTokenArgs', Req),
    PaymentTool = maps:get(<<"paymentTool">>, Params),
    Token = tokenize_payment_tool(PaymentTool),
    put_data(new_id(), token, Token),
    Session = generate_session(),
    put_data(new_id(), session, Session),
    Resp = #{
        <<"token">> => Token,
        <<"session">> => Session
    },
    {201, [], Resp};

handle_request(OperationID = 'GetInvoiceByID', Req, _Context) ->
    _ = lager:info("Processing operation ~p", [OperationID]),
    InvoiceID = maps:get(invoiceID, Req),
    case get_data_by_id(InvoiceID, invoice) of
        {ok, Invoice} ->
            {200, [], Invoice};
        {error, not_found} ->
            {404, [], general_error(<<"Entity not found">>)}
    end;

handle_request(OperationID = 'GetInvoiceEvents', _Req, _Context) ->
    _ = lager:info("Processing operation ~p", [OperationID]),
    AllEvents = lists:map(
        fun([E]) -> E end,
        get_data_by_pattern({{'_', event}, '$1'})
    ),
    {200, [], AllEvents};

handle_request(OperationID = 'GetPaymentByID', Req, _Context) ->
    _ = lager:info("Processing operation ~p", [OperationID]),
    PaymentID = maps:get(paymentID, Req),
    case get_data_by_id(PaymentID, payment) of
        {ok, Payment} ->
            {200, [], Payment};
        {error, not_found} ->
            {404, [], general_error(<<"Entity not found">>)}
    end;

handle_request(_OperationID, _Req, _Context) ->
    {501, [], <<"Not implemented">>}.


%%%

-type callref() :: {pid(), Tag :: reference()}.
-type st() :: #state{}.

-spec init( Args :: any()) -> {ok, st()}.

init(_Args) ->
    TID = ets:new(mock_storage, [ordered_set, private, {heir, none}]),
    {ok, #state{tid = TID}}.

-spec handle_call(Request :: any(), From :: callref(), st()) -> {reply, term(), st()} | {noreply, st()}.

handle_call({put, ID, Type, Data}, _From, State = #state{tid = TID}) ->
    true = ets:insert(TID, {wrap_id(ID, Type), Data}),
    {reply, ok, State};

handle_call({get, ID, Type}, _From, State = #state{tid = TID}) ->
    Result = case ets:lookup(TID, wrap_id(ID, Type)) of
        [{_, Data}] -> {ok, Data};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({match, Pattern}, _From, State = #state{tid = TID}) ->
    Result = ets:match(TID, Pattern),
    {reply, Result, State};

handle_call({delete, Pattern}, _From, State = #state{tid = TID}) ->
    true = ets:match_delete(TID, Pattern),
    {reply, ok, State};

handle_call(id, _From, State = #state{last_id = ID}) ->
    NewID = ID + 1,
    {reply, NewID, State#state{last_id = NewID}}.

-spec handle_cast(Request :: any(), st()) -> {noreply, st()}.

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(any(), st()) -> {noreply, st()}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), st()) -> ok.

terminate(_Reason, _State) ->
    ok.

-spec code_change(Vsn :: term() | {down, Vsn :: term()}, st(), term()) -> {error, noimpl}.

code_change(_OldVsn, _State, _Extra) ->
    {error, noimpl}.

-spec put_data(ID :: any(), Type :: atom(), Data :: any()) -> ok.

put_data(ID, Type, Data) ->
    gen_server:call(?MODULE, {put, ID, Type, Data}).

-spec get_data_by_id(ID :: any(), Type :: atom()) -> {ok, Data :: any()} | {error, Reason :: any()}.

get_data_by_id(ID, Type) ->
    gen_server:call(?MODULE, {get, ID, Type}).

-spec get_data_by_pattern(Pattern :: ets:match_pattern()) -> [Data :: any()].

get_data_by_pattern(Pattern) ->
    gen_server:call(?MODULE, {match, Pattern}).

-spec delete_data(Pattern :: ets:match_pattern()) -> ok.

delete_data(Pattern) ->
    gen_server:call(?MODULE, {delete, Pattern}).

-spec new_id() -> ID :: binary().

new_id() ->
    ID = gen_server:call(?MODULE, id),
    genlib:to_binary(ID).

tokenize_payment_tool(Params = #{<<"paymentToolType">> := <<"cardData">>}) ->
    CardNumber = genlib:to_binary(maps:get(<<"cardNumber">>, Params)),
    ExpDate = maps:get(<<"expDate">>, Params),
    erlang:md5(<<CardNumber/binary, ExpDate/binary>>);

tokenize_payment_tool(_) ->
    error(unsupported_payment_tool). %%@TODO move this error to upper level

generate_session() ->
    genlib:unique().

-spec exhaust_session(Session :: any()) -> ok | error.

exhaust_session(Session) ->
    Pattern = {{'$1', session}, Session},
    case get_data_by_pattern(Pattern) of
        [_ID] ->
            delete_data(Pattern);
        [] -> {error, expired}
    end.

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

add_delayed_fake_payment(PaymentID) ->
    spawn(
        fun() ->
            _ = random:seed(erlang:system_time(milli_seconds)),
            timer:sleep(random:uniform(3) * 200),
            add_fake_payment(PaymentID)
        end
    ).

add_fake_payment(PaymentID) ->
    {ok, Payment} = get_data_by_id(PaymentID, payment),
    put_data(PaymentID, payment, Payment#{
        <<"status">> => <<"paid">>
    }),

    {{Y, M, D}, Time} = calendar:local_time(),
    {ok, CreatedAt} = rfc3339:format({{Y + 1, M, D}, Time}),
    EventID = genlib:to_int(new_id()),
    Event = #{
        <<"id">> => EventID,
        <<"createdAt">> => CreatedAt,
        <<"eventType">> => <<"paymentStatusChanged">>,
        <<"eventBody">> => #{
            <<"paymentID">> => PaymentID,
            <<"status">> => <<"paid">>
        }
    },
    put_data(EventID, event, Event).

wrap_id(ID, Type) ->
    {ID, Type}.
