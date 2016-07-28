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

-spec authorize_api_key(ApiKey :: binary(), OperationID :: atom()) -> Result :: boolean() | {boolean(), #{binary() => any()}}.
authorize_api_key(ApiKey, OperationID) -> capi_auth:auth_api_key(ApiKey, OperationID).

-spec handle_request(OperationID :: atom(), Req :: #{}, Context :: #{}) -> {Code :: integer, Headers :: [], Response :: #{}}.
handle_request('CreateInvoice', Req, _Context) ->
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

handle_request('CreatePayment', Req, _Context) ->
    InvoiceID = maps:get('invoice_id', Req),
    PaymentParams = maps:get('CreatePaymentArgs', Req),
    PaymentSession = maps:get(<<"paymentSession">>, PaymentParams),
    case match_data({{'$1', session}, PaymentSession}) of
        [[_SessionID]] ->
            delete_data({{'$1', session}, '_'}),
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
            {201, [], Resp};
        _ ->
            Resp = logic_error(<<"expired_session">>, <<"Payment session is not valid">>),
            {400, [], Resp}
    end;

handle_request('CreatePaymentToolToken', Req, _Context) ->
    Params = maps:get('PaymentTool', Req),
    Token = tokenize_payment_tool(Params),
    put_data(new_id(), token, Token),
    Session = generate_session(),
    put_data(new_id(), session, Session),
    Resp = #{
        <<"token">> => Token,
        <<"session">> => Session
    },
    {201, [], Resp};

handle_request('GetInvoiceByID', Req, _Context) ->
    InvoiceID = maps:get(invoice_id, Req),
    [{_, Invoice}] = get_data(InvoiceID, invoice),
    {200, [], Invoice};

handle_request('GetInvoiceEvents', _Req, _Context) ->
    Events = [],
    {200, [], Events};

handle_request('GetPaymentByID', Req, _Context) ->
    PaymentID = maps:get(payment_id, Req),
    [{_, Payment}] = get_data(PaymentID, payment),
    {200, [], Payment};

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
    Result = ets:insert(TID, {wrap_id(ID, Type), Data}),
    {reply, Result, State};

handle_call({get, ID, Type}, _From, State = #state{tid = TID}) ->
    Result = ets:lookup(TID, wrap_id(ID, Type)),
    {reply, Result, State};

handle_call({match, Pattern}, _From, State = #state{tid = TID}) ->
    Result = ets:match(TID, Pattern),
    {reply, Result, State};

handle_call({delete, Pattern}, _From, State = #state{tid = TID}) ->
    Result = ets:match_delete(TID, Pattern),
    {reply, Result, State};

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

put_data(ID, Type, Data) ->
    gen_server:call(?MODULE, {put, ID, Type, Data}).

get_data(ID, Type) ->
    gen_server:call(?MODULE, {get, ID, Type}).

match_data(Pattern) ->
    gen_server:call(?MODULE, {match, Pattern}).

delete_data(Pattern) ->
    gen_server:call(?MODULE, {delete, Pattern}).

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
    integer_to_binary(rand:uniform(100000)).

logic_error(Code, Message) ->
    #{code => Code, message => Message}.

wrap_id(ID, Type) ->
    {ID, Type}.
