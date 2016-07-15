-module(capi_mock_handler).

-behaviour(swagger_logic_handler).
-behaviour(gen_server).

%% API callbacks
-export([start_link/0]).
-export([handle_request/2]).
-export([authorize_api_key/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    tid,
    last_id = 0
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

authorize_api_key(ApiKey, OperationID) -> capi_auth:auth_api_key(ApiKey, OperationID).

handle_request('CreateInvoice', Req) ->
    Params = maps:get('CreateInvoiceArgs', Req),
    ID = new_id(),
    Invoice = #{
        <<"id">> => new_id(),
        <<"amount">> => maps:get(<<"amount">>, Params),
        <<"context">> => maps:get(<<"context">>, Params),
        <<"currency">> => maps:get(<<"currency">>, Params),
        <<"description">> => maps:get(<<"description">>, Params),
        <<"dueDate">> => maps:get(<<"dueDate">>, Params),
        <<"product">> => maps:get(<<"product">>, Params),
        <<"shopID">> => maps:get(<<"shopID">>, Params)
    },
    put_data(ID, Invoice),
    Resp = #{
        <<"id">> => ID
    },
    {201, [], Resp};

handle_request('CreatePayment', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('CreatePaymentToolToken', Req) ->
    Params = maps:get('PaymentTool', Req),
    Token = tokenize_payment_tool(Params),
    put_data(new_id(), Token),
    Resp = #{
        <<"token">> => Token,
        <<"session">> => generate_session()
    },
    {201, [], Resp};

handle_request('CreateProfile', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('DeleteProfile', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetInvoiceByID', _Req = #{<<"id">> := ID}) ->
    Invoice = get_data(ID),
    {200, [], Invoice};

handle_request('GetInvoiceEvents', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetPaymentByID', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetProfileByID', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetProfiles', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('UpdateProfile', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request(OperationID, Req) ->
    io:format(user, "Got request to process: ~p~n", [{OperationID, Req}]),
    {501, [], <<"Not implemented">>}.


%%%
init(_Args) ->
    TID = ets:new(mock_storage, [ordered_set, private, {heir, none}]),
    {ok, #state{tid = TID}}.

handle_call({put, ID, Data}, _From, State = #state{tid = TID}) ->
    Result = ets:insert(TID, {ID, Data}),
    {reply, Result, State};

handle_call({get, ID}, _From, State = #state{tid = TID}) ->
    Result = ets:lookup(TID, ID),
    {reply, Result, State};

handle_call(id, _From, State = #state{last_id = ID}) ->
    NewID = ID + 1,
    {reply, NewID, State#state{last_id = NewID}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

put_data(ID, Data) ->
    gen_server:call(?MODULE, {put, ID, Data}).

get_data(ID) ->
    gen_server:call(?MODULE, {get, ID}).

new_id() ->
    ID = gen_server:call(?MODULE, id),
    integer_to_binary(ID).

tokenize_payment_tool(Params = #{<<"paymentToolType">> := <<"cardData">>}) ->
    CardNumber = genlib:to_binary(maps:get(<<"cardNumber">>, Params)),
    ExpDate = maps:get(<<"expDate">>, Params),
    erlang:md5(<<CardNumber/binary, ExpDate/binary>>);

tokenize_payment_tool(_) ->
    error(unsupported_payment_tool). %%@TODO move this error to upper level

generate_session() ->
    integer_to_binary(rand:uniform(100000)).
