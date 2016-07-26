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
    put_data(ID, Invoice),
    Resp = #{
        <<"id">> => ID
    },
    {201, [], Resp};

handle_request('CreatePayment', Req) ->
    InvoiceID = maps:get('invoice_id', Req),
    PaymentParams = maps:get('CreatePaymentArgs', Req),
    ID = new_id(),
    Payment = #{
        <<"id">> => ID ,
        <<"invoiceID">> => InvoiceID,
        <<"createdAt">> => <<"2016-12-12 17:00:00">>,
        <<"status">> => <<"pending">>,
        <<"paymentToolToken">> => maps:get(<<"paymentToolToken">>, PaymentParams)
    },
    put_data(ID, Payment),
    Resp = #{
        <<"id">> => ID
    },
    {201, [], Resp};

handle_request('CreatePaymentToolToken', Req) ->
    Params = maps:get('PaymentTool', Req),
    Token = tokenize_payment_tool(Params),
    put_data(new_id(), Token),
    Resp = #{
        <<"token">> => Token,
        <<"session">> => generate_session()
    },
    {201, [], Resp};

handle_request('GetInvoiceByID', Req) ->
    InvoiceID = maps:get(invoice_id, Req),
    [{_, Invoice}] = get_data(InvoiceID),
    {200, [], Invoice};

handle_request('GetInvoiceEvents', _Req) ->
    Events = [],
    {200, [], Events};

handle_request('GetPaymentByID', Req) ->
    PaymentID = maps:get(payment_id, Req),
    [{_, Payment}] = get_data(PaymentID),
    {200, [], Payment};

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
