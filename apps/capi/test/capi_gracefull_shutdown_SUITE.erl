-module(capi_gracefull_shutdown_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include("capi_dummy_data.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([test/1]).

-behaviour(supervisor).
-export([init/1]).

-define(NUMBER_OF_WORKERS, 10).

-type test_case_name() :: atom().
-type config()         :: [{atom(), any()}].

-spec all() -> [test_case_name()].

all() ->
    [test].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    capi_ct_helper:init_suite(?MODULE, C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- ?config(apps, C)],
    ok.

-spec test(config()) -> _.

test(Config0) ->
    Config = [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | Config0],
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', _)     -> ok = timer:sleep(2000), {ok, ?PAYPROC_INVOICE} end},
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"key">>)} end}
    ], Config),
    Token = get_token(),
    ok = spawn_workers(Token, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = application:stop(capi),
    ok = receive_loop(fun(Result) -> {ok, _} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(Token, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun(Result) -> {error, econnrefused} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%%

receive_loop(_, N, _Timeout) when N =< 0 ->
    ok;
receive_loop(MatchFun, N, Timeout) ->
    receive
        {result, Result} ->
            MatchFun(Result)
    after Timeout ->
        error(timeout)
    end,
    receive_loop(MatchFun, N - 1, Timeout).

spawn_workers(_, _, N) when N =< 0 ->
    ok;
spawn_workers(Token, ParentPID, N) ->
    erlang:spawn_link(fun() -> worker(Token, ParentPID) end),
    spawn_workers(Token, ParentPID, N - 1).

worker(Token, ParentPID) ->
    Context = get_context(Token),
    Req = #{
        <<"shopID">>      => ?STRING,
        <<"amount">>      => ?INTEGER,
        <<"currency">>    => ?RUB,
        <<"metadata">>    => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">>     => ?TIMESTAMP,
        <<"product">>     => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    Result = capi_client_invoices:create_invoice(Context, Req),
    ParentPID ! {result, Result}.

get_context(Token) ->
    capi_ct_helper:get_context(Token).

get_token() ->
    BasePermissions = [
        {[invoices], write},
        {[invoices], read}
    ],
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    Token.
