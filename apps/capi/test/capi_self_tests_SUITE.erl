-module(capi_self_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("jose/include/jose_jwk.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    oops_body_test/1
]).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-define(OOPS_BODY, filename:join(?config(data_dir, Config), "cutest_cat_alive")).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, stream_handler_tests}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {stream_handler_tests, [],
            [
                oops_body_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config, [{oops_bodies, #{
        500 => ?OOPS_BODY
    }}]).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(stream_handler_tests, Config) ->
    BasePermissions = [
        {[invoices], write},
        {[invoices], read},
        {[party], write},
        {[party], read},
        {[invoices, payments], write},
        {[invoices, payments], read},
        {[customers], write},
        {[payouts], write},
        {[payouts], read}
    ],
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    Context = capi_ct_helper:get_context(Token),
    [{context, Context} | Config];

init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec oops_body_test(config()) ->
    _.

oops_body_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, "spanish inquisition"} end}], Config),
    Context = ?config(context, Config),
    {Endpoint, PreparedParams, Opts0} = capi_client_lib:make_request(Context, #{}),
    Url = swag_client_utils:get_url(Endpoint, "/v2/processing/me"),
    Headers = maps:to_list(maps:get(header, PreparedParams)),
    Body = <<"{}">>,
    Opts = Opts0 ++ [with_body],
    {ok, 500, _, OopsBody} = hackney:request(
        get,
        Url,
        Headers,
        Body,
        Opts
    ),
    {ok, OopsBody} = file:read_file(?OOPS_BODY).
