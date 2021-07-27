-module(capi_deadline_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("capi_dummy_data.hrl").

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
    deadline_error_test/1,
    deadline_absolute_ok_test/1,
    deadline_relative_ok_test/1
]).

-define(badresp(Code), {error, {invalid_response_code, Code}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, deadline_header}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {deadline_header, [], [
            deadline_error_test,
            deadline_absolute_ok_test,
            deadline_relative_ok_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(deadline_header, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_tk:mock_service(capi_ct_helper_tk:user_session_handler(), SupPid),
    Apps2 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{group_apps, Apps1 ++ Apps2}, {group_test_sup, SupPid} | Config];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = capi_utils:maybe(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec deadline_error_test(config()) -> _.
deadline_error_test(_Config) ->
    Context = capi_ct_helper:get_context(capi_ct_helper:issue_token(unlimited), <<"blabla">>),
    {error, {400, _}} = capi_client_categories:get_categories(Context).

-spec deadline_absolute_ok_test(config()) -> _.
deadline_absolute_ok_test(Config) ->
    Context = capi_ct_helper:get_context(capi_ct_helper:issue_token(unlimited)),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                timer:sleep(10000),
                {ok, ?PAYPROC_INVOICE}
            end}
        ],
        Config
    ),
    Deadline = woody_deadline:from_timeout(3000),
    ?badresp(504) = capi_client_invoices:get_invoice_by_id(Context#{deadline => Deadline}, ?STRING),
    Deadline2 = woody_deadline:from_timeout(3000),
    {ok, _} = capi_client_categories:get_categories(Context#{deadline => Deadline2}).

-spec deadline_relative_ok_test(config()) -> _.
deadline_relative_ok_test(Config) ->
    DeadlineRelative = <<"3s">>,
    Context = capi_ct_helper:get_context(capi_ct_helper:issue_token(unlimited), DeadlineRelative),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                timer:sleep(10000),
                {ok, ?PAYPROC_INVOICE}
            end}
        ],
        Config
    ),
    ?badresp(504) = capi_client_invoices:get_invoice_by_id(Context, ?STRING),
    {ok, _} = capi_client_categories:get_categories(Context).
