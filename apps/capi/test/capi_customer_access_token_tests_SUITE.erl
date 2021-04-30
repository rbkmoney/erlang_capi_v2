-module(capi_customer_access_token_tests_SUITE).

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
    get_customer_ok_test/1,
    create_binding_ok_test/1,
    create_binding_expired_test/1,
    get_bindings_ok_test/1,
    get_binding_ok_test/1,
    get_customer_events_ok_test/1
]).

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
        {group, operations_by_customer_access_token_after_customer_creation},
        {group, operations_by_customer_access_token_after_token_creation}
    ].

customer_access_token_tests() ->
    [
        get_customer_ok_test,
        create_binding_ok_test,
        create_binding_expired_test,
        get_bindings_ok_test,
        get_binding_ok_test,
        get_customer_events_ok_test
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_customer_access_token_after_customer_creation, [], customer_access_token_tests()},
        {operations_by_customer_access_token_after_token_creation, [], customer_access_token_tests()}
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
init_per_group(operations_by_customer_access_token_after_customer_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Create', _) -> {ok, ?CUSTOMER} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        MockServiceSup
    ),
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), MockServiceSup),
    {ok, Token} = capi_ct_helper:issue_token([{[customers], write}], unlimited),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"contactInfo">> => #{<<"email">> => <<"bla@bla.ru">>},
        <<"metadata">> => #{<<"text">> => [<<"SOMESHIT">>, 42]}
    },
    {ok, #{
        <<"customerAccessToken">> := #{<<"payload">> := CustAccToken}
    }} = capi_client_customers:create_customer(capi_ct_helper:get_context(Token), Req),
    _ = capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{context, capi_ct_helper:get_context(CustAccToken)}, {group_apps, Apps}, {group_test_sup, SupPid} | Config];
init_per_group(operations_by_customer_access_token_after_token_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    _ = capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], MockServiceSup),
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), MockServiceSup),
    {ok, Token} = capi_ct_helper:issue_token([{[customers], write}], unlimited),
    {ok, #{<<"payload">> := CustAccToken}} = capi_client_customers:create_customer_access_token(
        capi_ct_helper:get_context(Token),
        ?STRING
    ),
    _ = capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{context, capi_ct_helper:get_context(CustAccToken)}, {group_apps, Apps}, {group_test_sup, SupPid} | Config];
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

-spec get_customer_ok_test(config()) -> _.
get_customer_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_customer_by_id(?config(context, Config), ?STRING).

-spec create_binding_ok_test(config()) -> _.
create_binding_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun
                ('Get', _) -> {ok, ?CUSTOMER};
                ('StartBinding', _) -> {ok, ?CUSTOMER_BINDING}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    PaymentToolToken = ?TEST_PAYMENT_TOKEN,
    Req2 = #{
        <<"paymentResource">> => #{
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => PaymentToolToken
        }
    },
    {ok, _} = capi_client_customers:create_binding(?config(context, Config), ?STRING, Req2).

-spec create_binding_expired_test(config()) -> _.
create_binding_expired_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    PaymentTool = {bank_card, ?BANK_CARD},
    ValidUntil = capi_utils:deadline_from_timeout(0),
    PaymentToolToken = capi_crypto:create_encrypted_payment_tool_token(PaymentTool, ValidUntil),
    Req = #{
        <<"paymentResource">> => #{
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => PaymentToolToken
        }
    },
    Resp = capi_client_customers:create_binding(?config(context, Config), ?STRING, Req),
    {error, {400, #{<<"code">> := <<"invalidPaymentToolToken">>}}} = Resp.

-spec get_bindings_ok_test(config()) -> _.
get_bindings_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}
        ],
        Config
    ),
    {ok, _} = capi_client_customers:get_bindings(?config(context, Config), ?STRING).

-spec get_binding_ok_test(config()) -> _.
get_binding_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_binding(?config(context, Config), ?STRING, ?STRING).

-spec get_customer_events_ok_test(config()) -> _.
get_customer_events_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun
                ('Get', _) -> {ok, ?CUSTOMER};
                ('GetEvents', _) -> {ok, []}
            end}
        ],
        Config
    ),
    {ok, _} = capi_client_customers:get_customer_events(?config(context, Config), ?STRING, 10).
