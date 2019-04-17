-module(capi_customer_access_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
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
    get_customer_ok_test/1,
    create_binding_ok_test/1,
    get_bindings_ok_test/1,
    get_binding_ok_test/1,
    get_customer_events_ok_test/1
]).

-define(CAPI_PORT                   , 8080).
-define(CAPI_HOST_NAME              , "localhost").
-define(CAPI_URL                    , ?CAPI_HOST_NAME ++ ":" ++ integer_to_list(?CAPI_PORT)).

-define(badresp(Code), {error, {invalid_response_code, Code}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, operations_by_customer_access_token_after_customer_creation},
        {group, operations_by_customer_access_token_after_token_creation}
    ].

customer_access_token_tests() ->
    [
        get_customer_ok_test,
        create_binding_ok_test,
        get_bindings_ok_test,
        get_binding_ok_test,
        get_customer_events_ok_test
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_customer_access_token_after_customer_creation, [],
            customer_access_token_tests()
        },
        {operations_by_customer_access_token_after_token_creation, [],
            customer_access_token_tests()
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(operations_by_customer_access_token_after_customer_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    {ok, Token} = capi_ct_helper:issue_token([{[customers], write}], unlimited),
    capi_ct_helper:mock_services([{customer_management, fun('Create', _) -> {ok, ?CUSTOMER} end}], MockServiceSup),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"contactInfo">> => #{<<"email">> => <<"bla@bla.ru">>},
        <<"metadata">> => #{<<"text">> => [<<"SOMESHIT">>, 42]}
    },
    {ok, #{
            <<"customerAccessToken">> := #{<<"payload">> := CustAccToken}
        }
    } = capi_client_customers:create_customer(capi_ct_helper:get_context(Token), Req),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(CustAccToken)} | Config];

init_per_group(operations_by_customer_access_token_after_token_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    {ok, Token} = capi_ct_helper:issue_token([{[customers], write}], unlimited),
    capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], MockServiceSup),
    {ok,
        #{<<"payload">> := CustAccToken}
    } = capi_client_customers:create_customer_access_token(capi_ct_helper:get_context(Token), ?STRING),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(CustAccToken)}| Config];

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

-spec get_customer_ok_test(config()) ->
    _.
get_customer_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_customer_by_id(?config(context, Config), ?STRING).

-spec create_binding_ok_test(config()) ->
    _.
create_binding_ok_test(Config) ->
    capi_ct_helper:mock_services(
        [
            {cds_storage, fun
                ('PutSession', _) -> {ok, ok};
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT}
            end},
            {customer_management, fun('StartBinding', _) -> {ok, ?CUSTOMER_BINDING} end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender key">>)} end},
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
        ],
        Config
    ),
    Req1 = #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"cardNumber">> => <<"4111111111111111">>,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => #{
            <<"fingerprint">> => <<"test fingerprint">>
        }
    },
    {ok, #{
        <<"paymentToolToken">> := Token,
        <<"paymentSession">> := Session
    }} = capi_client_tokens:create_payment_resource(?config(context, Config), Req1),
    Req2 = #{
        <<"paymentResource">> => #{
            <<"paymentSession">> => Session,
            <<"paymentToolToken">> => Token
        }
    },
    {ok, _} = capi_client_customers:create_binding(?config(context, Config), ?STRING, Req2).


-spec get_bindings_ok_test(config()) ->
    _.
get_bindings_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_bindings(?config(context, Config), ?STRING).

-spec get_binding_ok_test(config()) ->
    _.
get_binding_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_binding(?config(context, Config), ?STRING, ?STRING).

-spec get_customer_events_ok_test(config()) ->
    _.
get_customer_events_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('GetEvents', _) -> {ok, []} end}], Config),
    {ok, _} = capi_client_customers:get_customer_events(?config(context, Config), ?STRING, 10).
