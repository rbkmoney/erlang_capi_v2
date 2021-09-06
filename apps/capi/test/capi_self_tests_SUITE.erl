-module(capi_self_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
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
    oops_body_test/1,
    schema_param_validation/1,
    query_param_validation/1
]).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-define(OOPS_BODY, filename:join(?config(data_dir, Config), "cutest_cat_alive")).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, stream_handler_tests},
        {group, validation_tests}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {stream_handler_tests, [], [
            oops_body_test
        ]},
        {validation_tests, [], [
            schema_param_validation,
            query_param_validation
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config, [
        {oops_bodies, #{
            500 => ?OOPS_BODY
        }}
    ]).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(GroupName, Config) when stream_handler_tests =:= GroupName; validation_tests =:= GroupName ->
    Context = capi_ct_helper:get_context(capi_ct_helper:issue_token(unlimited)),
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_token_keeper:mock_user_session_token(SupPid),
    Apps2 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{context, Context}, {group_apps, Apps1 ++ Apps2}, {group_test_sup, SupPid} | Config];
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
    _ = capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec oops_body_test(config()) -> _.
oops_body_test(Config) ->
    _ = capi_ct_helper:mock_services([{invoicing, fun('Get', _) -> {ok, "spanish inquisition"} end}], Config),
    Context = ?config(context, Config),
    Params = #{binding => #{<<"invoiceID">> => ?STRING}},
    {Endpoint, PreparedParams, Opts0} = capi_client_lib:make_request(Context, Params),
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

-spec schema_param_validation(config()) -> _.
schema_param_validation(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"key">>)} end}
        ],
        Config
    ),
    Req0 = #{
        <<"shopID">> => ?STRING,
        <<"amount">> => <<"vry much">>,
        <<"currency">> => <<"green paper">>,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">> => <<"asap">>,
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    {error, {request_validation_failed, _}} =
        capi_client_invoices:create_invoice(?config(context, Config), Req0).

-spec query_param_validation(config()) -> _.
query_param_validation(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end},
            {geo_ip_service, fun('GetLocationName', _) -> {ok, #{123 => ?STRING}} end}
        ],
        Config
    ),
    Query0 = [
        {payerEmail, <<"te%^st@test.ru">>}
    ],
    {error, {request_validation_failed, _}} =
        capi_client_searches:search_invoices(?config(context, Config), ?STRING, Query0),
    Query1 = #{
        <<"geoIDs">> => <<"no,also no">>,
        <<"language">> => <<"ru">>
    },
    {error, {request_validation_failed, _}} =
        capi_client_geo:get_location_names(?config(context, Config), Query1).
