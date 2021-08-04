-module(capi_authorization_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("capi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
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
    authorization_error_no_header_test/1,
    authorization_error_no_permission_test/1
]).

-export([
    get_party_forbidden_notfound/1,
    get_invoice_forbidden_notfound/1,
    get_invoice_by_external_id_forbidden_notfound/1,
    get_payment_by_external_id_forbidden_notfound/1,
    get_refund_by_external_id_forbidden_notfound/1,
    get_customer_forbidden_notfound/1
]).

-define(emptyresp(Code), {error, {Code, #{}}}).

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
        {group, authorization},
        {group, forbidden_masking}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {authorization, [], [
            authorization_error_no_header_test,
            authorization_error_no_permission_test
        ]},
        {forbidden_masking, [], [
            get_party_forbidden_notfound,
            get_invoice_forbidden_notfound,
            get_invoice_by_external_id_forbidden_notfound,
            get_payment_by_external_id_forbidden_notfound,
            get_refund_by_external_id_forbidden_notfound,
            get_customer_forbidden_notfound
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

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(GroupName, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_tk:mock_service(capi_ct_helper_tk:user_session_handler(), SupPid),
    Apps2 =
        case GroupName of
            forbidden_masking ->
                capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_forbidden(), SupPid);
            _ ->
                []
        end,
    [{group_apps, Apps1 ++ Apps2}, {group_test_sup, SupPid} | Config].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = capi_utils:maybe(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1),
    ok.

%%% Tests

-spec authorization_error_no_header_test(config()) -> _.
authorization_error_no_header_test(Config) ->
    Token = <<>>,
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    ?emptyresp(401) = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_error_no_permission_test(config()) -> _.
authorization_error_no_permission_test(Config) ->
    Token = ?API_TOKEN,
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_forbidden(), Config),
    ?emptyresp(401) = capi_client_parties:get_my_party(capi_ct_helper:get_context(Token)).

%%%

-spec get_party_forbidden_notfound(config()) -> _.
-spec get_invoice_forbidden_notfound(config()) -> _.
-spec get_invoice_by_external_id_forbidden_notfound(config()) -> _.
-spec get_payment_by_external_id_forbidden_notfound(config()) -> _.
-spec get_refund_by_external_id_forbidden_notfound(config()) -> _.
-spec get_customer_forbidden_notfound(config()) -> _.

get_party_forbidden_notfound(Config) ->
    PartyID = <<"NONEXISTENT">>,
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {throwing, #payproc_PartyNotFound{}}
            end}
        ],
        Config
    ),
    {error, {404, _}} = capi_client_parties:get_party_by_id(mk_context(), PartyID).

get_invoice_forbidden_notfound(Config) ->
    InvoiceID = <<"NONEXISTENT">>,
    _ = capi_ct_helper:mock_services(
        [{invoicing, fun('Get', _) -> {throwing, #payproc_InvoiceNotFound{}} end}],
        Config
    ),
    {error, {404, _}} = capi_client_invoices:get_invoice_by_id(mk_context(), InvoiceID).

get_invoice_by_external_id_forbidden_notfound(Config) ->
    ExternalID = <<"NEVERWAS">>,
    _ = capi_ct_helper:mock_services(
        [{bender, fun('GetInternalID', _) -> {throwing, capi_ct_helper_bender:no_internal_id()} end}],
        Config
    ),
    {error, {404, _}} = capi_client_invoices:get_invoice_by_external_id(mk_context(), ExternalID).

get_payment_by_external_id_forbidden_notfound(Config) ->
    ExternalID = <<"NEVERWAS">>,
    _ = capi_ct_helper:mock_services(
        [{bender, fun('GetInternalID', _) -> {throwing, capi_ct_helper_bender:no_internal_id()} end}],
        Config
    ),
    {error, {404, _}} = capi_client_payments:get_payment_by_external_id(mk_context(), ExternalID).

get_refund_by_external_id_forbidden_notfound(Config) ->
    ExternalID = <<"WHEREDIDYOUGETIT">>,
    _ = capi_ct_helper:mock_services(
        [{bender, fun('GetInternalID', _) -> {throwing, capi_ct_helper_bender:no_internal_id()} end}],
        Config
    ),
    {error, {404, _}} = capi_client_payments:get_refund_by_external_id(mk_context(), ExternalID).

get_customer_forbidden_notfound(Config) ->
    CustomerID = <<"NONEXISTENT">>,
    _ = capi_ct_helper:mock_services(
        [{customer_management, fun('Get', _) -> {throwing, #payproc_CustomerNotFound{}} end}],
        Config
    ),
    {error, {404, _}} = capi_client_customers:get_customer_by_id(mk_context(), CustomerID).

mk_context() ->
    capi_ct_helper:get_context(?API_TOKEN).
