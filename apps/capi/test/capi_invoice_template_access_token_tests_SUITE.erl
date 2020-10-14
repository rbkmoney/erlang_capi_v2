-module(capi_invoice_template_access_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
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
    create_invoice_with_tpl_ok_test/1,
    get_invoice_template_ok_test/1,
    get_invoice_payment_methods_by_tpl_id_ok_test/1
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
        {group, operations_by_invoice_template_access_token}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_invoice_template_access_token, [],
            [
                create_invoice_with_tpl_ok_test,
                get_invoice_template_ok_test,
                get_invoice_payment_methods_by_tpl_id_ok_test
            ]
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
init_per_group(operations_by_invoice_template_access_token, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    {ok, Token} = capi_ct_helper:issue_token([{[party], write}], unlimited),
    capi_ct_helper:mock_services([{invoice_templating, fun('Create', _) -> {ok, ?INVOICE_TPL} end}], MockServiceSup),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"details">> => #{
            <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
            <<"product">> => <<"test_invoice_template_product">>,
            <<"price">> => #{
                <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
                <<"currency">> => ?RUB,
                <<"amount">> => ?INTEGER
            }
        },
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    {ok, #{
            <<"invoiceTemplateAccessToken">> := #{<<"payload">> := InvTemplAccToken}
        }
    } = capi_client_invoice_templates:create(capi_ct_helper:get_context(Token), Req),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(InvTemplAccToken)} | Config];

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

-spec create_invoice_with_tpl_ok_test(config()) ->
    _.
create_invoice_with_tpl_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {generator, fun('GenerateID', _) ->
            capi_ct_helper_bender:generate_id(<<"bender_key">>) end},
        {invoice_templating, fun('Get', _) -> {ok, ?INVOICE_TPL} end},
        {invoicing, fun('CreateWithTemplate', _) -> {ok, ?PAYPROC_INVOICE} end}
    ], Config),
    Req = #{
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>}
    },
    {ok, _} = capi_client_invoice_templates:create_invoice(?config(context, Config), ?STRING, Req).

-spec get_invoice_template_ok_test(config()) ->
    _.
get_invoice_template_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoice_templating, fun('Get', _) -> {ok, ?INVOICE_TPL} end}], Config),
    {ok, _} = capi_client_invoice_templates:get_template_by_id(?config(context, Config), ?STRING).

-spec get_invoice_payment_methods_by_tpl_id_ok_test(config()) ->
    _.
get_invoice_payment_methods_by_tpl_id_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoice_templating, fun('ComputeTerms', _) -> {ok, ?TERM_SET} end},
                                  {party_management,   fun('Get', _)          -> {ok, ?PARTY}    end}], Config),
    {ok, _} = capi_client_invoice_templates:get_invoice_payment_methods(?config(context, Config), ?STRING).
