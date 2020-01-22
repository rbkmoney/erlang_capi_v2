-module(capi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_accounter_thrift.hrl").
-include_lib("damsel/include/dmsl_cds_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_tool_provider_thrift.hrl").
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
    woody_unexpected_test/1,
    woody_unavailable_test/1,
    woody_retry_test/1,
    woody_unknown_test/1,

    authorization_positive_lifetime_ok_test/1,
    authorization_unlimited_lifetime_ok_test/1,
    authorization_far_future_deadline_ok_test/1,
    authorization_permission_ok_test/1,
    authorization_negative_lifetime_error_test/1,
    authorization_bad_deadline_error_test/1,
    authorization_error_no_header_test/1,
    authorization_error_no_permission_test/1,
    authorization_bad_token_error_test/1,

    create_invoice_ok_test/1,
    get_invoice_ok_test/1,
    get_invoice_events_ok_test/1,
    get_invoice_payment_methods_ok_test/1,
    create_invoice_access_token_ok_test/1,
    rescind_invoice_ok_test/1,
    fulfill_invoice_ok_test/1,

    create_invoice_with_tpl_ok_test/1,
    create_invoice_template_ok_test/1,
    get_invoice_template_ok_test/1,
    update_invoice_template_ok_test/1,
    delete_invoice_template_ok_test/1,
    get_invoice_payment_methods_by_tpl_id_ok_test/1,

    get_account_by_id_ok_test/1,

    create_payment_ok_test/1,
    create_payment_with_encrypt_token_ok_test/1,
    get_payments_ok_test/1,
    get_payment_by_id_ok_test/1,
    create_refund/1,
    create_refund_idemp_ok_test/1,
    create_partial_refund/1,
    create_partial_refund_without_currency/1,
    get_refund_by_id/1,
    get_refunds/1,
    cancel_payment_ok_test/1,
    capture_payment_ok_test/1,

    get_my_party_ok_test/1,
    suspend_my_party_ok_test/1,
    activate_my_party_ok_test/1,

    get_shop_by_id_ok_test/1,
    get_shops_ok_test/1,
    activate_shop_ok_test/1,
    suspend_shop_ok_test/1,

    get_claim_by_id_ok_test/1,
    get_claims_ok_test/1,
    revoke_claim_ok_test/1,
    create_claim_ok_test/1,
    update_claim_by_id_test/1,

    get_contract_by_id_ok_test/1,
    get_contracts_ok_test/1,
    get_contract_adjustments_ok_test/1,
    get_contract_adjustment_by_id_ok_test/1,

    get_payout_tools_ok_test/1,
    get_payout_tool_by_id/1,

    create_webhook_ok_test/1,
    get_webhooks/1,
    get_webhook_by_id/1,
    delete_webhook_by_id/1,

    get_locations_names_ok_test/1,

    search_invoices_ok_test/1,
    search_payments_ok_test/1,
    search_payouts_ok_test/1,

    get_payment_conversion_stats_ok_test/1,
    get_payment_revenue_stats_ok_test/1,
    get_payment_geo_stats_ok_test/1,
    get_payment_rate_stats_ok_test/1,
    get_payment_method_stats_ok_test/1,

    get_reports_ok_test/1,
    download_report_file_ok_test/1,
    download_report_file_not_found_test/1,

    get_categories_ok_test/1,
    get_category_by_ref_ok_test/1,
    get_schedule_by_ref_ok_test/1,
    get_payment_institutions/1,
    get_payment_institution_by_ref/1,
    get_payment_institution_payment_terms/1,
    get_payment_institution_payout_terms/1,

    create_customer_ok_test/1,
    get_customer_ok_test/1,
    create_customer_access_token_ok_test/1,
    create_binding_ok_test/1,
    get_bindings_ok_test/1,
    get_binding_ok_test/1,
    get_customer_events_ok_test/1,
    delete_customer_ok_test/1
]).

-define(CAPI_IP                     , "::").
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
        {group, woody_errors},
        {group, operations_by_base_api_token},
        {group, operations_by_invoice_access_token_after_invoice_creation},
        {group, operations_by_invoice_access_token_after_token_creation},
        {group, operations_by_invoice_template_access_token},
        {group, operations_by_customer_access_token_after_customer_creation},
        {group, operations_by_customer_access_token_after_token_creation},
        {group, authorization}
    ].

invoice_access_token_tests() ->
    [
        get_invoice_ok_test,
        get_invoice_events_ok_test,
        get_invoice_payment_methods_ok_test,
        create_payment_ok_test,
        create_payment_with_encrypt_token_ok_test,
        get_payments_ok_test,
        get_payment_by_id_ok_test,
        cancel_payment_ok_test,
        capture_payment_ok_test
    ].

customer_access_token_tests() ->
    [
        get_customer_ok_test,
        create_binding_ok_test,
        get_bindings_ok_test,
        get_binding_ok_test,
        get_customer_events_ok_test
    ].

-spec test() -> _.

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {woody_errors, [],
            [
                woody_unexpected_test,
                woody_unavailable_test,
                woody_retry_test,
                woody_unknown_test
            ]
        },
        {operations_by_base_api_token, [],
            [
                create_invoice_ok_test,
                create_invoice_access_token_ok_test,
                create_invoice_template_ok_test,
                create_customer_ok_test,
                create_customer_access_token_ok_test,
                rescind_invoice_ok_test,
                fulfill_invoice_ok_test,
                create_refund,
                create_refund_idemp_ok_test,
                create_partial_refund,
                create_partial_refund_without_currency,
                get_refund_by_id,
                get_refunds,
                update_invoice_template_ok_test,
                delete_invoice_template_ok_test,
                get_account_by_id_ok_test,
                get_my_party_ok_test,
                suspend_my_party_ok_test,
                activate_my_party_ok_test,
                get_shop_by_id_ok_test,
                get_shops_ok_test,
                activate_shop_ok_test,
                suspend_shop_ok_test,
                get_claim_by_id_ok_test,
                get_claims_ok_test,
                revoke_claim_ok_test,
                create_claim_ok_test,
                update_claim_by_id_test,
                get_contract_by_id_ok_test,
                get_contracts_ok_test,
                get_contract_adjustments_ok_test,
                get_contract_adjustment_by_id_ok_test,
                get_payout_tools_ok_test,
                get_payout_tool_by_id,
                create_webhook_ok_test,
                get_webhooks,
                get_webhook_by_id,
                delete_webhook_by_id,
                get_locations_names_ok_test,
                search_invoices_ok_test,
                search_payments_ok_test,
                search_payouts_ok_test,
                get_payment_conversion_stats_ok_test,
                get_payment_revenue_stats_ok_test,
                get_payment_geo_stats_ok_test,
                get_payment_rate_stats_ok_test,
                get_payment_method_stats_ok_test,
                get_reports_ok_test,
                download_report_file_ok_test,
                download_report_file_not_found_test,
                get_categories_ok_test,
                get_category_by_ref_ok_test,
                get_schedule_by_ref_ok_test,
                get_payment_institutions,
                get_payment_institution_by_ref,
                get_payment_institution_payment_terms,
                get_payment_institution_payout_terms,
                delete_customer_ok_test
            ]
        },
        {operations_by_invoice_access_token_after_invoice_creation, [],
            invoice_access_token_tests()
        },
        {operations_by_invoice_access_token_after_token_creation, [],
            invoice_access_token_tests()
        },
        {operations_by_invoice_template_access_token, [],
            [
                create_invoice_with_tpl_ok_test,
                get_invoice_template_ok_test,
                get_invoice_payment_methods_by_tpl_id_ok_test
            ]
        },
        {operations_by_customer_access_token_after_customer_creation, [],
            customer_access_token_tests()
        },
        {operations_by_customer_access_token_after_token_creation, [],
            customer_access_token_tests()
        },
        {authorization, [],
            [
                authorization_positive_lifetime_ok_test,
                authorization_unlimited_lifetime_ok_test,
                authorization_far_future_deadline_ok_test,
                authorization_permission_ok_test,
                authorization_negative_lifetime_error_test,
                authorization_bad_deadline_error_test,
                authorization_error_no_header_test,
                authorization_error_no_permission_test,
                authorization_bad_token_error_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    SupPid = start_mocked_service_sup(),
    Apps1 =
        capi_ct_helper:start_app(woody),
    ServiceURLs = mock_services_([
        {
            'Repository',
            {dmsl_domain_config_thrift, 'Repository'},
            fun('Checkout', _) -> {ok, ?SNAPSHOT} end
        }
    ], SupPid),
    Apps2 =
        capi_ct_helper:start_app(dmt_client, [{max_cache_size, #{}}, {service_urls, ServiceURLs}]) ++
        start_capi(Config),
    [{apps, lists:reverse(Apps2 ++ Apps1)}, {suite_test_sup, SupPid} | Config].

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(operations_by_invoice_access_token_after_invoice_creation, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = issue_token([{[invoices], write}], unlimited),
    mock_services([
        {bender,    fun('GenerateID', _) ->
            {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {invoicing, fun('Create', [_, #payproc_InvoiceParams{id = <<"bender_key">>}]) ->
            {ok, ?PAYPROC_INVOICE} end}
    ], MockServiceSup),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">> => ?TIMESTAMP,
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    {ok,
        #{
            <<"invoiceAccessToken">> := #{<<"payload">> := InvAccToken}
        }
    } = capi_client_invoices:create_invoice(get_context(Token), Req),
    stop_mocked_service_sup(MockServiceSup),
    [{context, get_context(InvAccToken)} | Config];

init_per_group(operations_by_invoice_access_token_after_token_creation, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = issue_token([{[invoices], write}], unlimited),
    mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], MockServiceSup),
    {ok, #{<<"payload">> := InvAccToken}
    } = capi_client_invoices:create_invoice_access_token(get_context(Token), ?STRING),
    stop_mocked_service_sup(MockServiceSup),
    [{context, get_context(InvAccToken)} | Config];

init_per_group(operations_by_invoice_template_access_token, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = issue_token([{[party], write}], unlimited),
    mock_services([{invoice_templating, fun('Create', _) -> {ok, ?INVOICE_TPL} end}], MockServiceSup),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => get_lifetime(),
        <<"cost">> => #{
            <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        },
        <<"product">> => <<"test_invoice_template_product">>,
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    {ok, #{
            <<"invoiceTemplateAccessToken">> := #{<<"payload">> := InvTemplAccToken}
        }
    } = capi_client_invoice_templates:create(get_context(Token), Req),
    stop_mocked_service_sup(MockServiceSup),
    [{context, get_context(InvTemplAccToken)} | Config];

init_per_group(operations_by_customer_access_token_after_customer_creation, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = issue_token([{[customers], write}], unlimited),
    mock_services([{customer_management, fun('Create', _) -> {ok, ?CUSTOMER} end}], MockServiceSup),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"contactInfo">> => #{<<"email">> => <<"bla@bla.ru">>},
        <<"metadata">> => #{<<"text">> => [<<"SOMESHIT">>, 42]}
    },
    {ok, #{
            <<"customerAccessToken">> := #{<<"payload">> := CustAccToken}
        }
    } = capi_client_customers:create_customer(get_context(Token), Req),
    stop_mocked_service_sup(MockServiceSup),
    [{context, get_context(CustAccToken)} | Config];

init_per_group(operations_by_customer_access_token_after_token_creation, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = issue_token([{[customers], write}], unlimited),
    mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], MockServiceSup),
    {ok,
        #{<<"payload">> := CustAccToken}
    } = capi_client_customers:create_customer_access_token(get_context(Token), ?STRING),
    stop_mocked_service_sup(MockServiceSup),
    [{context, get_context(CustAccToken)}| Config];

init_per_group(GroupName, Config) when
    GroupName == operations_by_base_api_token;
    GroupName == woody_errors
->
    BasePermissions = [
        {[invoices], write},
        {[invoices], read},
        {[party], write},
        {[party], read},
        {[invoices, payments], write},
        {[invoices, payments], read},
        {[customers], write}
    ],
    {ok, Token} = issue_token(BasePermissions, unlimited),
    Context = get_context(Token),
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
    [{test_sup, start_mocked_service_sup()} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec woody_unexpected_test(config()) ->
    _.

woody_unexpected_test(Config) ->
    _ = mock_services([{party_management, fun('Get', _) -> {ok, "spanish inquisition"} end}], Config),
    ?badresp(500) = capi_client_parties:get_my_party(?config(context, Config)).

-spec woody_unavailable_test(config()) ->
    _.

woody_unavailable_test(Config) ->
    _ = capi_ct_helper:start_app(capi_woody_client, [{service_urls, #{
        party_management => <<"http://spanish.inquision/v1/partymgmt">>
    }}]),
    ?badresp(503) = capi_client_parties:get_my_party(?config(context, Config)).

-spec woody_retry_test(config()) ->
    _.

woody_retry_test(Config) ->
    _ = capi_ct_helper:start_app(capi_woody_client, [
        {service_urls, #{
            party_management => <<"http://spanish.inquision/v1/partymgmt">>
        }},
        {service_retries, #{
            party_management    => #{
                'Get'   => {linear, 30, 1000},
                '_'     => finish
            }
        }},
        {service_deadlines, #{
            party_management => 5000
        }}
    ]),
    {Time, ?badresp(503)} = timer:tc(capi_client_parties, get_my_party, [?config(context, Config)]),
    _ = ?assert(Time > 4000000),
    _ = ?assert(Time < 6000000).

-spec woody_unknown_test(config()) ->
    _.

woody_unknown_test(Config) ->
    _ = mock_services([{party_management, fun('Get', _) -> timer:sleep(60000) end}], Config),
    ?badresp(504) = capi_client_parties:get_my_party(?config(context, Config)).

-spec authorization_positive_lifetime_ok_test(config()) ->
    _.
authorization_positive_lifetime_ok_test(_Config) ->
    {ok, Token} = issue_token([], {lifetime, 10}),
    {ok, _} = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_unlimited_lifetime_ok_test(config()) ->
    _.
authorization_unlimited_lifetime_ok_test(_Config) ->
    {ok, Token} = issue_token([], unlimited),
    {ok, _} = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_far_future_deadline_ok_test(config()) ->
    _.
authorization_far_future_deadline_ok_test(_Config) ->
    {ok, Token} = issue_token([], {deadline, 4102444800}), % 01/01/2100 @ 12:00am (UTC)
    {ok, _} = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_permission_ok_test(config()) ->
    _.
authorization_permission_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, Token} = issue_token([{[party], read}], unlimited),
    {ok, _} = capi_client_parties:get_my_party(get_context(Token)).

-spec authorization_negative_lifetime_error_test(config()) ->
    _.
authorization_negative_lifetime_error_test(_Config) ->
    ok.
    % {ok, Token} = issue_token([], {lifetime, -10}),
    % ?badresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_bad_deadline_error_test(config()) ->
    _.
authorization_bad_deadline_error_test(_Config) ->
    ok.
    % {ok, Token} = issue_token([], {deadline, -10}),
    % ?badresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_error_no_header_test(config()) ->
    _.
authorization_error_no_header_test(_Config) ->
    Token = <<>>,
    ?badresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_error_no_permission_test(config()) ->
    _.
authorization_error_no_permission_test(_Config) ->
    {ok, Token} = issue_token([], {lifetime, 10}),
    ?badresp(401) = capi_client_parties:get_my_party(get_context(Token)).

-spec authorization_bad_token_error_test(config()) ->
    _.
authorization_bad_token_error_test(Config) ->
    {ok, Token} = issue_dummy_token([{[party], read}], Config),
    ?badresp(401) = capi_client_parties:get_my_party(get_context(Token)).

-spec create_invoice_ok_test(config()) ->
    _.
create_invoice_ok_test(Config) ->
    mock_services([
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"key">>)} end},
        {invoicing, fun('Create', [_, #payproc_InvoiceParams{ id = <<"key">>}]) -> {ok, ?PAYPROC_INVOICE} end}
    ], Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">> => ?TIMESTAMP,
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    {ok, _} = capi_client_invoices:create_invoice(?config(context, Config), Req).

-spec create_invoice_with_tpl_ok_test(config()) ->
    _.
create_invoice_with_tpl_ok_test(Config) ->
    mock_services([
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"key">>)} end},
        {invoicing, fun('CreateWithTemplate', [_, #payproc_InvoiceWithTemplateParams{ id = <<"key">>}]) ->
            {ok, ?PAYPROC_INVOICE} end}
    ], Config),
    Req = #{
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>}
    },
    {ok, _} = capi_client_invoice_templates:create_invoice(?config(context, Config), ?STRING, Req).

-spec get_invoice_ok_test(config()) ->
    _.
get_invoice_ok_test(Config) ->
    mock_services([
        {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}
    ], Config),
    {ok, _} = capi_client_invoices:get_invoice_by_id(?config(context, Config), ?STRING).

-spec get_invoice_events_ok_test(config()) ->
    _.
get_invoice_events_ok_test(Config) ->
    Inc = fun
        (X) when is_integer(X) -> X + 1;
        (_) -> 1
    end,
    _ = mock_services([
        {invoicing, fun
            ('GetEvents', [_, _, #payproc_EventRange{'after' = ID, limit = N}]) ->
                {ok, lists:sublist([
                    ?INVOICE_EVENT(1),
                    ?INVOICE_EVENT(2),
                    ?INVOICE_EVENT_PRIVATE(3),
                    ?INVOICE_EVENT(4),
                    ?INVOICE_EVENT_PRIVATE(5),
                    ?INVOICE_EVENT_PRIVATE(6),
                    ?INVOICE_EVENT(7)
                ], Inc(ID), N)}
        end}
    ], Config),
    {ok, [#{<<"id">> := 1}, #{<<"id">> := 2}, #{<<"id">> := 4}]} =
        capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, 3),
    {ok, [#{<<"id">> := 4}, #{<<"id">> := 7}]} =
        capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, 2, 3).

-spec get_invoice_payment_methods_ok_test(config()) ->
    _.
get_invoice_payment_methods_ok_test(Config) ->
    mock_services([
        {party_management, fun('Get', _) -> {ok, ?PARTY} end},
        {invoicing, fun('ComputeTerms', _) -> {ok, ?TERM_SET} end}
    ], Config),
    {ok, _} = capi_client_invoices:get_invoice_payment_methods(?config(context, Config), ?STRING).

-spec create_invoice_access_token_ok_test(config()) ->
    _.
create_invoice_access_token_ok_test(Config) ->
    mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    {ok, _} = capi_client_invoices:create_invoice_access_token(?config(context, Config), ?STRING).

-spec rescind_invoice_ok_test(config()) ->
    _.
rescind_invoice_ok_test(Config) ->
    mock_services([{invoicing, fun('Rescind', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoices:rescind_invoice(?config(context, Config), ?STRING, ?STRING).

-spec fulfill_invoice_ok_test(config()) ->
    _.
fulfill_invoice_ok_test(Config) ->
    mock_services([{invoicing, fun('Fulfill', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoices:fulfill_invoice(?config(context, Config), ?STRING, ?STRING).

-spec create_invoice_template_ok_test(config()) ->
    _.
create_invoice_template_ok_test(Config) ->
    mock_services([{invoice_templating, fun('Create', _) -> {ok, ?INVOICE_TPL} end}], Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => get_lifetime(),
        <<"cost">> => #{
            <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        },
        <<"product">> => <<"test_invoice_template_product">>,
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req).

-spec get_invoice_template_ok_test(config()) ->
    _.
get_invoice_template_ok_test(Config) ->
    mock_services([{invoice_templating, fun('Get', _) -> {ok, ?INVOICE_TPL} end}], Config),
    {ok, _} = capi_client_invoice_templates:get_template_by_id(?config(context, Config), ?STRING).

-spec update_invoice_template_ok_test(config()) ->
    _.
update_invoice_template_ok_test(Config) ->
    mock_services([{invoice_templating, fun('Update', _) -> {ok, ?INVOICE_TPL} end}], Config),
    Req = #{
        <<"cost">> => #{
            <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostFixed">>,
            <<"amount">> => ?INTEGER,
            <<"currency">> => ?RUB
        },
        <<"lifetime">> => get_lifetime(),
        <<"product">> => <<"test_invoice_template_product">>,
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    {ok, _} = capi_client_invoice_templates:update(?config(context, Config), ?STRING, Req).

-spec delete_invoice_template_ok_test(config()) ->
    _.
delete_invoice_template_ok_test(Config) ->
    mock_services([{invoice_templating, fun('Delete', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoice_templates:delete(?config(context, Config), ?STRING).

-spec get_invoice_payment_methods_by_tpl_id_ok_test(config()) ->
    _.
get_invoice_payment_methods_by_tpl_id_ok_test(Config) ->
    mock_services([
          {party_management, fun('Get', _) -> {ok, ?PARTY} end},
        {'invoice_templating', fun('ComputeTerms', _) -> {ok, ?TERM_SET} end}
    ], Config),
    {ok, _} = capi_client_invoice_templates:get_invoice_payment_methods(?config(context, Config), ?STRING).

-spec get_account_by_id_ok_test(config()) ->
    _.
get_account_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('GetAccountState', _) -> {ok, ?ACCOUNT_STATE} end}], Config),
    {ok, _} = capi_client_accounts:get_account_by_id(?config(context, Config), ?INTEGER).

-spec create_payment_ok_test(config()) ->
    _.
create_payment_ok_test(Config) ->
    mock_services(
        [
            {invoicing, fun(
                'StartPayment',
                [_, <<"TEST">>, #payproc_InvoicePaymentParams{id = <<"payment_key">>}]
            ) ->
                {ok, ?PAYPROC_PAYMENT}
            end},
            {bender, fun
                ('GenerateID', [_, {sequence,  _}, _]) -> {ok, capi_ct_helper_bender:get_result(<<"payment_key">>)};
                ('GenerateID', [_, {constant,  _}, _]) -> {ok, capi_ct_helper_bender:get_result(<<"session_key">>)}
            end}
        ],
        Config
    ),
    PaymentToolToken = ?TEST_PAYMENT_TOKEN,
    Req2 = #{
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => PaymentToolToken,
            <<"contactInfo">> => #{
                <<"email">> => <<"bla@bla.ru">>
            }
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req2, ?STRING).

-spec create_payment_with_encrypt_token_ok_test(config()) ->
    _.
create_payment_with_encrypt_token_ok_test(Config) ->
    mock_services(
        [
            {invoicing, fun(
                'StartPayment',
                [_, <<"TEST">>, #payproc_InvoicePaymentParams{id = <<"payment_key">>}]
            ) ->
                {ok, ?PAYPROC_PAYMENT}
            end},
            {bender, fun
                ('GenerateID', [_, {sequence,  _}, _]) -> {ok, capi_ct_helper_bender:get_result(<<"payment_key">>)};
                ('GenerateID', [_, {constant,  _}, _]) -> {ok, capi_ct_helper_bender:get_result(<<"session_key">>)}
            end}
        ],
        Config
    ),
    PaymentToolToken = get_encrypted_token(),
    Req2 = #{
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => PaymentToolToken,
            <<"contactInfo">> => #{
                <<"email">> => <<"bla@bla.ru">>
            }
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req2, ?STRING).

get_encrypted_token() ->
    PaymentTool = {bank_card, #domain_BankCard{
        token = <<"4111111111111111">>,
        payment_system = mastercard,
        bin = <<>>,
        masked_pan = <<"1111">>,
        cardholder_name = <<"Degus Degusovich">>
    }},
    capi_crypto:create_encrypted_payment_tool_token(<<"idemp key">>, PaymentTool).

-spec get_payments_ok_test(config()) ->
    _.
get_payments_ok_test(Config) ->
    mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    {ok, _} = capi_client_payments:get_payments(?config(context, Config), ?STRING).

-spec get_payment_by_id_ok_test(config()) ->
    _.
get_payment_by_id_ok_test(Config) ->
    mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    {ok, _} = capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec create_refund(config()) ->
    _.
create_refund(Config) ->
    mock_services([
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {invoicing, fun('RefundPayment', _) -> {ok, ?REFUND_DOMAIN} end}
    ], Config),
    Req = #{<<"reason">> => ?STRING},
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_refund_idemp_ok_test(config()) ->
    _.
create_refund_idemp_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    mock_services([
        {invoicing,
            fun(
                'RefundPayment',
                [_, _, _, #payproc_InvoicePaymentRefundParams{id = ID}]
            ) ->
                {ok, ?REFUND_DOMAIN(ID)}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
    ], Config),
    Req = #{
        <<"reason">> => ?STRING,
        <<"id">> => ?STRING
    },
    {ok, Refund} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING),
    {ok, Refund2} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING),
    ?assertEqual(BenderKey,  maps:get(<<"id">>, Refund)),
    ?assertEqual(Refund, Refund2).

-spec create_partial_refund(config()) ->
    _.
create_partial_refund(Config) ->
    mock_services([
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {invoicing, fun('RefundPayment', _) -> {ok, ?REFUND_DOMAIN} end}
    ], Config),
    Req = #{
        <<"reason">> => ?STRING,
        <<"currency">> => ?RUB,
        <<"amount">> => ?INTEGER
    },
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_partial_refund_without_currency(config()) ->
    _.
create_partial_refund_without_currency(Config) ->
    mock_services([
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
        {
            invoicing,
            fun
                ('GetPayment', _) ->
                    {ok, ?PAYPROC_PAYMENT};
                ('RefundPayment', _) ->
                    {ok, ?REFUND_DOMAIN}
            end
        }
    ], Config),
    Req = #{
        <<"reason">> => ?STRING,
        <<"amount">> => ?INTEGER
    },
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec get_refund_by_id(config()) ->
    _.
get_refund_by_id(Config) ->
    mock_services([{invoicing, fun('GetPaymentRefund', _) -> {ok, ?REFUND_DOMAIN} end}], Config),
    {ok, _} = capi_client_payments:get_refund_by_id(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec get_refunds(config()) ->
    _.
get_refunds(Config) ->
    mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    {ok, _} = capi_client_payments:get_refunds(?config(context, Config), ?STRING, ?STRING).

-spec cancel_payment_ok_test(config()) ->
    _.
cancel_payment_ok_test(Config) ->
    mock_services([{invoicing, fun('CancelPayment', _) -> {ok, ok} end}], Config),
    ok = capi_client_payments:cancel_payment(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec capture_payment_ok_test(config()) ->
    _.
capture_payment_ok_test(Config) ->
    mock_services([{invoicing, fun('CapturePaymentNew', _) -> {ok, ok} end}], Config),
    ok = capi_client_payments:capture_payment(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec get_my_party_ok_test(config()) ->
    _.
get_my_party_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)).

-spec suspend_my_party_ok_test(config()) ->
    _.
suspend_my_party_ok_test(Config) ->
    mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    ok = capi_client_parties:suspend_my_party(?config(context, Config)).

-spec activate_my_party_ok_test(config()) ->
    _.
activate_my_party_ok_test(Config) ->
    mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    ok = capi_client_parties:activate_my_party(?config(context, Config)).

-spec get_shop_by_id_ok_test(config()) ->
    _.
get_shop_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('GetShop', _) -> {ok, ?SHOP} end}], Config),
    {ok, _} = capi_client_shops:get_shop_by_id(?config(context, Config), ?STRING).

-spec get_shops_ok_test(config()) ->
    _.
get_shops_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_shops:get_shops(?config(context, Config)).

-spec suspend_shop_ok_test(config()) ->
    _.
suspend_shop_ok_test(Config) ->
    mock_services([{party_management, fun('SuspendShop', _) -> {ok, ok} end}], Config),
    ok = capi_client_shops:suspend_shop(?config(context, Config), ?STRING).

-spec activate_shop_ok_test(config()) ->
    _.
activate_shop_ok_test(Config) ->
    mock_services([{party_management, fun('ActivateShop', _) -> {ok, ok} end}], Config),
    ok = capi_client_shops:activate_shop(?config(context, Config), ?STRING).

-spec get_claim_by_id_ok_test(config()) ->
    _.
get_claim_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('GetClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}], Config),
    {ok, _} = capi_client_claims:get_claim_by_id(?config(context, Config), ?INTEGER).

-spec get_claims_ok_test(config()) ->
    _.
get_claims_ok_test(Config) ->
    mock_services([
        {party_management, fun('GetClaims', _) -> {ok, [
            ?CLAIM(?CLAIM_CHANGESET),
            ?CLAIM(?CONTRACTOR_CLAIM_CHANGESET),
            ?CLAIM(?WALLET_CLAIM_CHANGESET)
        ]} end}
    ], Config),
    {ok, [_OnlyOneClaim]} = capi_client_claims:get_claims(?config(context, Config)).

-spec revoke_claim_ok_test(config()) ->
    _.
revoke_claim_ok_test(Config) ->
    mock_services([{party_management, fun('RevokeClaim', _) -> {ok, ok} end}], Config),
    ok = capi_client_claims:revoke_claim_by_id(?config(context, Config), ?STRING, ?INTEGER, ?INTEGER).

-spec create_claim_ok_test(config()) ->
    _.
create_claim_ok_test(Config) ->
    mock_services([{party_management, fun('CreateClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}], Config),
    Changeset = [
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ?STRING,
            <<"contractModificationType">> => <<"ContractCreation">>,
            <<"contractor">> => #{
                <<"contractorType">> => <<"LegalEntity">>,
                <<"entityType">> => <<"RussianLegalEntity">>,
                <<"registeredName">> => <<"testRegisteredName">>,
                <<"registeredNumber">> => <<"1234567890123">>,
                <<"inn">> => <<"1234567890">>,
                <<"actualAddress">> => <<"testActualAddress">>,
                <<"postAddress">> => <<"testPostAddress">>,
                <<"representativePosition">> => <<"testRepresentativePosition">>,
                <<"representativeFullName">> => <<"testRepresentativeFullName">>,
                <<"representativeDocument">> => <<"testRepresentativeDocument">>,
                <<"bankAccount">> => #{
                    <<"account">> => <<"12345678901234567890">>,
                    <<"bankName">> => <<"testBankName">>,
                    <<"bankPostAccount">> => <<"12345678901234567890">>,
                    <<"bankBik">> => <<"123456789">>
                }
            }
        },
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => <<"PrivateEntityContract">>,
            <<"contractModificationType">> => <<"ContractCreation">>,
            <<"contractor">> => #{
                <<"contractorType">> => <<"PrivateEntity">>,
                <<"entityType">> => <<"RussianPrivateEntity">>,
                <<"firstName">> => ?STRING,
                <<"secondName">> => ?STRING,
                <<"middleName">> => ?STRING,
                <<"contactInfo">> => #{}
            },
            <<"paymentInstitutionID">> => ?INTEGER
        },
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ?STRING,
            <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
            <<"payoutToolID">> => ?STRING,
            <<"currency">> => ?RUB,
            <<"details">> => #{
                <<"detailsType">> => <<"PayoutToolDetailsBankAccount">>,
                <<"account">> => <<"12345678901234567890">>,
                <<"bankName">> => <<"testBankName">>,
                <<"bankPostAccount">> => <<"12345678901234567890">>,
                <<"bankBik">> => <<"123456789">>
            }
        },
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ?STRING,
            <<"contractModificationType">> => <<"ContractLegalAgreementBinding">>,
            <<"legalAgreement">> => #{
                <<"id">> => ?STRING,
                <<"signedAt">> => ?TIMESTAMP,
                <<"validUntil">> => ?TIMESTAMP
            }
        },
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ?STRING,
            <<"contractModificationType">> => <<"ContractReportingPreferencesChange">>,
            <<"serviceAcceptanceActPreferences">> => #{
                <<"scheduleID">> => ?INTEGER,
                <<"signer">> => #{
                    <<"position">> => ?STRING,
                    <<"fullName">> => ?STRING,
                    <<"document">> => #{<<"representativeDocumentType">> => <<"ArticlesOfAssociation">>}
                }
            }
        }
    ],
    {ok, _} = capi_client_claims:create_claim(?config(context, Config), Changeset).

-spec update_claim_by_id_test(config()) ->
    _.
update_claim_by_id_test(_) ->
    % Not realised yet.
    ok.

-spec get_contract_by_id_ok_test(config()) ->
    _.
get_contract_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?STRING),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?WALLET_CONTRACT_ID).

-spec get_contracts_ok_test(config()) ->
    _.
get_contracts_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, [_First, _Second]} = capi_client_contracts:get_contracts(?config(context, Config)).

-spec get_contract_adjustments_ok_test(config()) ->
    _.
get_contract_adjustments_ok_test(Config) ->
    mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_adjustments(?config(context, Config), ?STRING).

-spec get_contract_adjustment_by_id_ok_test(config()) ->
    _.
get_contract_adjustment_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_adjustment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec get_payout_tools_ok_test(config()) ->
    _.
get_payout_tools_ok_test(Config) ->
    mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_payouts:get_payout_tools(?config(context, Config), ?STRING).

-spec get_payout_tool_by_id(config()) ->
    _.
get_payout_tool_by_id(Config) ->
    mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_RU),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_US).

-spec create_webhook_ok_test(config()) ->
    _.
create_webhook_ok_test(Config) ->
    mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end},
            {webhook_manager, fun('Create', _) -> {ok, ?WEBHOOK} end}
        ],
        Config
    ),
    Req = #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => #{
            <<"topic">> => <<"InvoicesTopic">>,
            <<"shopID">> => ?STRING,
            <<"eventTypes">> => []
        }
    },
    {ok, _} = capi_client_webhooks:create_webhook(?config(context, Config), Req).

-spec get_webhooks(config()) ->
    _.
get_webhooks(Config) ->
    mock_services([{webhook_manager, fun('GetList', _) -> {ok, [?WEBHOOK]} end}], Config),
    {ok, _} = capi_client_webhooks:get_webhooks(?config(context, Config)).

-spec get_webhook_by_id(config()) ->
    _.
get_webhook_by_id(Config) ->
    mock_services([{webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK} end}], Config),
    {ok, _} = capi_client_webhooks:get_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec delete_webhook_by_id(config()) ->
    _.
delete_webhook_by_id(Config) ->
    mock_services([{webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK}; ('Delete', _) -> {ok, ok} end}], Config),
    ok = capi_client_webhooks:delete_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec get_locations_names_ok_test(config()) ->
    _.
get_locations_names_ok_test(Config) ->
    mock_services([{geo_ip_service, fun('GetLocationName', _) -> {ok, #{123 => ?STRING}} end}], Config),
    Query = #{
        <<"geoIDs">> => <<"5,3,6,5,4">>,
        <<"language">> => <<"ru">>
    },
    {ok, _} = capi_client_geo:get_location_names(?config(context, Config), Query).

-spec search_invoices_ok_test(config()) ->
    _.
search_invoices_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {invoiceStatus, <<"fulfilled">>},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        {lastDigits, <<"2222">>},
        {bin, <<"424242">>},
        {bankCardTokenProvider, <<"applepay">>},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmount, 10000}
    ],

    {ok, _, _} = capi_client_searches:search_invoices(?config(context, Config), ?STRING, Query).

-spec search_payments_ok_test(config()) ->
    _.
search_payments_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.0.1">>},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        % {lastDigits, <<"2222">>}, %%@FIXME cannot be used until getting the newest api client
        % {bin, <<"424242">>},
        {bankCardTokenProvider, <<"applepay">>},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmount, 10000}
    ],

    {ok, _, _} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_payouts_ok_test(config()) ->
    _.
search_payouts_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetPayouts', _) -> {ok, ?STAT_RESPONSE_PAYOUTS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {shopID, <<"testShopID">>},
        {payoutID, <<"testPayoutID">>},
        {payoutToolType, <<"PayoutCard">>}
    ],

    {ok, _, _} = capi_client_searches:search_payouts(?config(context, Config), ?STRING, Query).

-spec get_payment_conversion_stats_ok_test(_) ->
    _.
get_payment_conversion_stats_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_conversion_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_revenue_stats_ok_test(config()) ->
    _.
get_payment_revenue_stats_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_revenue_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_geo_stats_ok_test(config()) ->
    _.
get_payment_geo_stats_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_geo_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_rate_stats_ok_test(config()) ->
    _.
get_payment_rate_stats_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_rate_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_method_stats_ok_test(config()) ->
    _.
get_payment_method_stats_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1},
        {paymentMethod, <<"bankCard">>}
    ],
    {ok, _} = capi_client_analytics:get_payment_method_stats(?config(context, Config), ?STRING, Query).

-spec get_reports_ok_test(config()) ->
    _.
get_reports_ok_test(Config) ->
    mock_services([{reporting, fun('GetReports', _) -> {ok, ?FOUND_REPORTS} end}], Config),
    {ok, _} = capi_client_reports:get_reports(?config(context, Config), ?STRING, ?TIMESTAMP, ?TIMESTAMP).

-spec download_report_file_ok_test(_) ->
    _.
download_report_file_ok_test(Config) ->
    mock_services([{reporting, fun
        ('GetReport', _) -> {ok, ?REPORT};
        ('GeneratePresignedUrl', _) -> {ok, ?STRING} end}
    ], Config),
    {ok, _} = capi_client_reports:download_file(?config(context, Config), ?STRING, ?INTEGER, ?STRING).

-spec download_report_file_not_found_test(_) ->
    _.
download_report_file_not_found_test(Config) ->
    mock_services([{reporting, fun
        ('GetReport', _) -> {ok, ?REPORT};
        ('GeneratePresignedUrl', _) -> {ok, ?STRING} end}
    ], Config),
    {error, {404, #{<<"message">> := <<"Report not found">>}}} =
        capi_client_reports:download_file(?config(context, Config), <<"WRONG_STRING">>, ?INTEGER, ?STRING).

-spec get_categories_ok_test(config()) ->
    _.
get_categories_ok_test(Config) ->
    {ok, _} = capi_client_categories:get_categories(?config(context, Config)).

-spec get_category_by_ref_ok_test(config()) ->
    _.
get_category_by_ref_ok_test(Config) ->
    {ok, _} = capi_client_categories:get_category_by_ref(?config(context, Config), ?INTEGER).

-spec get_schedule_by_ref_ok_test(config()) ->
    _.
get_schedule_by_ref_ok_test(Config) ->
    {ok, _} = capi_client_payouts:get_schedule_by_ref(?config(context, Config), ?INTEGER).

-spec get_payment_institutions(config()) ->
    _.
get_payment_institutions(Config) ->
    {ok, [_Something]} = capi_client_payment_institutions:get_payment_institutions(?config(context, Config)),
    {ok, []} =
        capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"live">>),
    {ok, [#{<<"realm">> := <<"test">>}]} =
        capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"test">>).

-spec get_payment_institution_by_ref(config()) ->
    _.
get_payment_institution_by_ref(Config) ->
    {ok, _} = capi_client_payment_institutions:get_payment_institution_by_ref(?config(context, Config), ?INTEGER).

-spec get_payment_institution_payment_terms(config()) ->
    _.
get_payment_institution_payment_terms(Config) ->
    mock_services(
        [
            {party_management, fun('ComputePaymentInstitutionTerms', _) -> {ok, ?TERM_SET} end}
        ],
        Config
    ),
    {ok, _} =
        capi_client_payment_institutions:get_payment_institution_payment_terms(?config(context, Config), ?INTEGER).

-spec get_payment_institution_payout_terms(config()) ->
    _.
get_payment_institution_payout_terms(Config) ->
    mock_services(
        [
            {party_management, fun('ComputePaymentInstitutionTerms', _) -> {ok, ?TERM_SET} end}
        ],
        Config
    ),
    {ok, _} = capi_client_payment_institutions:get_payment_institution_payout_methods(
        ?config(context, Config),
        ?INTEGER,
        <<"RUB">>
    ),
    {ok, _} = capi_client_payment_institutions:get_payment_institution_payout_schedules(
        ?config(context, Config),
        ?INTEGER,
        <<"USD">>,
        <<"BankAccount">>
    ).

-spec create_customer_ok_test(config()) ->
    _.
create_customer_ok_test(Config) ->
    mock_services([{customer_management, fun('Create', _) -> {ok, ?CUSTOMER} end}], Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"contactInfo">> => #{<<"email">> => <<"bla@bla.ru">>},
        <<"metadata">> => #{<<"text">> => [<<"SOMESHIT">>, 42]}
    },
    {ok, _} = capi_client_customers:create_customer(?config(context, Config), Req).

-spec get_customer_ok_test(config()) ->
    _.
get_customer_ok_test(Config) ->
    mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_customer_by_id(?config(context, Config), ?STRING).

-spec create_customer_access_token_ok_test(config()) ->
    _.
create_customer_access_token_ok_test(Config) ->
    mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:create_customer_access_token(?config(context, Config), ?STRING).

-spec create_binding_ok_test(config()) ->
    _.
create_binding_ok_test(Config) ->
    mock_services(
        [
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end},
            {customer_management, fun('StartBinding', _) -> {ok, ?CUSTOMER_BINDING} end}
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


-spec get_bindings_ok_test(config()) ->
    _.
get_bindings_ok_test(Config) ->
    mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_bindings(?config(context, Config), ?STRING).

-spec get_binding_ok_test(config()) ->
    _.
get_binding_ok_test(Config) ->
    mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:get_binding(?config(context, Config), ?STRING, ?STRING).

-spec get_customer_events_ok_test(config()) ->
    _.
get_customer_events_ok_test(Config) ->
    mock_services([{customer_management, fun('GetEvents', _) -> {ok, []} end}], Config),
    {ok, _} = capi_client_customers:get_customer_events(?config(context, Config), ?STRING, 10).

-spec delete_customer_ok_test(config()) ->
    _.
delete_customer_ok_test(Config) ->
    mock_services([{customer_management, fun('Delete', _) -> {ok, ok} end}], Config),
    {ok, _} = capi_client_customers:delete_customer(?config(context, Config), ?STRING).

%%

issue_token(ACL, LifeTime) ->
    PartyID = ?STRING,
    Claims = #{?STRING => ?STRING},
    capi_authorizer_jwt:issue({{PartyID, capi_acl:from_list(ACL)}, Claims}, LifeTime).

issue_dummy_token(ACL, Config) ->
    Claims = #{
        <<"jti">> => unique_id(),
        <<"sub">> => ?STRING,
        <<"exp">> => 0,
        <<"resource_access">> => #{
            <<"common-api">> => #{
                <<"roles">> => capi_acl:encode(capi_acl:from_list(ACL))
            }
        }
    },
    BadPemFile = get_keysource("keys/local/dummy.pem", Config),
    BadJWK = jose_jwk:from_pem_file(BadPemFile),
    GoodPemFile = get_keysource("keys/local/private.pem", Config),
    GoodJWK = jose_jwk:from_pem_file(GoodPemFile),
    JWKPublic = jose_jwk:to_public(GoodJWK),
    {_Module, PublicKey} = JWKPublic#jose_jwk.kty,
    {_PemEntry, Data, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey),
    KID = base64url:encode(crypto:hash(sha256, Data)),
    JWT = jose_jwt:sign(BadJWK, #{<<"alg">> => <<"RS256">>, <<"kid">> => KID}, Claims),
    {_Modules, Token} = jose_jws:compact(JWT),
    {ok, Token}.

start_capi(Config) ->
    Key = get_keysource("keys/local/jwk.json", Config),
    Passwd = get_keysource("keys/local/secret.password", Config),
    CapiEnv = [
        {ip, ?CAPI_IP},
        {port, ?CAPI_PORT},
        {service_type, real},
        {authorizers, #{
            jwt => #{
                signee => capi,
                keyset => #{
                    capi => {pem_file, get_keysource("keys/local/private.pem", Config)}
                }
            }
        }},
        {lechiffre_opts,  #{
            encryption_key_path => {Key, Passwd},
            decryption_key_paths => [{Key, Passwd}]
        }}
    ],
    capi_ct_helper:start_app(capi, CapiEnv).

% TODO move it to `capi_dummy_service`, looks more appropriate
start_mocked_service_sup() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    SupPid.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

mock_services(Services, SupOrConfig) ->
    start_woody_client(mock_services_(Services, SupOrConfig)).

% TODO need a better name
mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));

mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?CAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => capi_woody_event_handler,
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    lists:foldl(
        fun (Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, capi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {capi_dummy_service, #{function => Fun}}}}.

start_woody_client(ServiceURLs) ->
    capi_ct_helper:start_app(capi_woody_client, [{service_urls, ServiceURLs}]).

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?CAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

% TODO not so failproof, ideally we need to bind socket first and then give to a ranch listener
get_random_port() ->
    rand:uniform(32768) + 32767.

get_context(Token) ->
    capi_client_lib:get_context(?CAPI_URL, Token, 10000, ipv4).

get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).

get_lifetime() ->
    get_lifetime(0, 0, 7).

get_lifetime(YY, MM, DD) ->
    #{
       <<"years">>  => YY,
       <<"months">> => MM,
       <<"days">>   => DD
    }.

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).
