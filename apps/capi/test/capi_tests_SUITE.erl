-module(capi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("dmsl/include/dmsl_webhooker_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_tool_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payout_processing_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
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
    get_payments_ok_test/1,
    get_payment_by_id_ok_test/1,
    get_client_payment_status_test/1,
    get_merchant_payment_status_test/1,
    create_refund/1,
    create_refund_error/1,
    create_partial_refund/1,
    create_partial_refund_without_currency/1,
    get_refund_by_id/1,
    get_refunds/1,
    cancel_payment_ok_test/1,
    capture_payment_ok_test/1,

    create_first_recurrent_payment_ok_test/1,
    create_second_recurrent_payment_ok_test/1,
    get_recurrent_payments_ok_test/1,

    create_visa_payment_resource_ok_test/1,
    create_nspkmir_payment_resource_ok_test/1,
    create_euroset_payment_resource_ok_test/1,
    create_qw_payment_resource_ok_test/1,
    create_applepay_tokenized_payment_resource_ok_test/1,
    create_googlepay_tokenized_payment_resource_ok_test/1,
    create_googlepay_plain_payment_resource_ok_test/1,

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
    create_claim_invalid_residence_test/1,

    get_contract_by_id_ok_test/1,
    get_contracts_ok_test/1,
    get_contract_adjustments_ok_test/1,
    get_contract_adjustment_by_id_ok_test/1,

    get_payout_tools_ok_test/1,
    get_payout_tool_by_id/1,

    create_payout/1,
    get_payout/1,
    get_payout_fail/1,

    create_webhook_ok_test/1,
    get_webhooks/1,
    get_webhook_by_id/1,
    delete_webhook_by_id/1,

    get_locations_names_ok_test/1,

    search_invoices_ok_test/1,
    search_payments_ok_test/1,
    search_refunds_ok_test/1,
    search_payouts_ok_test/1,

    get_payment_conversion_stats_ok_test/1,
    get_payment_revenue_stats_ok_test/1,
    get_payment_geo_stats_ok_test/1,
    get_payment_rate_stats_ok_test/1,
    get_payment_method_stats_ok_test/1,

    get_reports_ok_test/1,
    get_report_ok_test/1,
    create_report_ok_test/1,
    download_report_file_ok_test/1,

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
    delete_customer_ok_test/1,

    deadline_error_test/1,
    deadline_absolute_ok_test/1,
    deadline_relative_ok_test/1
]).

-define(CAPI_IP                     , "::").
-define(CAPI_PORT                   , 8080).
-define(CAPI_HOST_NAME              , "localhost").
-define(CAPI_URL                    , ?CAPI_HOST_NAME ++ ":" ++ integer_to_list(?CAPI_PORT)).

-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error,{Code, #{}}}).

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
        {group, authorization},
        {group, deadline_header}
    ].

invoice_access_token_tests() ->
    [
        get_invoice_ok_test,
        get_invoice_events_ok_test,
        get_invoice_payment_methods_ok_test,
        create_payment_ok_test,
        get_payments_ok_test,
        get_client_payment_status_test,
        get_payment_by_id_ok_test,
        cancel_payment_ok_test,
        capture_payment_ok_test,
        create_first_recurrent_payment_ok_test,
        create_second_recurrent_payment_ok_test,
        get_recurrent_payments_ok_test,
        {group, payment_resources}
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
        {woody_errors, [],
            [
                woody_unexpected_test,
                woody_unavailable_test,
                woody_retry_test,
                woody_unknown_test
            ]
        },
        {payment_resources, [],
            [
                create_visa_payment_resource_ok_test,
                create_nspkmir_payment_resource_ok_test,
                create_euroset_payment_resource_ok_test,
                create_qw_payment_resource_ok_test,
                create_applepay_tokenized_payment_resource_ok_test,
                create_googlepay_tokenized_payment_resource_ok_test,
                create_googlepay_plain_payment_resource_ok_test
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
                get_merchant_payment_status_test,
                create_refund,
                create_refund_error,
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
                create_claim_invalid_residence_test,
                get_contract_by_id_ok_test,
                get_contracts_ok_test,
                get_contract_adjustments_ok_test,
                get_contract_adjustment_by_id_ok_test,
                get_payout_tools_ok_test,
                get_payout_tool_by_id,
                create_payout,
                get_payout,
                get_payout_fail,
                create_webhook_ok_test,
                get_webhooks,
                get_webhook_by_id,
                delete_webhook_by_id,
                get_locations_names_ok_test,
                search_invoices_ok_test,
                search_payments_ok_test,
                search_refunds_ok_test,
                search_payouts_ok_test,
                get_payment_conversion_stats_ok_test,
                get_payment_revenue_stats_ok_test,
                get_payment_geo_stats_ok_test,
                get_payment_rate_stats_ok_test,
                get_payment_method_stats_ok_test,
                get_reports_ok_test,
                get_report_ok_test,
                create_report_ok_test,
                download_report_file_ok_test,
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
        },
        {deadline_header, [],
            [
                deadline_error_test,
                deadline_absolute_ok_test,
                deadline_relative_ok_test
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
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(woody),
    ServiceURLs = mock_services_([
        {
            'Repository',
            {dmsl_domain_config_thrift, 'Repository'},
            fun('Checkout', _) -> {ok, ?SNAPSHOT} end
        }
    ], SupPid),
    Apps2 =
        capi_ct_helper:start_app(dmt_client, [{max_cache_size, #{}}, {service_urls, ServiceURLs}, {cache_update_interval, 50000}]) ++
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
    mock_services([{invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end}], MockServiceSup),
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
    GroupName == woody_errors;
    GroupName == deadline_header
->
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
    {ok, Token} = issue_token(BasePermissions, unlimited),
    {ok, Token2} = issue_token(BasePermissions, unlimited),
    {ok, Token3} = issue_token(<<"TEST2">>, BasePermissions, unlimited),
    Context = get_context(Token),
    Config2 = [{context_with_relative_deadline, get_context(Token2, <<"3s">>)} | Config],
    Config3 = [{context_with_absolute_deadline, Context} | Config2],
    Config4 = [{context_with_diff_party, get_context(Token3)} | Config3],
    [{context, Context} | Config4];

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
    true = (Time > 3000000) and (Time < 6000000).

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
    {ok, Token} = issue_token([], {lifetime, -10}),
    ?emptyresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_bad_deadline_error_test(config()) ->
    _.
authorization_bad_deadline_error_test(_Config) ->
    {ok, Token} = issue_token([], {deadline, -10}),
    ?emptyresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_error_no_header_test(config()) ->
    _.
authorization_error_no_header_test(_Config) ->
    Token = <<>>,
    ?emptyresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_error_no_permission_test(config()) ->
    _.
authorization_error_no_permission_test(_Config) ->
    {ok, Token} = issue_token([], {lifetime, 10}),
    ?emptyresp(401) = capi_client_parties:get_my_party(get_context(Token)).

-spec authorization_bad_token_error_test(config()) ->
    _.
authorization_bad_token_error_test(Config) ->
    {ok, Token} = issue_dummy_token([{[party], read}], Config),
    ?emptyresp(401) = capi_client_parties:get_my_party(get_context(Token)).

-spec create_invoice_ok_test(config()) ->
    _.
create_invoice_ok_test(Config) ->
    mock_services([{invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
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
    mock_services([{invoicing, fun('CreateWithTemplate', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    Req = #{
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>}
    },
    {ok, _} = capi_client_invoice_templates:create_invoice(?config(context, Config), ?STRING, Req).

-spec get_invoice_ok_test(config()) ->
    _.
get_invoice_ok_test(Config) ->
    mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    {ok, _} = capi_client_invoices:get_invoice_by_id(?config(context, Config), ?STRING).

-spec get_invoice_events_ok_test(config()) ->
    _.
get_invoice_events_ok_test(Config) ->
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
                ], if is_integer(ID) -> ID + 1; true -> 1 end, N)}
        end}
    ], Config),
    {ok, [#{<<"id">> := 1}, #{<<"id">> := 2}, #{<<"id">> := 4}]} =
        capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, 3),
    {ok, [#{<<"id">> := 4}, #{<<"id">> := 7}]} =
        capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, 2, 3).

-spec get_invoice_payment_methods_ok_test(config()) ->
    _.
get_invoice_payment_methods_ok_test(Config) ->
    mock_services([{invoicing, fun('ComputeTerms', _) -> {ok, ?TERM_SET} end}], Config),
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
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    Details0 = #{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => <<"test_invoice_template_product">>,
        <<"price">> => #{
            <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        }
    },
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req#{<<"details">> => Details0}),
    Details1 = #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency">> => ?RUB,
        <<"cart">> => [
            #{
                <<"product">> => ?STRING,
                <<"price">> => ?INTEGER,
                <<"quantity">> => ?INTEGER
            },
            #{
                <<"product">> => ?STRING,
                <<"price">> => ?INTEGER,
                <<"quantity">> => ?INTEGER,
                <<"taxMode">> => #{
                    <<"type">> => <<"InvoiceLineTaxVAT">>,
                    <<"rate">> => <<"18%">>
                }
            }
        ]
    },
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req#{<<"details">> => Details1}).

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
        <<"lifetime">> => get_lifetime(),
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    Details0 = #{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => <<"test_invoice_template_product">>,
        <<"price">> => #{
            <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        }
    },
    {ok, _} = capi_client_invoice_templates:update(?config(context, Config), ?STRING, Req#{<<"details">> => Details0}),
    Details1 = #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency">> => ?RUB,
        <<"cart">> => [
            #{
                <<"product">> => ?STRING,
                <<"price">> => ?INTEGER,
                <<"quantity">> => ?INTEGER
            }
        ]
    },
    {ok, _} = capi_client_invoice_templates:update(?config(context, Config), ?STRING, Req#{<<"details">> => Details1}).

-spec delete_invoice_template_ok_test(config()) ->
    _.
delete_invoice_template_ok_test(Config) ->
    mock_services([{invoice_templating, fun('Delete', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoice_templates:delete(?config(context, Config), ?STRING).

-spec get_invoice_payment_methods_by_tpl_id_ok_test(config()) ->
    _.
get_invoice_payment_methods_by_tpl_id_ok_test(Config) ->
    mock_services([{'invoice_templating', fun('ComputeTerms', _) -> {ok, ?TERM_SET} end}], Config),
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
            {cds_storage, fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end},
            {invoicing, fun('StartPayment', _) -> {ok, ?PAYPROC_PAYMENT} end},
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
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => Session,
            <<"paymentToolToken">> => Token,
            <<"contactInfo">> => #{
                <<"email">> => <<"bla@bla.ru">>
            }
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req2, ?STRING).

-spec create_first_recurrent_payment_ok_test(config()) ->
    _.
create_first_recurrent_payment_ok_test(Config) ->
    mock_services(
        [
            {cds_storage, fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end},
            {invoicing, fun('StartPayment', _) -> {ok, ?PAYPROC_PAYMENT} end},
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
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> => true,
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => Session,
            <<"paymentToolToken">> => Token,
            <<"contactInfo">> => #{
                <<"email">> => <<"bla@bla.ru">>
            }
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req2, ?STRING).

-spec create_second_recurrent_payment_ok_test(config()) ->
    _.
create_second_recurrent_payment_ok_test(Config) ->
    mock_services(
        [
            {cds_storage, fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end},
            {invoicing, fun('StartPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}
        ],
        Config
    ),
    Req2 = #{
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> => true,
        <<"payer">> => #{
            <<"payerType">> => <<"RecurrentPayer">>,
            <<"recurrentParentPayment">> => #{
                <<"invoiceID">> => <<"1">>,
                <<"paymentID">> => <<"2">>
            },
            <<"contactInfo">> => #{
                <<"email">> => <<"bla@bla.ru">>
            }
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req2, ?STRING).

-spec get_payments_ok_test(config()) ->
    _.
get_payments_ok_test(Config) ->
    mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    {ok, _} = capi_client_payments:get_payments(?config(context, Config), ?STRING).

-spec get_recurrent_payments_ok_test(config()) ->
    _.
get_recurrent_payments_ok_test(Config) ->
    Invoice = ?PAYPROC_INVOICE([?PAYPROC_PAYMENT(?RECURRENT_PAYMENT, [?REFUND], [?ADJUSTMENT])]),
    mock_services([{invoicing, fun('Get', _) -> {ok, Invoice} end}], Config),
    {ok, _} = capi_client_payments:get_payments(?config(context, Config), ?STRING).

-spec get_payment_by_id_ok_test(config()) ->
    _.
get_payment_by_id_ok_test(Config) ->
    mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    {ok, _} = capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec get_client_payment_status_test(config()) ->
    _.
get_client_payment_status_test(Config) ->
    {ok, #{
        <<"status">> := <<"failed">>,
        <<"error" >> := #{<<"code">> := <<"InvalidPaymentTool">>}
    }} = get_failed_payment_with_invalid_cvv(Config).

-spec get_merchant_payment_status_test(config()) ->
    _.
get_merchant_payment_status_test(Config) ->
    {ok, #{
        <<"status">> := <<"failed">>,
        <<"error" >> :=
            #{<<"code">> := <<"authorization_failed">>, <<"subError">> :=
                #{<<"code">> := <<"payment_tool_rejected">>,<<"subError">> :=
                    #{<<"code">> := <<"bank_card_rejected">>, <<"subError">> :=
                        #{<<"code">> := <<"cvv_invalid">>}}}}
    }} = get_failed_payment_with_invalid_cvv(Config).

-spec get_failed_payment_with_invalid_cvv(config()) ->
    _.
get_failed_payment_with_invalid_cvv(Config) ->
    Failure =
        payproc_errors:construct('PaymentFailure', {authorization_failed,
            {payment_tool_rejected,
                {bank_card_rejected,
                    {cvv_invalid, #payprocerr_GeneralFailure{}}
                }
            }
        }, <<"Reason">>),
    mock_services(
        [{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_FAILED_PAYMENT({failure, Failure})} end}],
        Config
    ),
    % mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec create_refund(config()) ->
    _.
create_refund(Config) ->
    mock_services([{invoicing, fun('RefundPayment', _) -> {ok, ?REFUND} end}], Config),
    Req = #{<<"reason">> => ?STRING},
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_refund_error(config()) ->
    _.
create_refund_error(Config) ->
    mock_services([
        {invoicing, fun
            ('RefundPayment', [_, <<"42">> | _]) ->
                throw(#payproc_InvalidPartyStatus{
                    status = {blocking, {blocked, #domain_Blocked{reason = ?STRING, since = ?TIMESTAMP}}}
                });
            ('RefundPayment', [_, <<"43">> | _]) ->
                throw(#payproc_InvalidContractStatus{
                    status = {expired, #domain_ContractExpired{}}
                })
        end}
    ], Config),
    Req = #{<<"reason">> => ?STRING},
    {error, {400, _}} = capi_client_payments:create_refund(?config(context, Config), Req, <<"42">>, ?STRING),
    {error, {400, _}} = capi_client_payments:create_refund(?config(context, Config), Req, <<"43">>, ?STRING).

-spec create_partial_refund(config()) ->
    _.
create_partial_refund(Config) ->
    mock_services([{invoicing, fun('RefundPayment', _) -> {ok, ?REFUND} end}], Config),
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
        {
            invoicing,
            fun
                ('GetPayment', _) ->
                    {ok, ?PAYPROC_PAYMENT};
                ('RefundPayment', _) ->
                    {ok, ?REFUND}
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
    mock_services([{invoicing, fun('GetPaymentRefund', _) -> {ok, ?REFUND} end}], Config),
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
    mock_services([{invoicing, fun('CapturePayment', _) -> {ok, ok} end}], Config),
    ok = capi_client_payments:capture_payment(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec create_visa_payment_resource_ok_test(_) ->
    _.
create_visa_payment_resource_ok_test(Config) ->
    mock_services([
        {cds_storage, fun
            ('PutCardData', [
                #'CardData'{pan = <<"411111", _:6/binary, Mask:4/binary>>},
                #'SessionData'{
                    auth_data = {card_security_code, #'CardSecurityCode'{
                        value = <<"232">>
                    }}
                }
            ]) ->
                {ok, #'PutCardDataResult'{
                    bank_card = #domain_BankCard{
                        token = ?STRING,
                        payment_system = visa,
                        bin = <<"411111">>,
                        masked_pan = Mask
                    },
                    session_id = ?STRING
                }}
        end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
        <<"paymentSystem">> := <<"visa">>,
        <<"lastDigits">> := <<"1111">>,
        <<"bin">> := <<"411111">>,
        <<"cardNumberMask">> := <<"411111******1111">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"4111111111111111">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_nspkmir_payment_resource_ok_test(_) ->
    _.
create_nspkmir_payment_resource_ok_test(Config) ->
    mock_services([
        {cds_storage, fun
            ('PutCardData', [
                #'CardData'{pan = <<"22001111", _:6/binary, Mask:2/binary>>},
                #'SessionData'{
                    auth_data = {card_security_code, #'CardSecurityCode'{
                        value = <<"232">>
                    }}
                }
            ]) ->
                {ok, #'PutCardDataResult'{
                    bank_card = #domain_BankCard{
                        token = ?STRING,
                        payment_system = nspkmir,
                        bin = <<"22001111">>,
                        masked_pan = Mask
                    },
                    session_id = ?STRING
                }}
        end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT(<<"NSPK MIR">>)} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
        <<"paymentSystem">> := <<"nspkmir">>,
        <<"cardNumberMask">> := <<"22001111******11">>,
        <<"lastDigits">> := <<"11">>,
        <<"bin">> := <<"22001111">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardNumber">> => <<"2200111111111111">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_euroset_payment_resource_ok_test(_) ->
    _.
create_euroset_payment_resource_ok_test(Config) ->
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsPaymentTerminal">>,
        <<"provider">> := <<"euroset">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"PaymentTerminalData">>,
            <<"provider">> => <<"euroset">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_qw_payment_resource_ok_test(_) ->
    _.
create_qw_payment_resource_ok_test(Config) ->
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsDigitalWallet">>,
        <<"digitalWalletDetailsType">> := <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask">> := <<"+7******3210">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"DigitalWalletData">>,
            <<"digitalWalletType">> => <<"DigitalWalletQIWI">>,
            <<"phoneNumber">> => <<"+79876543210">>
        },
        <<"clientInfo">> => ClientInfo
    }).

-spec create_applepay_tokenized_payment_resource_ok_test(_) ->
    _.
create_applepay_tokenized_payment_resource_ok_test(Config) ->
    mock_services([
        {payment_tool_provider_apple_pay, fun('Unwrap', _) -> {ok, ?UNWRAPPED_PAYMENT_TOOL(?APPLE_PAY_DETAILS)} end},
        {cds_storage, fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{<<"paymentSystem">> := <<"mastercard">>}}} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"ApplePay">>,
                <<"merchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }).

-spec create_googlepay_tokenized_payment_resource_ok_test(_) ->
    _.
create_googlepay_tokenized_payment_resource_ok_test(Config) ->
    mock_services([
        {payment_tool_provider_google_pay, fun('Unwrap', _) -> {ok, ?UNWRAPPED_PAYMENT_TOOL(?GOOGLE_PAY_DETAILS)} end},
        {cds_storage, fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end},
        {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end}
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"paymentSystem">> := <<"mastercard">>,
        <<"tokenProvider">> := <<"googlepay">>
    }}} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"GooglePay">>,
                <<"gatewayMerchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }).

-spec create_googlepay_plain_payment_resource_ok_test(_) ->
    _.
create_googlepay_plain_payment_resource_ok_test(Config) ->
    mock_services([
        {payment_tool_provider_google_pay,
            fun('Unwrap', _) ->
                {ok, ?UNWRAPPED_PAYMENT_TOOL(
                    ?GOOGLE_PAY_DETAILS,
                    {card, #paytoolprv_Card{
                        pan = <<"1234567890123456">>,
                        exp_date = #paytoolprv_ExpDate{month = 10, year = 2018}
                    }}
                )}
            end
        },
        {cds_storage,
            fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end
        },
        {binbase,
            fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT} end
        }
    ], Config),
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := Details = #{<<"paymentSystem">> := <<"mastercard">>}}} =
        capi_client_tokens:create_payment_resource(?config(context, Config), #{
            <<"paymentTool">> => #{
                <<"paymentToolType">> => <<"TokenizedCardData">>,
                <<"provider">> => <<"GooglePay">>,
                <<"gatewayMerchantID">> => <<"SomeMerchantID">>,
                <<"paymentToken">> => #{}
            },
            <<"clientInfo">> => ClientInfo
        }),
    false = maps:is_key(<<"tokenProvider">>, Details).

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
            },
            <<"paymentInstitutionID">> => ?INTEGER
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
            <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
            <<"payoutToolID">> => ?STRING,
            <<"currency">> => ?USD,
            <<"details">> => #{
                <<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>,
                <<"number">> => <<"12345678901234567890">>,
                <<"iban">> => <<"GR1601101250000000012300695">>,
                <<"bankDetails">> => #{
                    <<"bik">> => <<"123456789">>,
                    <<"countryCode">> => <<"USA">>,
                    <<"name">> => <<"testUsaBankName">>,
                    <<"address">> => ?STRING
                },
                <<"correspondentBankAccount">> => #{
                    <<"number">> => <<"00000000000000000000">>
                }
            }
        },
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ?STRING,
            <<"contractModificationType">> => <<"ContractPayoutToolInfoModification">>,
            <<"payoutToolID">> => ?STRING,
            <<"details">> => #{
                <<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>,
                <<"number">> => <<"12345678901234567890">>,
                <<"iban">> => <<"GR1601101250000000012300695">>,
                <<"bankDetails">> => #{
                    <<"aba_rtn">> => <<"129131673">>,
                    <<"countryCode">> => <<"USA">>,
                    <<"name">> => <<"testUsaBankName">>,
                    <<"address">> => ?STRING
                },
                <<"correspondentBankAccount">> => #{
                    <<"number">> => <<"00000000000000000000">>
                }
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

-spec create_claim_invalid_residence_test(config()) ->
    _.
create_claim_invalid_residence_test(Config) ->
    mock_services([{party_management, fun('CreateClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}], Config),
    Changeset = [
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ?STRING,
            <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
            <<"payoutToolID">> => ?STRING,
            <<"currency">> => ?USD,
            <<"details">> => #{
                <<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>,
                <<"number">> => <<"12345678901234567890">>,
                <<"iban">> => <<"GR1601101250000000012300695">>,
                <<"bankDetails">> => #{
                    <<"bik">> => <<"123456789">>,
                    <<"countryCode">> => <<"EUR">>,
                    <<"name">> => <<"testBankName">>,
                    <<"address">> => ?STRING
                },
                <<"correspondentBankAccount">> => #{
                    <<"number">> => <<"00000000000000000000">>
                }
            }
        }
    ],
    {error, {400, _}} = capi_client_claims:create_claim(?config(context, Config), Changeset).

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
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_US),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?WALLET_TOOL).

-spec create_payout(config()) ->
    _.
create_payout(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, []),
    mock_services([{payouts, fun('CreatePayout', _) -> {ok, Payout} end}], Config),
    Req = #{
        <<"id">> => ?STRING,
        <<"shopID">> => ?STRING,
        <<"payoutToolID">> => ?WALLET_TOOL,
        <<"amount">> => 2,
        <<"currency">> => <<"RUB">>,
        <<"metadata">> => #{
            <<"payoutBinary">> => <<"sample data">>,
            <<"payoutInt">> => 5,
            <<"payoutList">> => [
                <<"some_1">>,
                <<"some_2">>
            ],
            <<"payoutMap">> => #{
                <<"someKey">> => 234
            },
            <<"how_about_null">> => null
        }
    },
    {ok, _} = capi_client_payouts:create_payout(?config(context, Config), Req, ?STRING).

-spec get_payout(config()) ->
    _.
get_payout(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, [?PAYOUT_PROC_PAYOUT_SUMMARY_ITEM]),
    mock_services([{payouts, fun('Get', _) -> {ok, Payout} end}], Config),
    {ok, _} = capi_client_payouts:get_payout(?config(context, Config), ?STRING).

-spec get_payout_fail(config()) ->
    _.
get_payout_fail(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, [?PAYOUT_PROC_PAYOUT_SUMMARY_ITEM]),
    mock_services([{payouts, fun('Get', _) -> {ok, Payout} end}], Config),
    {error, {400, _}} = capi_client_payouts:get_payout(?config(context_with_diff_party, Config), ?STRING).

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
            <<"eventTypes">> => [ <<"InvoiceCreated">>
                                , <<"InvoicePaid">>
                                , <<"InvoiceCancelled">>
                                , <<"InvoiceFulfilled">>
                                , <<"PaymentStarted">>
                                , <<"PaymentProcessed">>
                                , <<"PaymentCaptured">>
                                , <<"PaymentCancelled">>
                                , <<"PaymentRefunded">>
                                , <<"PaymentFailed">>
                                , <<"PaymentRefundCreated">>
                                , <<"PaymentRefundFailed">>
                                , <<"PaymentRefundSucceeded">>
                                ]
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {invoiceStatus, <<"fulfilled">>},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.1">>},
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
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>}
    ],

    {ok, _, _} = capi_client_searches:search_invoices(?config(context, Config), ?STRING, Query).

-spec search_payments_ok_test(config()) ->
    _.
search_payments_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}], Config),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
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
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>}
    ],

    {ok, _, _} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_refunds_ok_test(config()) ->
    _.
search_refunds_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {shopID, <<"testShopID">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {refundID, <<"testRefundID">>},
        {refundStatus, <<"succeeded">>}
    ],

    {ok, _, _} = capi_client_searches:search_refunds(?config(context, Config), ?STRING, Query).

-spec search_payouts_ok_test(config()) ->
    _.
search_payouts_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetPayouts', _) -> {ok, ?STAT_RESPONSE_PAYOUTS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {shopID, <<"testShopID">>},
        {payoutID, <<"testPayoutID">>},
        {payoutToolType, <<"Wallet">>}
    ],

    {ok, _, _} = capi_client_searches:search_payouts(?config(context, Config), ?STRING, Query).

-spec get_payment_conversion_stats_ok_test(_) ->
    _.
get_payment_conversion_stats_ok_test(Config) ->
    mock_services([{merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1},
        {paymentMethod, <<"bankCard">>}
    ],
    {ok, _} = capi_client_analytics:get_payment_method_stats(?config(context, Config), ?STRING, Query).

-spec get_reports_ok_test(config()) ->
    _.
get_reports_ok_test(Config) ->
    mock_services([{reporting, fun('GetReports', _) -> {ok, [?REPORT]} end}], Config),
    {ok, _} = capi_client_reports:get_reports(?config(context, Config), ?STRING, ?TIMESTAMP, ?TIMESTAMP).

-spec get_report_ok_test(config()) ->
    _.
get_report_ok_test(Config) ->
    mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT} end}], Config),
    {ok, _} = capi_client_reports:get_report(?config(context, Config), ?STRING, ?INTEGER).

-spec create_report_ok_test(config()) ->
    _.
create_report_ok_test(Config) ->
    mock_services([
        {reporting, fun
            ('GenerateReport', _)           -> {ok, ?INTEGER};
            ('GetReport', [_, _, ?INTEGER]) -> {ok, ?REPORT}
        end}
    ], Config),
    {ok, _} = capi_client_reports:create_report(?config(context, Config), ?STRING, ?REPORT_TYPE, ?TIMESTAMP, ?TIMESTAMP).

-spec download_report_file_ok_test(_) ->
    _.
download_report_file_ok_test(Config) ->
    mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT}; ('GeneratePresignedUrl', _) -> {ok, ?STRING} end}], Config),
    {ok, _} = capi_client_reports:download_file(?config(context, Config), ?STRING, ?INTEGER, ?STRING).

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
    {ok, []} = capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"live">>),
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
    {ok, _} = capi_client_payment_institutions:get_payment_institution_payment_terms(?config(context, Config), ?INTEGER).

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
            {cds_storage, fun('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT} end},
            {customer_management, fun('StartBinding', _) -> {ok, ?CUSTOMER_BINDING} end},
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

-spec deadline_error_test(config()) ->
    _.
deadline_error_test(_Config) ->
    {ok, Token} = issue_token([], {deadline, 4102444800}), % 01/01/2100 @ 12:00am (UTC)
    {error, {400, _}} = capi_client_categories:get_categories(get_context(Token, <<"blabla">>)).

-spec deadline_absolute_ok_test(config()) ->
    _.
deadline_absolute_ok_test(Config) ->
    Context = ?config(context_with_absolute_deadline, Config),
    _ = mock_services([{party_management, fun('Get', _) -> timer:sleep(5000), {ok, ?PARTY} end}], Config),
    Deadline = woody_deadline:from_timeout(3000),
    BinDeadline = woody_deadline:to_binary(Deadline),
    ?badresp(504) = capi_client_parties:get_my_party(Context#{deadline => BinDeadline}),
    Deadline2 = woody_deadline:from_timeout(3000),
    BinDeadline2 = woody_deadline:to_binary(Deadline2),
    {ok, _} = capi_client_categories:get_categories(Context#{deadline => BinDeadline2}).

-spec deadline_relative_ok_test(config()) ->
    _.
deadline_relative_ok_test(Config) ->
    Context = ?config(context_with_relative_deadline, Config),
    _ = mock_services([{party_management, fun('Get', _) -> timer:sleep(10000), {ok, ?PARTY} end}], Config),
    ?badresp(504) = capi_client_parties:get_my_party(Context),
    {ok, _} = capi_client_categories:get_categories(Context).

%%

issue_token(ACL, LifeTime) ->
    issue_token(?STRING, ACL, LifeTime).

issue_token(PartyID, ACL, LifeTime) ->
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

get_context(Token, Deadline) ->
    DefEvtHandler = capi_client_lib:default_event_handler(),
    capi_client_lib:get_context(?CAPI_URL, Token, 10000, ipv4, DefEvtHandler, Deadline).


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
