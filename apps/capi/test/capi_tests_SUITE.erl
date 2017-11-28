-module(capi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("dmsl/include/dmsl_webhooker_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").
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
    create_refund/1,
    get_refund_by_id/1,
    get_refunds/1,
    cancel_payment_ok_test/1,
    capture_payment_ok_test/1,

    create_payment_tool_token_ok_test/1,

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

    get_categories_ok_test/1,
    get_category_by_ref_ok_test/1,

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
        get_payments_ok_test,
        get_payment_by_id_ok_test,
        cancel_payment_ok_test,
        capture_payment_ok_test,
        create_payment_tool_token_ok_test
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
                get_categories_ok_test,
                get_category_by_ref_ok_test,
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
    Apps =
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(woody) ++
        start_capi(Config),
    [{apps, lists:reverse(Apps)} | Config].

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(operations_by_invoice_access_token_after_invoice_creation, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = get_token([{[invoices], write}], unlimited),
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
    {ok, Token} = get_token([{[invoices], write}], unlimited),
    mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], MockServiceSup),
    {ok, #{<<"payload">> := InvAccToken}
    } = capi_client_invoices:create_invoice_access_token(get_context(Token), ?STRING),
    stop_mocked_service_sup(MockServiceSup),
    [{context, get_context(InvAccToken)} | Config];

init_per_group(operations_by_invoice_template_access_token, Config) ->
    MockServiceSup = start_mocked_service_sup(),
    {ok, Token} = get_token([{[party], write}], unlimited),
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
    {ok, Token} = get_token([{[customers], write}], unlimited),
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
    {ok, Token} = get_token([{[customers], write}], unlimited),
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
    {ok, Token} = get_token(BasePermissions, unlimited),
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

-spec woody_unknown_test(config()) ->
    _.

woody_unknown_test(Config) ->
    _ = mock_services([{party_management, fun('Get', _) -> timer:sleep(60000) end}], Config),
    ?badresp(504) = capi_client_parties:get_my_party(?config(context, Config)).

-spec authorization_positive_lifetime_ok_test(config()) ->
    _.
authorization_positive_lifetime_ok_test(Config) ->
    mock_services([{repository, fun('Checkout', _) -> {ok, ?SNAPSHOT} end}], Config),
    {ok, Token} = get_token([], {lifetime, 10}),
    {ok, _} = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_unlimited_lifetime_ok_test(config()) ->
    _.
authorization_unlimited_lifetime_ok_test(Config) ->
    mock_services([{repository, fun('Checkout', _) -> {ok, ?SNAPSHOT} end}], Config),
    {ok, Token} = get_token([], unlimited),
    {ok, _} = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_far_future_deadline_ok_test(config()) ->
    _.
authorization_far_future_deadline_ok_test(Config) ->
    mock_services([{repository, fun('Checkout', _) -> {ok, ?SNAPSHOT} end}], Config),
    {ok, Token} = get_token([], {deadline, 4102444800}), % 01/01/2100 @ 12:00am (UTC)
    {ok, _} = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_permission_ok_test(config()) ->
    _.
authorization_permission_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, Token} = get_token([{[party], read}], unlimited),
    {ok, _} = capi_client_parties:get_my_party(get_context(Token)).

-spec authorization_negative_lifetime_error_test(config()) ->
    _.
authorization_negative_lifetime_error_test(_Config) ->
    {ok, Token} = get_token([], {lifetime, -10}),
    ?badresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_bad_deadline_error_test(config()) ->
    _.
authorization_bad_deadline_error_test(_Config) ->
    {ok, Token} = get_token([], {deadline, -10}),
    ?badresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_error_no_header_test(config()) ->
    _.
authorization_error_no_header_test(_Config) ->
    Token = <<>>,
    ?badresp(401) = capi_client_categories:get_categories(get_context(Token)).

-spec authorization_error_no_permission_test(config()) ->
    _.
authorization_error_no_permission_test(_Config) ->
    {ok, Token} = get_token([], {lifetime, 10}),
    ?badresp(401) = capi_client_parties:get_my_party(get_context(Token)).

-spec authorization_bad_token_error_test(config()) ->
    _.
authorization_bad_token_error_test(Config) ->
    {ok, Token} = get_dummy_token([{[party], read}], Config),
    ?badresp(401) = capi_client_parties:get_my_party(get_context(Token)).

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
    mock_services([{invoicing, fun('GetEvents', _) -> {ok, [?INVOICE_EVENT]} end}], Config),
    Limit = 10,
    {ok, Events1} = capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, Limit),
    {ok, Events2} = capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, 10, Limit),
    true = ((length(Events1) =< Limit) andalso (length(Events2) =< Limit)).

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
            {invoicing, fun('StartPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}
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
    mock_services([{invoicing, fun('RefundPayment', _) -> {ok, ?REFUND} end}], Config),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), ?STRING, ?STRING, <<>>).

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

-spec create_payment_tool_token_ok_test(_) ->
    _.
create_payment_tool_token_ok_test(Config) ->
    mock_services([
        {cds_storage, fun
            ('PutCardData', [#'CardData'{pan = <<"411111", _:6/binary, Mask:4/binary>>}]) ->
                {ok, #'PutCardDataResult'{
                    bank_card = #domain_BankCard{
                        token = ?STRING,
                        payment_system = visa,
                        bin = <<"411111">>,
                        masked_pan = Mask
                    },
                    session_id = ?STRING
                }};
            ('PutCardData', [#'CardData'{pan = <<"22001111", _:6/binary, Mask:2/binary>>}]) ->
                {ok, #'PutCardDataResult'{
                    bank_card = #domain_BankCard{
                        token = ?STRING,
                        payment_system = nspkmir,
                        bin = <<"22001111">>,
                        masked_pan = Mask
                    },
                    session_id = ?STRING
                }}
        end}
    ], Config),
    PaymentTool = #{
        <<"paymentToolType">> => <<"CardData">>,
        <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
        <<"expDate">> => <<"08/27">>,
        <<"cvv">> => <<"232">>
    },
    ClientInfo = #{<<"fingerprint">> => <<"test fingerprint">>},
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
        <<"paymentSystem">> := <<"visa">>,
        <<"cardNumberMask">> := <<"1111">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => PaymentTool#{<<"cardNumber">> => <<"4111111111111111">>},
        <<"clientInfo">> => ClientInfo
    }),
    {ok, #{<<"paymentToolDetails">> := #{
        <<"detailsType">> := <<"PaymentToolDetailsBankCard">>,
        <<"paymentSystem">> := <<"nspkmir">>,
        <<"cardNumberMask">> := <<"11">>
    }}} = capi_client_tokens:create_payment_resource(?config(context, Config), #{
        <<"paymentTool">> => PaymentTool#{<<"cardNumber">> => <<"2200111111111111">>},
        <<"clientInfo">> => ClientInfo
    }).

-spec get_my_party_ok_test(config()) ->
    _.
get_my_party_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)).

-spec suspend_my_party_ok_test(config()) ->
    _.
suspend_my_party_ok_test(Config) ->
    mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    capi_client_parties:suspend_my_party(?config(context, Config)).

-spec activate_my_party_ok_test(config()) ->
    _.
activate_my_party_ok_test(Config) ->
    mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    capi_client_parties:activate_my_party(?config(context, Config)).

-spec get_shop_by_id_ok_test(config()) ->
    _.
get_shop_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('GetShop', _) -> {ok, ?SHOP} end}], Config),
    capi_client_shops:get_shop_by_id(?config(context, Config), ?STRING).

-spec get_shops_ok_test(config()) ->
    _.
get_shops_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_shops:get_shops(?config(context, Config)).

-spec suspend_shop_ok_test(config()) ->
    _.
suspend_shop_ok_test(Config) ->
    mock_services([{party_management, fun('SuspendShop', _) -> {ok, ok} end}], Config),
    capi_client_shops:suspend_shop(?config(context, Config), ?STRING).

-spec activate_shop_ok_test(config()) ->
    _.
activate_shop_ok_test(Config) ->
    mock_services([{party_management, fun('ActivateShop', _) -> {ok, ok} end}], Config),
    capi_client_shops:activate_shop(?config(context, Config), ?STRING).

-spec get_claim_by_id_ok_test(config()) ->
    _.
get_claim_by_id_ok_test(Config) ->
    mock_services([{party_management, fun('GetClaim', _) -> {ok, ?CLAIM} end}], Config),
    {ok, _} = capi_client_claims:get_claim_by_id(?config(context, Config), ?INTEGER).

-spec get_claims_ok_test(config()) ->
    _.
get_claims_ok_test(Config) ->
    mock_services([{party_management, fun('GetClaims', _) -> {ok, [?CLAIM]} end}], Config),
    {ok, _} = capi_client_claims:get_claims(?config(context, Config)).

-spec revoke_claim_ok_test(config()) ->
    _.
revoke_claim_ok_test(Config) ->
    mock_services([{party_management, fun('RevokeClaim', _) -> {ok, ok} end}], Config),
    ok = capi_client_claims:revoke_claim_by_id(?config(context, Config), ?STRING, ?INTEGER, ?INTEGER).

-spec create_claim_ok_test(config()) ->
    _.
create_claim_ok_test(Config) ->
    mock_services([{party_management, fun('CreateClaim', _) -> {ok, ?CLAIM} end}], Config),
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
    mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?STRING).

-spec get_contracts_ok_test(config()) ->
    _.
get_contracts_ok_test(Config) ->
    mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_contracts:get_contracts(?config(context, Config)).

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
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?STRING).

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
        {payerEmail, <<"test@test_rbk.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        %%{cardNumberMask, <<"2222">>},  %%@FIXME cannot be used until getting the newest api client
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.0.1">>},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerEmail, <<"test@test_rbk.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        %% {cardNumberMask, <<"2222">>}, %%@FIXME cannot be used until getting the newest api client
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
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}}
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

-spec download_report_file_ok_test(_) ->
    _.
download_report_file_ok_test(Config) ->
    mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT}; ('GeneratePresignedUrl', _) -> {ok, ?STRING} end}], Config),
    {ok, _} = capi_client_reports:download_file(?config(context, Config), ?STRING, ?INTEGER, ?STRING).

-spec get_categories_ok_test(config()) ->
    _.
get_categories_ok_test(Config) ->
    mock_services([{repository, fun('Checkout', _) -> {ok, ?SNAPSHOT} end}], Config),
    {ok, _} = capi_client_categories:get_categories(?config(context, Config)).

-spec get_category_by_ref_ok_test(config()) ->
    _.
get_category_by_ref_ok_test(Config) ->
    mock_services([{repository, fun('Checkout', _) -> {ok, ?SNAPSHOT} end}], Config),
    {ok, _} = capi_client_categories:get_category_by_ref(?config(context, Config), ?INTEGER).

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
            {customer_management, fun('StartBinding', _) -> {ok, ?CUSTOMER_BINDING} end}
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

%%

get_token(ACL, LifeTime) ->
    PartyID = ?STRING,
    Claims = #{?STRING => ?STRING},
    capi_authorizer_jwt:issue({{PartyID, capi_acl:from_list(ACL)}, Claims}, LifeTime).

get_dummy_token(ACL, Config) ->
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

start_mocked_service_sup() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    SupPid.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

mock_services(Services, SupPid) when is_pid(SupPid) ->
    mock_services(Services, [{test_sup, SupPid}]);

mock_services(Services, Config) when is_list(Config) ->
    Module = capi_dummy_service,
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?CAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Module},
        #{
            ip => IP,
            port => Port,
            event_handler => capi_woody_event_handler,
            handlers => lists:map(
                fun({Service, Fun}) ->
                    WoodyService = proplists:get_value(Service, woody_services()),
                    {make_path(Service), {WoodyService, {Module, #{function => Fun}}}}
                end,
                Services
            )
        }
    ),
    {ok, _} = supervisor:start_child(?config(test_sup, Config), ChildSpec),
    ServiceURLs = lists:foldl(
        fun({Service, _}, Acc) ->
            URL = iolist_to_binary(["http://", ?CAPI_HOST_NAME, ":", integer_to_list(Port), make_path(Service)]),
            Acc#{Service => URL}
        end,
        #{},
        Services
    ),
    capi_ct_helper:start_app(capi_woody_client, [{service_urls, ServiceURLs}]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

get_random_port() ->
    rand:uniform(32768) + 32767.

get_context(Token) ->
    capi_client_lib:get_context(?CAPI_URL, Token, 10000, ipv4).

woody_services() ->
    [
        {invoicing, {dmsl_payment_processing_thrift, 'Invoicing'}},
        {invoice_templating, {dmsl_payment_processing_thrift, 'InvoiceTemplating'}},
        {accounter, {dmsl_accounter_thrift, 'Accounter'}},
        {party_management, {dmsl_payment_processing_thrift, 'PartyManagement'}},
        {cds_storage, {dmsl_cds_thrift, 'Storage'}},
        {repository, {dmsl_domain_config_thrift, 'Repository'}},
        {webhook_manager, {dmsl_webhooker_thrift, 'WebhookManager'}},
        {geo_ip_service, {dmsl_geo_ip_thrift, 'GeoIpService'}},
        {merchant_stat, {dmsl_merch_stat_thrift, 'MerchantStatistics'}},
        {reporting, {dmsl_reporting_thrift, 'Reporting'}},
        {customer_management, {dmsl_payment_processing_thrift, 'CustomerManagement'}}
    ].

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
