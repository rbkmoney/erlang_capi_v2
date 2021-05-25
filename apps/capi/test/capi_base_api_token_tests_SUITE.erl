-module(capi_base_api_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").
-include_lib("damsel/include/dmsl_payout_processing_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("capi_bouncer_data.hrl").

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
    create_invoice_ok_test/1,
    create_invoice_autorization_error_test/1,
    get_invoice_by_external_id/1,
    create_invoice_access_token_ok_test/1,
    create_invoice_template_ok_test/1,
    create_invoice_with_template_test/1,
    create_invoice_template_autorization_error_test/1,
    create_customer_ok_test/1,
    create_customer_autorization_error_test/1,
    delete_customer_ok_test/1,
    create_customer_access_token_ok_test/1,
    rescind_invoice_ok_test/1,
    fulfill_invoice_ok_test/1,
    get_merchant_payment_status_test/1,
    create_payment_ok_test/1,
    create_refund/1,
    create_refund_blocked_error/1,
    create_refund_expired_error/1,
    create_partial_refund/1,
    create_partial_refund_without_currency/1,
    get_refund_by_id/1,
    get_refunds/1,
    get_chargeback_by_id/1,
    get_refund_by_external_id/1,
    update_invoice_template_ok_test/1,
    delete_invoice_template_ok_test/1,
    get_account_by_id_ok_test/1,
    get_my_party_ok_test/1,
    suspend_my_party_ok_test/1,
    activate_my_party_ok_test/1,
    get_party_by_id_ok_test/1,
    suspend_party_by_id_ok_test/1,
    activate_party_by_id_ok_test/1,
    get_shop_by_id_ok_test/1,
    get_shops_ok_test/1,
    activate_shop_ok_test/1,
    suspend_shop_ok_test/1,
    get_shop_by_id_for_party_ok_test/1,
    get_shops_for_party_ok_test/1,
    suspend_shop_for_party_ok_test/1,
    activate_shop_for_party_ok_test/1,
    get_shop_by_id_for_party_error_test/1,
    get_shops_for_party_error_test/1,
    suspend_shop_for_party_error_test/1,
    activate_shop_for_party_error_test/1,
    get_claim_by_id_ok_test/1,
    get_claims_ok_test/1,
    revoke_claim_ok_test/1,
    create_claim_ok_test/1,
    update_claim_by_id_test/1,
    create_claim_invalid_residence_test/1,
    get_contract_by_id_ok_test/1,
    get_contract_by_id_for_party_ok_test/1,
    get_contracts_ok_test/1,
    get_contracts_for_party_ok_test/1,
    get_contract_adjustments_ok_test/1,
    get_contract_adjustments_for_party_ok_test/1,
    get_contract_adjustment_by_id_ok_test/1,
    get_contract_adjustment_by_id_for_party_ok_test/1,
    get_payout_tools_ok_test/1,
    get_payout_tools_for_party_ok_test/1,
    get_payout_tool_by_id/1,
    get_payout_tool_by_id_for_party/1,
    create_payout/1,
    get_payout/1,
    create_payout_autorization_error/1,
    get_payout_fail/1,
    create_webhook_ok_test/1,
    create_webhook_limit_exceeded_test/1,
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
    get_report_not_found_test/1,
    create_report_ok_test/1,
    download_report_file_ok_test/1,
    download_report_file_not_found_test/1,
    get_categories_ok_test/1,
    get_category_by_ref_ok_test/1,
    get_schedule_by_ref_ok_test/1,
    get_payment_institutions/1,
    get_payment_institution_by_ref/1,
    get_payment_institution_payment_terms/1,
    get_payment_institution_payout_terms/1,
    get_payment_institution_payout_schedules/1,
    check_no_payment_by_external_id_test/1,
    check_no_internal_id_for_external_id_test/1,
    retrieve_payment_by_external_id_test/1,
    check_no_invoice_by_external_id_test/1,
    check_support_decrypt_v2_test/1
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
        {group, operations_by_base_api_token},
        {group, operations_by_base_api_token_with_new_auth},
        {group, payment_tool_token_support}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_base_api_token, [], [
            get_merchant_payment_status_test,
            update_invoice_template_ok_test,
            delete_invoice_template_ok_test,
            get_my_party_ok_test,
            suspend_my_party_ok_test,
            activate_my_party_ok_test,
            get_shop_by_id_ok_test,
            get_shops_ok_test,
            activate_shop_ok_test,
            suspend_shop_ok_test,

            get_payment_conversion_stats_ok_test,
            get_payment_revenue_stats_ok_test,
            get_payment_geo_stats_ok_test,
            get_payment_rate_stats_ok_test,
            get_payment_method_stats_ok_test,
            get_reports_ok_test,
            get_report_ok_test,
            get_report_not_found_test,
            create_report_ok_test,
            download_report_file_ok_test,
            download_report_file_not_found_test,
            get_category_by_ref_ok_test,
            get_schedule_by_ref_ok_test,
            delete_customer_ok_test,

            retrieve_payment_by_external_id_test,
            check_no_invoice_by_external_id_test
        ]},
        {operations_by_base_api_token_with_new_auth, [], [
            create_customer_ok_test,
            create_customer_autorization_error_test,
            delete_customer_ok_test,
            create_customer_access_token_ok_test,

            create_invoice_ok_test,
            create_invoice_autorization_error_test,
            get_invoice_by_external_id,
            create_invoice_access_token_ok_test,
            create_invoice_template_ok_test,
            create_invoice_template_autorization_error_test,
            create_invoice_with_template_test,
            rescind_invoice_ok_test,
            fulfill_invoice_ok_test,
            update_invoice_template_ok_test,
            delete_invoice_template_ok_test,

            get_my_party_ok_test,
            suspend_my_party_ok_test,
            activate_my_party_ok_test,

            get_party_by_id_ok_test,
            suspend_party_by_id_ok_test,
            activate_party_by_id_ok_test,

            get_locations_names_ok_test,

            get_account_by_id_ok_test,
            get_categories_ok_test,

            get_claim_by_id_ok_test,
            get_claims_ok_test,
            revoke_claim_ok_test,
            create_claim_ok_test,
            update_claim_by_id_test,
            create_claim_invalid_residence_test,
            get_contract_by_id_ok_test,
            get_contract_by_id_for_party_ok_test,
            get_contracts_ok_test,
            get_contracts_for_party_ok_test,
            get_contract_adjustments_ok_test,
            get_contract_adjustments_for_party_ok_test,
            get_contract_adjustment_by_id_ok_test,
            get_contract_adjustment_by_id_for_party_ok_test,

            get_shop_by_id_for_party_ok_test,
            get_shop_by_id_for_party_error_test,
            get_shops_for_party_ok_test,
            get_shops_for_party_error_test,
            suspend_shop_for_party_ok_test,
            suspend_shop_for_party_error_test,
            activate_shop_for_party_ok_test,
            activate_shop_for_party_error_test,

            create_payment_ok_test,
            check_no_payment_by_external_id_test,
            create_refund,
            create_refund_blocked_error,
            create_refund_expired_error,
            create_partial_refund,
            create_partial_refund_without_currency,
            get_chargeback_by_id,
            get_refund_by_id,
            get_refunds,
            get_refund_by_external_id,
            check_no_internal_id_for_external_id_test,

            get_payment_institutions,
            get_payment_institution_by_ref,
            get_payment_institution_payment_terms,
            get_payment_institution_payout_terms,
            get_payment_institution_payout_schedules,

            create_webhook_ok_test,
            create_webhook_limit_exceeded_test,
            get_webhooks,
            get_webhook_by_id,
            delete_webhook_by_id,

            get_payout_tools_ok_test,
            get_payout_tools_for_party_ok_test,
            get_payout_tool_by_id,
            get_payout_tool_by_id_for_party,
            create_payout,
            create_payout_autorization_error,
            get_payout,
            get_payout_fail,
            search_invoices_ok_test,
            search_payments_ok_test,
            search_refunds_ok_test,
            search_payouts_ok_test
        ]},
        {payment_tool_token_support, [], [
            check_support_decrypt_v2_test
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
init_per_group(operations_by_base_api_token, Config) ->
    BasePermissions = get_base_permissions(),
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{context, capi_ct_helper:get_context(Token)}, {group_apps, Apps1}, {group_test_sup, SupPid} | Config];
init_per_group(operations_by_base_api_token_with_new_auth, Config) ->
    BasePermissions = get_base_permissions(),
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    [{context, capi_ct_helper:get_context(Token)} | Config];
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

get_base_permissions() ->
    [
        {[invoices], write},
        {[invoices], read},
        {[party], write},
        {[party], read},
        {[invoices, payments], write},
        {[invoices, payments], read},
        {[customers], write},
        {[payouts], write},
        {[payouts], read}
    ].

%%% Tests

-spec create_invoice_ok_test(config()) -> _.
create_invoice_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoice">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_invoices:create_invoice(?config(context, Config), ?INVOICE_PARAMS).

-spec create_invoice_autorization_error_test(config()) -> _.
create_invoice_autorization_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', {_, #payproc_InvoiceParams{party_id = <<"WrongPartyID">>}}) ->
                {throwing, #payproc_InvalidUser{}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoice">>, <<"WrongPartyID">>, ?STRING, Config),
    ?assertMatch(
        {error, {400, #{<<"code">> := <<"invalidPartyID">>}}},
        capi_client_invoices:create_invoice(
            ?config(context, Config),
            ?INVOICE_PARAMS#{<<"partyID">> => <<"WrongPartyID">>}
        )
    ).

-spec get_invoice_by_external_id(config()) -> _.
get_invoice_by_external_id(Config) ->
    ExternalID = <<"merch_id">>,
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{}}),
    InvoiceID = capi_utils:get_unique_id(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(InvoiceID, ExternalID)} end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(InvoiceID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"GetInvoiceByExternalID">>,
        InvoiceID,
        ?STRING,
        ?STRING,
        Config
    ),

    {ok, _} = capi_client_invoices:get_invoice_by_external_id(?config(context, Config), ExternalID).

-spec create_invoice_access_token_ok_test(config()) -> _.
create_invoice_access_token_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"CreateInvoiceAccessToken">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_invoices:create_invoice_access_token(?config(context, Config), ?STRING).

-spec create_invoice_template_ok_test(config()) -> _.
create_invoice_template_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun('Create', {_, #payproc_InvoiceTemplateCreateParams{party_id = ?STRING}}) ->
                {ok, ?INVOICE_TPL}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoiceTemplate">>, ?STRING, ?STRING, Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
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
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req#{
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS
    }).

-spec create_invoice_template_autorization_error_test(config()) -> _.
create_invoice_template_autorization_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun(
                'Create',
                {_, #payproc_InvoiceTemplateCreateParams{party_id = <<"WrongPartyID">>}}
            ) ->
                {throwing, #payproc_InvalidUser{}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"CreateInvoiceTemplate">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    ?assertMatch(
        {error, {400, #{<<"code">> := <<"invalidPartyID">>}}},
        capi_client_invoice_templates:create(
            ?config(context, Config),
            Req#{
                <<"partyID">> => <<"WrongPartyID">>,
                <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS
            }
        )
    ).

-spec create_invoice_with_template_test(config()) -> _.
create_invoice_with_template_test(Config) ->
    ExternalID = <<"external_id">>,
    BenderKey = <<"bender_key">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun
                ('Create', _) -> {ok, ?INVOICE_TPL};
                ('Get', _) -> {ok, ?INVOICE_TPL}
            end},
            {invoicing, fun(
                'CreateWithTemplate',
                {_UserInfo, #payproc_InvoiceWithTemplateParams{id = ID, external_id = EID}}
            ) ->
                {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
        ],
        Config
    ),

    Req = #{
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"externalID">> => ExternalID
    },
    Ctx = ?config(context, Config),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_tpl_op_ctx(
        <<"CreateInvoiceWithTemplate">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),

    {ok, #{<<"invoice">> := Invoice}} =
        capi_client_invoice_templates:create_invoice(Ctx, ?STRING, Req),
    ?assertEqual(BenderKey, maps:get(<<"id">>, Invoice)),
    ?assertEqual(ExternalID, maps:get(<<"externalID">>, Invoice)).

-spec check_no_internal_id_for_external_id_test(config()) -> _.
check_no_internal_id_for_external_id_test(Config) ->
    ExternalID = capi_utils:get_unique_id(),
    _ = capi_ct_helper:mock_services(
        [
            {bender, fun('GetInternalID', _) -> {throwing, capi_ct_helper_bender:no_internal_id()} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetPaymentByExternalID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    %% Ugly test case, but after full integration with bouncer we would expect
    %% {error, {401, #{}}}.
    {error, {_, 500}} = capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec create_customer_ok_test(config()) -> _.
create_customer_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Create', {#payproc_CustomerParams{party_id = ?STRING}}) -> {ok, ?CUSTOMER} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateCustomer">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_customers:create_customer(?config(context, Config), ?CUSTOMER_PARAMS).

-spec create_customer_autorization_error_test(config()) -> _.
create_customer_autorization_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Create', {#payproc_CustomerParams{party_id = <<"WrongPartyID">>}}) ->
                {throwing, #payproc_InvalidUser{}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"CreateCustomer">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {400, #{<<"code">> := <<"invalidPartyID">>}}},
        capi_client_customers:create_customer(
            ?config(context, Config),
            ?CUSTOMER_PARAMS#{
                <<"partyID">> => <<"WrongPartyID">>
            }
        )
    ).

-spec delete_customer_ok_test(config()) -> _.
delete_customer_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun
                ('Get', _) -> {ok, ?CUSTOMER};
                ('Delete', _) -> {ok, ok}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_customer_op_ctx(
        <<"DeleteCustomer">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_customers:delete_customer(?config(context, Config), ?STRING).

-spec create_customer_access_token_ok_test(config()) -> _.
create_customer_access_token_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_customer_op_ctx(
        <<"CreateCustomerAccessToken">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_customers:create_customer_access_token(?config(context, Config), ?STRING).

-spec rescind_invoice_ok_test(config()) -> _.
rescind_invoice_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Rescind', _) -> {ok, ok};
                ('Get', _) -> {ok, ?PAYPROC_INVOICE}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"RescindInvoice">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    ok = capi_client_invoices:rescind_invoice(?config(context, Config), ?STRING, ?STRING).

-spec fulfill_invoice_ok_test(config()) -> _.
fulfill_invoice_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Fulfill', _) -> {ok, ok};
                ('Get', _) -> {ok, ?PAYPROC_INVOICE}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"FulfillInvoice">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    ok = capi_client_invoices:fulfill_invoice(?config(context, Config), ?STRING, ?STRING).

-spec get_merchant_payment_status_test(config()) -> _.
get_merchant_payment_status_test(Config) ->
    {ok, #{
        <<"status">> := <<"failed">>,
        <<"error">> := #{
            <<"code">> := <<"authorization_failed">>,
            <<"subError">> := #{
                <<"code">> := <<"payment_tool_rejected">>,
                <<"subError">> := #{
                    <<"code">> := <<"bank_card_rejected">>,
                    <<"subError">> := #{<<"code">> := <<"cvv_invalid">>}
                }
            }
        }
    }} = get_failed_payment_with_invalid_cvv(Config).

-spec get_failed_payment_with_invalid_cvv(config()) -> _.
get_failed_payment_with_invalid_cvv(Config) ->
    Failure =
        payproc_errors:construct(
            'PaymentFailure',
            {authorization_failed,
                {payment_tool_rejected, {bank_card_rejected, {cvv_invalid, #payprocerr_GeneralFailure{}}}}},
            <<"Reason">>
        ),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE([?PAYPROC_FAILED_PAYMENT({failure, Failure})])} end}
        ],
        Config
    ),
    % mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec create_payment_ok_test(config()) -> _.
create_payment_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE};
                ('StartPayment', {_, _, PaymentParams}) ->
                    #payproc_InvoicePaymentParams{
                        id = ID,
                        external_id = EID,
                        payer = {payment_resource, _},
                        payer_session_info = ?PAYER_SESSION_INFO,
                        context = ?CONTENT
                    } =
                        PaymentParams,
                    {ok, ?PAYPROC_PAYMENT(ID, EID)}
            end},
            {bender, fun('GenerateID', _) ->
                {ok, capi_ct_helper_bender:get_result(BenderKey)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreatePayment">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    PaymentTool = {bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Degus">>)},
    PaymentToolToken = capi_payment_tool:create_encrypted_payment_tool_token(PaymentTool, undefined),
    Req = ?PAYMENT_PARAMS(ExternalID, PaymentToolToken),
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING).

-spec create_refund(config()) -> _.
create_refund(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{<<"reason">> => ?STRING},
    Tid = capi_ct_helper_bender:create_storage(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT])};
                ('RefundPayment', _) ->
                    {ok, ?REFUND}
            end},
            {generator, fun('GenerateID', _) ->
                capi_ct_helper_bender:generate_id(BenderKey)
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING),
    capi_ct_helper_bender:del_storage(Tid).

-spec create_refund_blocked_error(config()) -> _.
create_refund_blocked_error(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{<<"reason">> => ?STRING},
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    Invoice = ?PAYPROC_INVOICE,
                    {ok, Invoice#payproc_Invoice{payments = [?PAYPROC_PAYMENT]}};
                ('RefundPayment', {_, ?STRING, _, _}) ->
                    {throwing, #payproc_InvalidPartyStatus{
                        status = {blocking, {blocked, #domain_Blocked{reason = ?STRING, since = ?TIMESTAMP}}}
                    }}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(BenderKey) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {error, {400, _}} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_refund_expired_error(config()) -> _.
create_refund_expired_error(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{<<"reason">> => ?STRING},
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    Invoice = ?PAYPROC_INVOICE,
                    {ok, Invoice#payproc_Invoice{payments = [?PAYPROC_PAYMENT]}};
                ('RefundPayment', {_, ?STRING, _, _}) ->
                    {throwing, #payproc_InvalidContractStatus{status = {expired, #domain_ContractExpired{}}}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(BenderKey) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {error, {400, _}} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_partial_refund(config()) -> _.
create_partial_refund(Config) ->
    BenderKey = <<"bender_key">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    P = ?PAYPROC_PAYMENT,
                    {ok, ?PAYPROC_INVOICE([P])};
                (
                    'RefundPayment',
                    {
                        _,
                        _,
                        _,
                        #payproc_InvoicePaymentRefundParams{
                            cash = ?CASH,
                            cart = ?THRIFT_INVOICE_CART
                        }
                    }
                ) ->
                    {ok, ?REFUND}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(BenderKey) end}
        ],
        Config
    ),
    Req = #{
        <<"reason">> => ?STRING,
        <<"currency">> => ?RUB,
        <<"amount">> => ?INTEGER,
        <<"cart">> => ?INVOICE_CART
    },
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_partial_refund_without_currency(config()) -> _.
create_partial_refund_without_currency(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{
        <<"reason">> => ?STRING,
        <<"amount">> => ?INTEGER
    },
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('GetPayment', _) ->
                    {ok, ?PAYPROC_PAYMENT};
                ('Get', _) ->
                    P = ?PAYPROC_PAYMENT,
                    {ok, ?PAYPROC_INVOICE([P])};
                ('RefundPayment', _) ->
                    {ok, ?REFUND}
            end},
            {generator, fun('GenerateID', _) ->
                capi_ct_helper_bender:generate_id(BenderKey)
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec get_refund_by_id(config()) -> _.
get_refund_by_id(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT,
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(?STRING, ?STRING)]},
                {ok, ?PAYPROC_INVOICE([Payment])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_refund_op_ctx(
        <<"GetRefundByID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_refund_by_id(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec get_refunds(config()) -> _.
get_refunds(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT,
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(?STRING, ?STRING)]},
                {ok, ?PAYPROC_INVOICE([Payment])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetRefunds">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_refunds(?config(context, Config), ?STRING, ?STRING).

-spec get_refund_by_external_id(config()) -> _.
get_refund_by_external_id(Config) ->
    ExternalID = <<"merch_id">>,
    RefundID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{
        <<"context_data">> => #{
            <<"invoice_id">> => ?STRING,
            <<"payment_id">> => ?STRING,
            <<"refund_id">> => ?STRING
        }
    }),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT,
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(RefundID, ExternalID)]},
                {ok, ?PAYPROC_INVOICE([Payment])}
            end},
            {bender, fun('GetInternalID', _) ->
                InternalKey = RefundID,
                {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_refund_op_ctx(
        <<"GetRefundByExternalID">>,
        ?STRING,
        ?STRING,
        RefundID,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_refund_by_external_id(?config(context, Config), ExternalID).

%

-spec get_chargeback_by_id(config()) -> _.
get_chargeback_by_id(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetChargebacks">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_chargebacks(?config(context, Config), ?STRING, ?STRING).

%

-spec update_invoice_template_ok_test(config()) -> _.
update_invoice_template_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun
                ('Update', _) -> {ok, ?INVOICE_TPL};
                ('Get', _) -> {ok, ?INVOICE_TPL}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_tpl_op_ctx(
        <<"UpdateInvoiceTemplate">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    Req = #{
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
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

-spec delete_invoice_template_ok_test(config()) -> _.
delete_invoice_template_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun
                ('Delete', _) -> {ok, ok};
                ('Get', _) -> {ok, ?INVOICE_TPL}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_tpl_op_ctx(
        <<"DeleteInvoiceTemplate">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    ok = capi_client_invoice_templates:delete(?config(context, Config), ?STRING).

-spec get_account_by_id_ok_test(config()) -> _.
get_account_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{party_management, fun('GetAccountState', _) -> {ok, ?ACCOUNT_STATE} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetAccountByID">>, ?STRING, Config),
    {ok, _} = capi_client_accounts:get_account_by_id(?config(context, Config), ?INTEGER).

-spec get_my_party_ok_test(config()) -> _.
get_my_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetMyParty">>, ?STRING, Config),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)).

-spec suspend_my_party_ok_test(config()) -> _.
suspend_my_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"SuspendMyParty">>, ?STRING, Config),
    ok = capi_client_parties:suspend_my_party(?config(context, Config)).

-spec activate_my_party_ok_test(config()) -> _.
activate_my_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"ActivateMyParty">>, ?STRING, Config),
    ok = capi_client_parties:activate_my_party(?config(context, Config)).

-spec get_party_by_id_ok_test(config()) -> _.
get_party_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetPartyByID">>, ?STRING, Config),
    {ok, _} = capi_client_parties:get_party_by_id(?config(context, Config), ?STRING).

-spec suspend_party_by_id_ok_test(config()) -> _.
suspend_party_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"SuspendPartyByID">>, ?STRING, Config),
    ok = capi_client_parties:suspend_party_by_id(?config(context, Config), ?STRING).

-spec activate_party_by_id_ok_test(config()) -> _.
activate_party_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"ActivatePartyByID">>, ?STRING, Config),
    ok = capi_client_parties:activate_party_by_id(?config(context, Config), ?STRING).

-spec get_shop_by_id_ok_test(config()) -> _.
get_shop_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetShop', _) -> {ok, ?SHOP} end}], Config),
    {ok, _} = capi_client_shops:get_shop_by_id(?config(context, Config), ?STRING).

-spec get_shop_by_id_for_party_ok_test(config()) -> _.
get_shop_by_id_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"GetShopByIDForParty">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shop_by_id_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_shop_by_id_for_party_error_test(config()) -> _.
get_shop_by_id_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', {_, <<"WrongPartyID">>, _}) ->
                {throwing, #payproc_InvalidUser{}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"GetShopByIDForParty">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:get_shop_by_id_for_party(?config(context, Config), <<"WrongPartyID">>, ?STRING)
    ).

-spec get_shops_ok_test(config()) -> _.
get_shops_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetShops">>, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shops(?config(context, Config)).

-spec get_shops_for_party_ok_test(config()) -> _.
get_shops_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('Get', _) -> {ok, ?PARTY} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetShopsForParty">>, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shops_for_party(?config(context, Config), ?STRING).

-spec get_shops_for_party_error_test(config()) -> _.
get_shops_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{party_management, fun('Get', {_, <<"WrongPartyID">>}) -> {throwing, #payproc_InvalidUser{}} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetShopsForParty">>, <<"WrongPartyID">>, Config),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:get_shops_for_party(?config(context, Config), <<"WrongPartyID">>)
    ).

-spec activate_shop_ok_test(config()) -> _.
activate_shop_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('ActivateShop', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"ActivateShop">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:activate_shop(?config(context, Config), ?STRING).

-spec activate_shop_for_party_ok_test(config()) -> _.
activate_shop_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ActivateShop', {_, ?STRING, _}) -> {ok, ok} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"ActivateShopForParty">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:activate_shop_for_party(?config(context, Config), ?STRING, ?STRING).

-spec activate_shop_for_party_error_test(config()) -> _.
activate_shop_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ActivateShop', {_, <<"WrongPartyID">>, _}) ->
                {throwing, #payproc_InvalidUser{}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"ActivateShopForParty">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:activate_shop_for_party(?config(context, Config), <<"WrongPartyID">>, ?STRING)
    ).

-spec suspend_shop_ok_test(config()) -> _.
suspend_shop_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('SuspendShop', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"SuspendShop">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:suspend_shop(?config(context, Config), ?STRING).

-spec suspend_shop_for_party_ok_test(config()) -> _.
suspend_shop_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('SuspendShop', _) -> {ok, ok} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"SuspendShopForParty">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:suspend_shop_for_party(?config(context, Config), ?STRING, ?STRING).

-spec suspend_shop_for_party_error_test(config()) -> _.
suspend_shop_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('SuspendShop', {_, <<"WrongPartyID">>, _}) ->
                {throwing, #payproc_InvalidUser{}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"SuspendShopForParty">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:suspend_shop_for_party(?config(context, Config), <<"WrongPartyID">>, ?STRING)
    ).

-spec get_claim_by_id_ok_test(config()) -> _.
get_claim_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_claim_op_ctx(<<"GetClaimByID">>, ?STRING, ?INTEGER_BINARY, Config),
    {ok, _} = capi_client_claims:get_claim_by_id(?config(context, Config), ?INTEGER).

-spec get_claims_ok_test(config()) -> _.
get_claims_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetClaims', _) ->
                {ok, [
                    ?CLAIM(?CLAIM_CHANGESET),
                    ?CLAIM(?CONTRACTOR_CLAIM_CHANGESET),
                    ?CLAIM(?WALLET_CLAIM_CHANGESET)
                ]}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetClaims">>, ?STRING, Config),
    {ok, [_OnlyOneClaim]} = capi_client_claims:get_claims(?config(context, Config)).

-spec revoke_claim_ok_test(config()) -> _.
revoke_claim_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('RevokeClaim', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_claim_op_ctx(
        <<"RevokeClaimByID">>,
        ?STRING,
        ?INTEGER_BINARY,
        Config
    ),
    ok = capi_client_claims:revoke_claim_by_id(?config(context, Config), ?STRING, ?INTEGER, ?INTEGER).

-spec create_claim_ok_test(config()) -> _.
create_claim_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('CreateClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateClaim">>, ?STRING, Config),
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

-spec update_claim_by_id_test(config()) -> _.
update_claim_by_id_test(_) ->
    % Not realised yet.
    ok.

-spec create_claim_invalid_residence_test(config()) -> _.
create_claim_invalid_residence_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('CreateClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateClaim">>, ?STRING, Config),
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

-spec get_contract_by_id_ok_test(config()) -> _.
get_contract_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #bctx_v1_ContextFragment{
                capi = ?CTX_CAPI(?CTX_CONTRACT_OP(<<"GetContractByID">>, ?STRING, _))
            }
        ),
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?STRING),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?WALLET_CONTRACT_ID).

-spec get_contract_by_id_for_party_ok_test(config()) -> _.
get_contract_by_id_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractByIDForParty">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_by_id_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_contracts_ok_test(config()) -> _.
get_contracts_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetContracts">>, ?STRING, Config),
    {ok, [_First, _Second]} = capi_client_contracts:get_contracts(?config(context, Config)).

-spec get_contracts_for_party_ok_test(config()) -> _.
get_contracts_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetContractsForParty">>, ?STRING, Config),
    {ok, [_First, _Second]} = capi_client_contracts:get_contracts_for_party(?config(context, Config), ?STRING).

-spec get_contract_adjustments_ok_test(config()) -> _.
get_contract_adjustments_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetContract', _) -> {ok, ?CONTRACT};
                ('Get', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustments">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustments(?config(context, Config), ?STRING).

-spec get_contract_adjustments_for_party_ok_test(config()) -> _.
get_contract_adjustments_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustmentsForParty">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustments_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_contract_adjustment_by_id_ok_test(config()) -> _.
get_contract_adjustment_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetContract', _) -> {ok, ?CONTRACT};
                ('Get', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustmentByID">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec get_contract_adjustment_by_id_for_party_ok_test(config()) -> _.
get_contract_adjustment_by_id_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustmentByIDForParty">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustment_by_id_for_party(
        ?config(context, Config),
        ?STRING,
        ?STRING,
        ?STRING
    ).

-spec get_payout_tools_ok_test(config()) -> _.
get_payout_tools_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetPayoutTools">>, ?STRING, Config),
    {ok, _} = capi_client_payouts:get_payout_tools(?config(context, Config), ?STRING).

-spec get_payout_tools_for_party_ok_test(config()) -> _.
get_payout_tools_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetPayoutToolsForParty">>, ?STRING, Config),
    {ok, _} = capi_client_payouts:get_payout_tools_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_payout_tool_by_id(config()) -> _.
get_payout_tool_by_id(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetPayoutToolByID">>, ?STRING, Config),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_RU),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_US),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?WALLET_TOOL).

-spec get_payout_tool_by_id_for_party(config()) -> _.
get_payout_tool_by_id_for_party(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetPayoutToolByIDForParty">>, ?STRING, Config),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id_for_party(
        ?config(context, Config),
        ?STRING,
        ?STRING,
        ?WALLET_TOOL
    ).

-spec create_payout(config()) -> _.
create_payout(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, []),
    _ = capi_ct_helper:mock_services([{payouts, fun('CreatePayout', _) -> {ok, Payout} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreatePayout">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_payouts:create_payout(?config(context, Config), ?PAYOUT_PARAMS, ?STRING).

-spec create_payout_autorization_error(config()) -> _.
create_payout_autorization_error(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, []),
    _ = capi_ct_helper:mock_services([{payouts, fun('CreatePayout', _) -> {ok, Payout} end}], Config),
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_forbidden(), Config),
    %% TODO Enable assert when bouncer is the only authorization authority
    %%    ?assertMatch(
    %%        {error, {401, _}},
    %%        capi_client_payouts:create_payout(
    %%            ?config(context, Config),
    %%            ?PAYOUT_PARAMS#{<<"partyID">> => <<"WrongPartyID">>},
    %%            ?STRING
    %%        )
    %%    ).

    {ok, _} = capi_client_payouts:create_payout(
        ?config(context, Config),
        ?PAYOUT_PARAMS#{<<"partyID">> => <<"WrongPartyID">>},
        ?STRING
    ).

-spec get_payout(config()) -> _.
get_payout(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, [?PAYOUT_PROC_PAYOUT_SUMMARY_ITEM]),
    _ = capi_ct_helper:mock_services([{payouts, fun('Get', _) -> {ok, Payout} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_payout_op_ctx(
        <<"GetPayout">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payouts:get_payout(?config(context, Config), ?STRING).

-spec get_payout_fail(config()) -> _.
get_payout_fail(Config) ->
    PartyID = <<"Wrong party id">>,
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, PartyID, [?PAYOUT_PROC_PAYOUT_SUMMARY_ITEM]),
    _ = capi_ct_helper:mock_services([{payouts, fun('Get', _) -> {ok, Payout} end}], Config),
    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #bctx_v1_ContextFragment{
                capi = ?CTX_CAPI(?CTX_PAYOUT_OP(<<"GetPayout">>, ?STRING, ?STRING)),
                payouts = #bctx_v1_ContextPayouts{
                    payout = undefined
                }
            }
        ),
        Config
    ),
    {error, {404, _}} = capi_client_payouts:get_payout(?config(context, Config), ?STRING).

-spec create_webhook_ok_test(config()) -> _.
create_webhook_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end},
            {webhook_manager, fun('Create', _) -> {ok, ?WEBHOOK} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateWebhook">>, ?STRING, Config),
    Req = #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => #{
            <<"topic">> => <<"InvoicesTopic">>,
            <<"shopID">> => ?STRING,
            <<"eventTypes">> => [
                <<"InvoiceCreated">>,
                <<"InvoicePaid">>,
                <<"InvoiceCancelled">>,
                <<"InvoiceFulfilled">>,
                <<"PaymentStarted">>,
                <<"PaymentProcessed">>,
                <<"PaymentCaptured">>,
                <<"PaymentCancelled">>,
                <<"PaymentRefunded">>,
                <<"PaymentFailed">>,
                <<"PaymentRefundCreated">>,
                <<"PaymentRefundFailed">>,
                <<"PaymentRefundSucceeded">>
            ]
        }
    },
    {ok, _} = capi_client_webhooks:create_webhook(?config(context, Config), Req).

-spec create_webhook_limit_exceeded_test(config()) -> _.
create_webhook_limit_exceeded_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end},
            {webhook_manager, fun('Create', _) -> {throwing, #webhooker_LimitExceeded{}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateWebhook">>, ?STRING, Config),
    Req = #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => #{
            <<"topic">> => <<"InvoicesTopic">>,
            <<"shopID">> => ?STRING,
            <<"eventTypes">> => [
                <<"InvoiceCreated">>,
                <<"InvoicePaid">>,
                <<"InvoiceCancelled">>,
                <<"InvoiceFulfilled">>,
                <<"PaymentStarted">>,
                <<"PaymentProcessed">>,
                <<"PaymentCaptured">>,
                <<"PaymentCancelled">>,
                <<"PaymentRefunded">>,
                <<"PaymentFailed">>,
                <<"PaymentRefundCreated">>,
                <<"PaymentRefundFailed">>,
                <<"PaymentRefundSucceeded">>
            ]
        }
    },
    Body = #{<<"message">> => <<"Webhook limit exceeded">>},
    {error, {429, Body}} = capi_client_webhooks:create_webhook(?config(context, Config), Req).

-spec get_webhooks(config()) -> _.
get_webhooks(Config) ->
    _ = capi_ct_helper:mock_services([{webhook_manager, fun('GetList', _) -> {ok, [?WEBHOOK]} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetWebhooks">>, ?STRING, Config),
    {ok, _} = capi_client_webhooks:get_webhooks(?config(context, Config)).

-spec get_webhook_by_id(config()) -> _.
get_webhook_by_id(Config) ->
    _ = capi_ct_helper:mock_services([{webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_webhook_op_ctx(
        <<"GetWebhookByID">>,
        ?INTEGER_BINARY,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_webhooks:get_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec delete_webhook_by_id(config()) -> _.
delete_webhook_by_id(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {webhook_manager, fun
                ('Get', _) -> {ok, ?WEBHOOK};
                ('Delete', _) -> {ok, ok}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_webhook_op_ctx(
        <<"DeleteWebhookByID">>,
        ?INTEGER_BINARY,
        ?STRING,
        Config
    ),
    ok = capi_client_webhooks:delete_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec get_locations_names_ok_test(config()) -> _.
get_locations_names_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{geo_ip_service, fun('GetLocationName', _) -> {ok, #{123 => ?STRING}} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetLocationsNames">>, Config),
    Query = #{
        <<"geoIDs">> => <<"5,3,6,5,4">>,
        <<"language">> => <<"ru">>
    },
    {ok, _} = capi_client_geo:get_location_names(?config(context, Config), Query).

-spec search_invoices_ok_test(config()) -> _.
search_invoices_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{merchant_stat, fun('GetInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_invoice_op_ctx(
        <<"SearchInvoices">>,
        ?STRING,
        ?STRING,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        <<"testCustomerID">>,
        Config
    ),
    ok = search_invoices_ok_test_(<<"applepay">>, Config),
    ok = search_invoices_ok_test_(<<"yandexpay">>, Config).

search_invoices_ok_test_(BankCardTokenProvider, Config) ->
    Query = [
        {limit, 2},
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
        {customerID, <<"testCustomerID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        {first6, <<"424242">>},
        {last4, <<"2222">>},
        {rrn, <<"090909090909">>},
        {bankCardTokenProvider, BankCardTokenProvider},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>}
    ],
    {ok, _, _} = capi_client_searches:search_invoices(?config(context, Config), ?STRING, Query),
    ok.

-spec search_payments_ok_test(config()) -> _.
search_payments_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_payment_op_ctx(
        <<"SearchPayments">>,
        ?STRING,
        ?STRING,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        Config
    ),
    ok = search_payments_ok_(<<"applepay">>, Config),
    ok = search_payments_ok_(<<"yandexpay">>, Config).

search_payments_ok_(BankCardTokenProvider, Config) ->
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        {first6, <<"424242">>},
        {last4, <<"2222">>},
        {rrn, <<"090909090909">>},
        {approvalCode, <<"808080">>},
        {bankCardTokenProvider, BankCardTokenProvider},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>}
    ],
    {ok, _, _} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query),
    ok.

-spec search_refunds_ok_test(config()) -> _.
search_refunds_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_refund_op_ctx(
        <<"SearchRefunds">>,
        ?STRING,
        <<"testShopID">>,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        <<"testRefundID">>,
        Config
    ),
    ShopID = <<"testShopID">>,
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {shopID, ShopID},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {refundID, <<"testRefundID">>},
        % {rrn, <<"090909090909">>},
        % {approvalCode, <<"808080">>},
        {refundStatus, <<"succeeded">>}
    ],

    {ok, _, _} = capi_client_searches:search_refunds(?config(context, Config), ShopID, Query).

-spec search_payouts_ok_test(config()) -> _.
search_payouts_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{merchant_stat, fun('GetPayouts', _) -> {ok, ?STAT_RESPONSE_PAYOUTS} end}],
        Config
    ),
    ShopID = <<"testShopID">>,
    _ = capi_ct_helper_bouncer:mock_assert_search_payout_op_ctx(
        <<"SearchPayouts">>,
        ?STRING,
        ShopID,
        <<"testPayoutID">>,
        Config
    ),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {payoutID, <<"testPayoutID">>},
        {payoutToolType, <<"Wallet">>}
    ],

    {ok, _, _} = capi_client_searches:search_payouts(?config(context, Config), ShopID, Query).

-spec get_payment_conversion_stats_ok_test(_) -> _.
get_payment_conversion_stats_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
        ],
        Config
    ),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_conversion_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_revenue_stats_ok_test(config()) -> _.
get_payment_revenue_stats_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
        ],
        Config
    ),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 36}}},
        {to_time, {{2020, 08, 11}, {19, 42, 36}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_revenue_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_geo_stats_ok_test(config()) -> _.
get_payment_geo_stats_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
        ],
        Config
    ),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11}, {19, 42, 37}}},
        {to_time, {{2020, 08, 11}, {19, 42, 37}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_geo_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_rate_stats_ok_test(config()) -> _.
get_payment_rate_stats_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
        ],
        Config
    ),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11}, {19, 42, 38}}},
        {to_time, {{2020, 08, 11}, {19, 42, 38}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _} = capi_client_analytics:get_payment_rate_stats(?config(context, Config), ?STRING, Query).

-spec get_payment_method_stats_ok_test(config()) -> _.
get_payment_method_stats_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
        ],
        Config
    ),
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

-spec get_reports_ok_test(config()) -> _.
get_reports_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{reporting, fun('GetReports', _) -> {ok, ?FOUND_REPORTS} end}], Config),
    {ok, _} = capi_client_reports:get_reports(?config(context, Config), ?STRING, ?TIMESTAMP, ?TIMESTAMP),
    {ok, _} = capi_client_reports:get_reports_for_party(
        ?config(context, Config),
        ?STRING,
        ?STRING,
        ?TIMESTAMP,
        ?TIMESTAMP
    ).

-spec get_report_ok_test(config()) -> _.
get_report_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT} end}], Config),
    {ok, _} = capi_client_reports:get_report(?config(context, Config), ?STRING, ?INTEGER_BINARY),
    {ok, _} = capi_client_reports:get_report_for_party(?config(context, Config), ?STRING, ?STRING, ?INTEGER_BINARY).

-spec get_report_not_found_test(config()) -> _.
get_report_not_found_test(Config) ->
    _ = capi_ct_helper:mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT} end}], Config),
    {error, {404, #{<<"message">> := <<"Report not found">>}}} =
        capi_client_reports:get_report(?config(context, Config), <<"WRONG_STRING">>, ?INTEGER_BINARY).

-spec create_report_ok_test(config()) -> _.
create_report_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {reporting, fun
                ('CreateReport', _) -> {ok, ?INTEGER};
                ('GetReport', {?INTEGER}) -> {ok, ?REPORT}
            end}
        ],
        Config
    ),
    {ok, _} = capi_client_reports:create_report(
        ?config(context, Config),
        ?STRING,
        ?REPORT_TYPE,
        ?TIMESTAMP,
        ?TIMESTAMP
    ),
    {ok, _} = capi_client_reports:create_report_for_party(
        ?config(context, Config),
        ?STRING,
        ?STRING,
        ?REPORT_TYPE,
        ?TIMESTAMP,
        ?TIMESTAMP
    ).

-spec download_report_file_ok_test(_) -> _.
download_report_file_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {reporting, fun
                ('GetReport', _) -> {ok, ?REPORT};
                ('GeneratePresignedUrl', _) -> {ok, ?STRING}
            end}
        ],
        Config
    ),
    {ok, _} = capi_client_reports:download_file(?config(context, Config), ?STRING, ?INTEGER_BINARY, ?STRING),
    {ok, _} = capi_client_reports:download_file_for_party(
        ?config(context, Config),
        ?STRING,
        ?STRING,
        ?INTEGER_BINARY,
        ?STRING
    ).

-spec download_report_file_not_found_test(_) -> _.
download_report_file_not_found_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {reporting, fun
                ('GetReport', _) -> {ok, ?REPORT};
                ('GeneratePresignedUrl', _) -> {ok, ?STRING}
            end}
        ],
        Config
    ),
    {error, {404, #{<<"message">> := <<"Report not found">>}}} =
        capi_client_reports:download_file(?config(context, Config), <<"WRONG_STRING">>, ?INTEGER_BINARY, ?STRING).

-spec get_categories_ok_test(config()) -> _.
get_categories_ok_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCategories">>, Config),
    {ok, _} = capi_client_categories:get_categories(?config(context, Config)).

-spec get_category_by_ref_ok_test(config()) -> _.
get_category_by_ref_ok_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCategoryByRef">>, Config),
    {ok, _} = capi_client_categories:get_category_by_ref(?config(context, Config), ?INTEGER).

-spec get_schedule_by_ref_ok_test(config()) -> _.
get_schedule_by_ref_ok_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    {ok, _} = capi_client_payouts:get_schedule_by_ref(?config(context, Config), ?INTEGER).

-spec check_no_payment_by_external_id_test(config()) -> _.
check_no_payment_by_external_id_test(Config) ->
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => <<"123">>}}),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE}
            end},
            {bender, fun('GetInternalID', _) ->
                InternalKey = capi_utils:get_unique_id(),
                {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)}
            end}
        ],
        Config
    ),

    {error,
        {404, #{
            <<"message">> := <<"Payment not found">>
        }}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec check_no_invoice_by_external_id_test(config()) -> _.
check_no_invoice_by_external_id_test(Config) ->
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{}),
    _ = capi_ct_helper:mock_services(
        [
            {bender, fun('GetInternalID', _) ->
                InternalKey = capi_utils:get_unique_id(),
                {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)}
            end}
        ],
        Config
    ),

    {error, {invalid_response_code, 500}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec retrieve_payment_by_external_id_test(config()) -> _.
retrieve_payment_by_external_id_test(Config) ->
    PaymentID = capi_utils:get_unique_id(),
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => <<"123">>}}),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT(PaymentID, ExternalID)])}
            end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(PaymentID, BenderContext)}
            end}
        ],
        Config
    ),
    {ok, #{
        <<"externalID">> := ExternalID
    }} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec get_payment_institutions(config()) -> _.
get_payment_institutions(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutions">>, Config),
    {ok, [_Something]} = capi_client_payment_institutions:get_payment_institutions(?config(context, Config)),
    {ok, []} =
        capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"live">>),
    {ok, [#{<<"realm">> := <<"test">>}]} =
        capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"test">>).

-spec get_payment_institution_by_ref(config()) -> _.
get_payment_institution_by_ref(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutionByRef">>, Config),
    {ok, _} = capi_client_payment_institutions:get_payment_institution_by_ref(?config(context, Config), ?INTEGER).

-spec get_payment_institution_payment_terms(config()) -> _.
get_payment_institution_payment_terms(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ComputePaymentInstitutionTerms', _) -> {ok, ?TERM_SET} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutionPaymentTerms">>, Config),
    {ok, _} =
        capi_client_payment_institutions:get_payment_institution_payment_terms(?config(context, Config), ?INTEGER).

-spec get_payment_institution_payout_terms(config()) -> _.
get_payment_institution_payout_terms(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ComputePaymentInstitutionTerms', _) -> {ok, ?TERM_SET} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutionPayoutMethods">>, Config),
    {ok, _} = capi_client_payment_institutions:get_payment_institution_payout_methods(
        ?config(context, Config),
        ?INTEGER,
        <<"RUB">>
    ).

-spec get_payment_institution_payout_schedules(config()) -> _.
get_payment_institution_payout_schedules(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ComputePaymentInstitutionTerms', _) -> {ok, ?TERM_SET} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutionPayoutSchedules">>, Config),

    {ok, _} = capi_client_payment_institutions:get_payment_institution_payout_schedules(
        ?config(context, Config),
        ?INTEGER,
        <<"USD">>,
        <<"BankAccount">>
    ).

-spec check_support_decrypt_v2_test(config()) -> _.
check_support_decrypt_v2_test(_Config) ->
    PaymentToolToken = <<
        "v2.eyJhbGciOiJFQ0RILUVTIiwiZW5jIjoiQTEyOEdDTSIsImVwayI6eyJhbGciOiJFQ0RILUVTIiwiY3J2IjoiUC0yNTYiLCJrdHkiOi"
        "JFQyIsInVzZSI6ImVuYyIsIngiOiJRanFmNFVrOTJGNzd3WXlEUjNqY3NwR2dpYnJfdVRmSXpMUVplNzVQb1R3IiwieSI6InA5cjJGV3F"
        "mU2xBTFJXYWhUSk8xY3VneVZJUXVvdzRwMGdHNzFKMFJkUVEifSwia2lkIjoia3hkRDBvclZQR29BeFdycUFNVGVRMFU1TVJvSzQ3dVp4"
        "V2lTSmRnbzB0MCJ9..j3zEyCqyfQjpEtQM.JAc3kqJm6zbn0fMZGlK_t14Yt4PvgOuoVL2DtkEgIXIqrxxWFbykKBGxQvwYisJYIUJJwt"
        "YbwvuGEODcK2uTC2quPD2Ejew66DLJF2xcAwE.MNVimzi8r-5uTATNalgoBQ"
    >>,
    {ok, {PaymentTool, ValidUntil}} = capi_payment_tool:decrypt_payment_tool_token(PaymentToolToken),
    ?assertEqual(
        {mobile_commerce, #domain_MobileCommerce{
            phone = #domain_MobilePhone{
                cc = <<"7">>,
                ctn = <<"9210001122">>
            },
            operator_deprecated = megafone
        }},
        PaymentTool
    ),
    ?assertEqual(<<"2020-10-29T23:44:15.499Z">>, capi_utils:deadline_to_binary(ValidUntil)).
