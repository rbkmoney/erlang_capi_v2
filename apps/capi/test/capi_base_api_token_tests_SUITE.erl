-module(capi_base_api_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

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
    create_invoice_ok_test/1,
    create_invoice_idemp_ok_test/1,
    create_invoice_idemp_fail_test/1,
    create_invoice_access_token_ok_test/1,
    create_invoice_template_ok_test/1,
    create_invoice_with_template_test/1,
    create_customer_ok_test/1,
    create_customer_access_token_ok_test/1,
    delete_customer_ok_test/1,
    rescind_invoice_ok_test/1,
    fulfill_invoice_ok_test/1,
    get_merchant_payment_status_test/1,
    create_refund/1,
    create_refund_error/1,
    create_partial_refund/1,
    create_partial_refund_without_currency/1,
    get_refund_by_id/1,
    get_refunds/1,
    update_invoice_template_ok_test/1,
    delete_invoice_template_ok_test/1,
    get_account_by_id_ok_test/1,
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
    check_no_payment_by_external_id_test/1,
    check_no_internal_id_for_external_id_test/1,
    retrieve_payment_by_external_id_test/1,
    check_no_invoice_by_external_id_test/1
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
        {group, operations_by_base_api_token}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_base_api_token, [],
            [
                create_invoice_ok_test,
                create_invoice_idemp_ok_test,
                create_invoice_idemp_fail_test,
                create_invoice_access_token_ok_test,
                create_invoice_template_ok_test,
                create_invoice_with_template_test,
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
                delete_customer_ok_test,
                check_no_payment_by_external_id_test,
                check_no_internal_id_for_external_id_test,
                retrieve_payment_by_external_id_test,
                check_no_invoice_by_external_id_test
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
init_per_group(operations_by_base_api_token, Config) ->
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
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    {ok, Token2} = capi_ct_helper:issue_token(<<"TEST2">>, BasePermissions, unlimited, #{}),
    Config2 = [{context_with_diff_party, capi_ct_helper:get_context(Token2)} | Config],
    [{context, capi_ct_helper:get_context(Token)} | Config2];

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
-spec create_invoice_ok_test(config()) ->
    _.
create_invoice_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', _)     -> {ok, ?PAYPROC_INVOICE} end},
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"key">>)} end}
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

-spec create_invoice_idemp_ok_test(config()) ->
    _.
create_invoice_idemp_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', [_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}]) ->
            {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
        end},
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
    ], Config),
    Req = #{
        <<"shopID">>      => ?STRING,
        <<"amount">>      => ?INTEGER,
        <<"currency">>    => ?RUB,
        <<"metadata">>    => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">>     => ?TIMESTAMP,
        <<"product">>     => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>,
        <<"externalID">>  => ExternalID
    },
    {ok, #{<<"invoice">> := Invoice}}  = capi_client_invoices:create_invoice(?config(context, Config), Req),
    {ok, #{<<"invoice">> := Invoice2}} = capi_client_invoices:create_invoice(?config(context, Config), Req),
    ?assertEqual(BenderKey,  maps:get(<<"id">>, Invoice)),
    ?assertEqual(ExternalID, maps:get(<<"externalID">>, Invoice)),
    ?assertEqual(Invoice, Invoice2).

-spec create_invoice_idemp_fail_test(config()) ->
    _.
create_invoice_idemp_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req = #{
        <<"shopID">>      => ?STRING,
        <<"amount">>      => ?INTEGER,
        <<"currency">>    => ?RUB,
        <<"metadata">>    => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">>     => ?TIMESTAMP,
        <<"product">>     => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>,
        <<"externalID">>  => ExternalID
    },
    Ctx = capi_msgp_marshalling:marshal(#{<<"params_hash">> => erlang:phash2(Req)}),
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', [_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}]) ->
            {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey, Ctx)} end}
    ], Config),

    {ok, #{<<"invoice">> := Invoice}} = capi_client_invoices:create_invoice(?config(context, Config), Req),
    InvoiceID = maps:get(<<"id">>, Invoice),
    BadExternalID = {error, {409, #{
        <<"externalID">> => ExternalID,
        <<"id">>         => InvoiceID,
        <<"message">>    => <<"This 'externalID' has been used by another request">>
    }}},
    Response = capi_client_invoices:create_invoice(
        ?config(context, Config),
        Req#{<<"product">> => <<"test_product2">>}
    ),
    ?assertEqual(BadExternalID, Response).

-spec create_invoice_access_token_ok_test(config()) ->
    _.
create_invoice_access_token_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    {ok, _} = capi_client_invoices:create_invoice_access_token(?config(context, Config), ?STRING).

-spec create_invoice_template_ok_test(config()) ->
    _.
create_invoice_template_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoice_templating, fun('Create', _) -> {ok, ?INVOICE_TPL} end}], Config),
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

-spec create_invoice_with_template_test(config()) ->
    _.
create_invoice_with_template_test(Config) ->
    ExternalID = <<"external_id">>,
    BenderKey  = <<"bender_key">>,
    capi_ct_helper:mock_services([
        {invoice_templating, fun('Create', _) -> {ok, ?INVOICE_TPL} end},
        {invoicing, fun(
            'CreateWithTemplate',
            [_UserInfo, #payproc_InvoiceWithTemplateParams{id = ID, external_id = EID}]
        ) ->
            {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
    ], Config),
    ReqTemp = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    Details = #{
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
    {ok, Template} = capi_client_invoice_templates:create(?config(context, Config), ReqTemp#{<<"details">> => Details}),
    #{<<"invoiceTemplate">> := #{<<"id">> := TemplateID}} = Template,
    Req = #{
        <<"amount">>      => ?INTEGER,
        <<"currency">>    => ?RUB,
        <<"metadata">>    => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"externalID">>  => ExternalID
    },
    {ok, #{<<"invoice">> := Invoice}} =
        capi_client_invoice_templates:create_invoice(?config(context, Config), TemplateID, Req),
    ?assertEqual(BenderKey,  maps:get(<<"id">>, Invoice)),
    ?assertEqual(ExternalID, maps:get(<<"externalID">>, Invoice)).

-spec create_customer_ok_test(config()) ->
    _.
create_customer_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('Create', _) -> {ok, ?CUSTOMER} end}], Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"contactInfo">> => #{<<"email">> => <<"bla@bla.ru">>},
        <<"metadata">> => #{<<"text">> => [<<"SOMESHIT">>, 42]}
    },
    {ok, _} = capi_client_customers:create_customer(?config(context, Config), Req).

-spec create_customer_access_token_ok_test(config()) ->
    _.
create_customer_access_token_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    {ok, _} = capi_client_customers:create_customer_access_token(?config(context, Config), ?STRING).

-spec delete_customer_ok_test(config()) ->
    _.
delete_customer_ok_test(Config) ->
    capi_ct_helper:mock_services([{customer_management, fun('Delete', _) -> {ok, ok} end}], Config),
    {ok, _} = capi_client_customers:delete_customer(?config(context, Config), ?STRING).

-spec rescind_invoice_ok_test(config()) ->
    _.
rescind_invoice_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('Rescind', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoices:rescind_invoice(?config(context, Config), ?STRING, ?STRING).

-spec fulfill_invoice_ok_test(config()) ->
    _.
fulfill_invoice_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('Fulfill', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoices:fulfill_invoice(?config(context, Config), ?STRING, ?STRING).

-spec get_merchant_payment_status_test(config()) ->
    _.
get_merchant_payment_status_test(Config) ->
    {ok, #{
        <<"status">> := <<"failed">>,
        <<"error" >> :=
            #{<<"code">> := <<"authorization_failed">>, <<"subError">> :=
                #{<<"code">> := <<"payment_tool_rejected">>, <<"subError">> :=
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
    capi_ct_helper:mock_services(
        [{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_FAILED_PAYMENT({failure, Failure})} end}],
        Config
    ),
    % mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).


-spec create_refund(config()) ->
    _.
create_refund(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('RefundPayment', _) -> {ok, ?REFUND} end}], Config),
    Req = #{<<"reason">> => ?STRING},
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_refund_error(config()) ->
    _.
create_refund_error(Config) ->
    capi_ct_helper:mock_services([
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
    capi_ct_helper:mock_services([{invoicing, fun('RefundPayment', _) -> {ok, ?REFUND} end}], Config),
    Req = #{
        <<"reason">> => ?STRING,
        <<"currency">> => ?RUB,
        <<"amount">> => ?INTEGER
    },
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_partial_refund_without_currency(config()) ->
    _.
create_partial_refund_without_currency(Config) ->
    capi_ct_helper:mock_services([
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
    capi_ct_helper:mock_services([{invoicing, fun('GetPaymentRefund', _) -> {ok, ?REFUND} end}], Config),
    {ok, _} = capi_client_payments:get_refund_by_id(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec get_refunds(config()) ->
    _.
get_refunds(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    {ok, _} = capi_client_payments:get_refunds(?config(context, Config), ?STRING, ?STRING).

-spec update_invoice_template_ok_test(config()) ->
    _.
update_invoice_template_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoice_templating, fun('Update', _) -> {ok, ?INVOICE_TPL} end}], Config),
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

-spec delete_invoice_template_ok_test(config()) ->
    _.
delete_invoice_template_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoice_templating, fun('Delete', _) -> {ok, ok} end}], Config),
    ok = capi_client_invoice_templates:delete(?config(context, Config), ?STRING).

-spec get_account_by_id_ok_test(config()) ->
    _.
get_account_by_id_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('GetAccountState', _) -> {ok, ?ACCOUNT_STATE} end}], Config),
    {ok, _} = capi_client_accounts:get_account_by_id(?config(context, Config), ?INTEGER).

-spec get_my_party_ok_test(config()) ->
    _.
get_my_party_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)).

-spec suspend_my_party_ok_test(config()) ->
    _.
suspend_my_party_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    ok = capi_client_parties:suspend_my_party(?config(context, Config)).

-spec activate_my_party_ok_test(config()) ->
    _.
activate_my_party_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    ok = capi_client_parties:activate_my_party(?config(context, Config)).

-spec get_shop_by_id_ok_test(config()) ->
    _.
get_shop_by_id_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('GetShop', _) -> {ok, ?SHOP} end}], Config),
    {ok, _} = capi_client_shops:get_shop_by_id(?config(context, Config), ?STRING).

-spec get_shops_ok_test(config()) ->
    _.
get_shops_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_shops:get_shops(?config(context, Config)).

-spec activate_shop_ok_test(config()) ->
    _.
activate_shop_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('ActivateShop', _) -> {ok, ok} end}], Config),
    ok = capi_client_shops:activate_shop(?config(context, Config), ?STRING).

-spec suspend_shop_ok_test(config()) ->
    _.
suspend_shop_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('SuspendShop', _) -> {ok, ok} end}], Config),
    ok = capi_client_shops:suspend_shop(?config(context, Config), ?STRING).

-spec get_claim_by_id_ok_test(config()) ->
    _.
get_claim_by_id_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {party_management, fun('GetClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}
    ], Config),
    {ok, _} = capi_client_claims:get_claim_by_id(?config(context, Config), ?INTEGER).

-spec get_claims_ok_test(config()) ->
    _.
get_claims_ok_test(Config) ->
    capi_ct_helper:mock_services([
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
    capi_ct_helper:mock_services([{party_management, fun('RevokeClaim', _) -> {ok, ok} end}], Config),
    ok = capi_client_claims:revoke_claim_by_id(?config(context, Config), ?STRING, ?INTEGER, ?INTEGER).

-spec create_claim_ok_test(config()) ->
    _.
create_claim_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {party_management, fun('CreateClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}
    ], Config),
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

-spec update_claim_by_id_test(config()) ->
    _.
update_claim_by_id_test(_) ->
    % Not realised yet.
    ok.

-spec create_claim_invalid_residence_test(config()) ->
    _.
create_claim_invalid_residence_test(Config) ->
    capi_ct_helper:mock_services([
        {party_management, fun('CreateClaim', _) -> {ok, ?CLAIM(?CLAIM_CHANGESET)} end}
    ], Config),
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

-spec get_contract_by_id_ok_test(config()) ->
    _.
get_contract_by_id_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?STRING),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?WALLET_CONTRACT_ID).

-spec get_contracts_ok_test(config()) ->
    _.
get_contracts_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, [_First, _Second]} = capi_client_contracts:get_contracts(?config(context, Config)).

-spec get_contract_adjustments_ok_test(config()) ->
    _.
get_contract_adjustments_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_adjustments(?config(context, Config), ?STRING).

-spec get_contract_adjustment_by_id_ok_test(config()) ->
    _.
get_contract_adjustment_by_id_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_contracts:get_contract_adjustment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec get_payout_tools_ok_test(config()) ->
    _.
get_payout_tools_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_payouts:get_payout_tools(?config(context, Config), ?STRING).

-spec get_payout_tool_by_id(config()) ->
    _.
get_payout_tool_by_id(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_RU),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?BANKID_US),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?WALLET_TOOL).

-spec create_payout(config()) ->
    _.
create_payout(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, []),
    capi_ct_helper:mock_services([{payouts, fun('CreatePayout', _) -> {ok, Payout} end}], Config),
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
    capi_ct_helper:mock_services([{payouts, fun('Get', _) -> {ok, Payout} end}], Config),
    {ok, _} = capi_client_payouts:get_payout(?config(context, Config), ?STRING).

-spec get_payout_fail(config()) ->
    _.
get_payout_fail(Config) ->
    Payout = ?PAYOUT(?WALLET_PAYOUT_TYPE, [?PAYOUT_PROC_PAYOUT_SUMMARY_ITEM]),
    capi_ct_helper:mock_services([{payouts, fun('Get', _) -> {ok, Payout} end}], Config),
    {error, {404, _}} = capi_client_payouts:get_payout(?config(context_with_diff_party, Config), ?STRING).

-spec create_webhook_ok_test(config()) ->
    _.
create_webhook_ok_test(Config) ->
    capi_ct_helper:mock_services(
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
    capi_ct_helper:mock_services([{webhook_manager, fun('GetList', _) -> {ok, [?WEBHOOK]} end}], Config),
    {ok, _} = capi_client_webhooks:get_webhooks(?config(context, Config)).

-spec get_webhook_by_id(config()) ->
    _.
get_webhook_by_id(Config) ->
    capi_ct_helper:mock_services([{webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK} end}], Config),
    {ok, _} = capi_client_webhooks:get_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec delete_webhook_by_id(config()) ->
    _.
delete_webhook_by_id(Config) ->
    capi_ct_helper:mock_services([
        {webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK}; ('Delete', _) -> {ok, ok} end}
    ], Config),
    ok = capi_client_webhooks:delete_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec get_locations_names_ok_test(config()) ->
    _.
get_locations_names_ok_test(Config) ->
    capi_ct_helper:mock_services([{geo_ip_service, fun('GetLocationName', _) -> {ok, #{123 => ?STRING}} end}], Config),
    Query = #{
        <<"geoIDs">> => <<"5,3,6,5,4">>,
        <<"language">> => <<"ru">>
    },
    {ok, _} = capi_client_geo:get_location_names(?config(context, Config), Query).

-spec search_invoices_ok_test(config()) ->
    _.
search_invoices_ok_test(Config) ->
    capi_ct_helper:mock_services([{merchant_stat, fun('GetInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}], Config),
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
    capi_ct_helper:mock_services([{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}], Config),
    Query = [
        {limit, 2},
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
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>}
    ],

    {ok, _, _} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_refunds_ok_test(config()) ->
    _.
search_refunds_ok_test(Config) ->
    capi_ct_helper:mock_services([{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
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
    capi_ct_helper:mock_services([{merchant_stat, fun('GetPayouts', _) -> {ok, ?STAT_RESPONSE_PAYOUTS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {shopID, <<"testShopID">>},
        {payoutID, <<"testPayoutID">>},
        {payoutToolType, <<"Wallet">>}
    ],

    {ok, _, _} = capi_client_searches:search_payouts(?config(context, Config), ?STRING, Query).

-spec get_payment_conversion_stats_ok_test(_) ->
    _.
get_payment_conversion_stats_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
    ], Config),
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
    capi_ct_helper:mock_services([
        {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
    ], Config),
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
    capi_ct_helper:mock_services([
        {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
    ], Config),
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
    capi_ct_helper:mock_services([
        {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
    ], Config),
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
    capi_ct_helper:mock_services([
        {merchant_stat, fun('GetStatistics', _) -> {ok, ?STAT_RESPONSE_RECORDS} end}
    ], Config),
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
    capi_ct_helper:mock_services([{reporting, fun('GetReports', _) -> {ok, [?REPORT]} end}], Config),
    {ok, _} = capi_client_reports:get_reports(?config(context, Config), ?STRING, ?TIMESTAMP, ?TIMESTAMP).

-spec get_report_ok_test(config()) ->
    _.
get_report_ok_test(Config) ->
    capi_ct_helper:mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT} end}], Config),
    {ok, _} = capi_client_reports:get_report(?config(context, Config), ?STRING, ?INTEGER).

-spec create_report_ok_test(config()) ->
    _.
create_report_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {reporting, fun
            ('GenerateReport', _)           -> {ok, ?INTEGER};
            ('GetReport', [_, _, ?INTEGER]) -> {ok, ?REPORT}
        end}
    ], Config),
    {ok, _} = capi_client_reports:create_report(
        ?config(context, Config),
        ?STRING,
        ?REPORT_TYPE,
        ?TIMESTAMP,
        ?TIMESTAMP
    ).

-spec download_report_file_ok_test(_) ->
    _.
download_report_file_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {reporting, fun('GetReport', _) -> {ok, ?REPORT}; ('GeneratePresignedUrl', _) -> {ok, ?STRING} end}
    ], Config),
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

-spec check_no_payment_by_external_id_test(config()) ->
    _.
check_no_payment_by_external_id_test(Config) ->
    ExternalID = capi_ct_helper:unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => <<"123">>}}),
    capi_ct_helper:mock_services([
        {invoicing, fun('GetPayment', _)  -> throw(#payproc_InvoicePaymentNotFound{}) end},
        {bender,  fun('GetInternalID', _) ->
            InternalKey = capi_ct_helper:unique_id(),
            {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)} end}
    ], Config),

    {error, {404, #{
        <<"message">> := <<"Payment not found">>
    }}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec check_no_invoice_by_external_id_test(config()) ->
    _.
check_no_invoice_by_external_id_test(Config) ->
    ExternalID = capi_ct_helper:unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{}),
    capi_ct_helper:mock_services([
        {bender,  fun('GetInternalID', _) ->
            InternalKey = capi_ct_helper:unique_id(),
            {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)} end}
    ], Config),

    {error, {404, #{
        <<"message">> := <<"Invoice not found">>
    }}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec check_no_internal_id_for_external_id_test(config()) ->
    _.
check_no_internal_id_for_external_id_test(Config) ->
    ExternalID = capi_ct_helper:unique_id(),
    capi_ct_helper:mock_services([
        {bender,  fun('GetInternalID', _) -> throw(capi_bender:no_internal_id()) end}
    ], Config),

    {error, {404, #{
        <<"message">> := <<"Payment not found">>
    }}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec retrieve_payment_by_external_id_test(config()) ->
    _.
retrieve_payment_by_external_id_test(Config) ->
    PaymentID = capi_ct_helper:unique_id(),
    ExternalID = capi_ct_helper:unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => <<"123">>}}),
    capi_ct_helper:mock_services([
        {invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT(PaymentID, ExternalID)} end},
        {bender,  fun('GetInternalID', _) ->
            InternalKey = capi_ct_helper:unique_id(),
            {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)} end}
    ], Config),
    {ok, #{
        <<"externalID">> := ExternalID
    }} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

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
    capi_ct_helper:mock_services(
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
    capi_ct_helper:mock_services(
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
