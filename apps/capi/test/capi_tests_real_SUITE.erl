-module(capi_tests_real_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("dmsl/include/dmsl_webhooker_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").
-include_lib("capi_dummy_data.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
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
    get_category_by_ref_ok_test/1
]).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    _.
all() ->
    [
        create_invoice_ok_test,
        get_invoice_ok_test,
        get_invoice_events_ok_test,
        get_invoice_payment_methods_ok_test,
        create_invoice_access_token_ok_test,
        rescind_invoice_ok_test,
        fulfill_invoice_ok_test,

        create_invoice_with_tpl_ok_test,
        create_invoice_template_ok_test,
        get_invoice_template_ok_test,
        update_invoice_template_ok_test,
        delete_invoice_template_ok_test,
        get_invoice_payment_methods_by_tpl_id_ok_test,

        get_account_by_id_ok_test,

        create_payment_ok_test,
        get_payments_ok_test,
        get_payment_by_id_ok_test,
        create_refund,
        get_refund_by_id,
        get_refunds,
        cancel_payment_ok_test,
        capture_payment_ok_test,

        create_payment_tool_token_ok_test,

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
        get_category_by_ref_ok_test
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    MainApps =
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(cowlib) ++
        capi_ct_helper:start_app(woody) ++
        start_capi(Config),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    ServiceApps = start_dummy_services(SupPid),
    {ok, Token} = get_token(),
    Context = get_context(Token, 10, 60000),
    [
        {context, Context},
        {apps, lists:reverse(MainApps ++ ServiceApps)},
        {test_sup, SupPid}
    | Config].

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    exit(?config(test_sup, C), shutdown),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, _C) ->
    meck:unload(capi_dummy_service),
    ok.

%%% Tests

-spec create_invoice_ok_test(config()) ->
    _.
create_invoice_ok_test(Config) ->
    mock_handle_function({ok, ?PAYPROC_INVOICE}),
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
    mock_handle_function({ok, ?PAYPROC_INVOICE}),
    Req = #{
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>}
    },
    {ok, _} = capi_client_invoice_templates:create_invoice(?config(context, Config), ?STRING, Req).

-spec get_invoice_ok_test(config()) ->
    _.
get_invoice_ok_test(Config) ->
    mock_handle_function({ok, ?PAYPROC_INVOICE}),
    {ok, _} = capi_client_invoices:get_invoice_by_id(?config(context, Config), ?STRING).

-spec get_invoice_events_ok_test(config()) ->
    _.
get_invoice_events_ok_test(Config) ->
    mock_handle_function({ok, [?EVENT]}),
    {ok, _} = capi_client_invoices:get_invoice_events(?config(context, Config), ?STRING, ?INTEGER).

-spec get_invoice_payment_methods_ok_test(config()) ->
    _.
get_invoice_payment_methods_ok_test(Config) ->
    mock_handle_function({ok, ?TERM_SET}),
    {ok, _} = capi_client_invoices:get_invoice_payment_methods(?config(context, Config), ?STRING).

-spec create_invoice_access_token_ok_test(config()) ->
    _.
create_invoice_access_token_ok_test(Config) ->
    mock_handle_function({ok, ?PAYPROC_INVOICE}),
    {ok, _} = capi_client_invoices:create_invoice_access_token(?config(context, Config), ?STRING).

-spec rescind_invoice_ok_test(config()) ->
    _.
rescind_invoice_ok_test(Config) ->
    mock_handle_function(),
    ok = capi_client_invoices:rescind_invoice(?config(context, Config), ?STRING, ?STRING).

-spec fulfill_invoice_ok_test(config()) ->
    _.
fulfill_invoice_ok_test(Config) ->
    mock_handle_function(),
    ok = capi_client_invoices:fulfill_invoice(?config(context, Config), ?STRING, ?STRING).

-spec create_invoice_template_ok_test(config()) ->
    _.
create_invoice_template_ok_test(Config) ->
    mock_handle_function({ok, ?INVOICE_TPL}),
    Req = #{
        <<"shopID">> => ?STRING,
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
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req).

-spec get_invoice_template_ok_test(config()) ->
    _.
get_invoice_template_ok_test(Config) ->
    mock_handle_function({ok, ?INVOICE_TPL}),
    {ok, _} = capi_client_invoice_templates:get_template_by_id(?config(context, Config), ?STRING).

-spec update_invoice_template_ok_test(config()) ->
    _.
update_invoice_template_ok_test(Config) ->
    mock_handle_function({ok, ?INVOICE_TPL}),
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
    mock_handle_function(),
    ok = capi_client_invoice_templates:delete(?config(context, Config), ?STRING).

-spec get_invoice_payment_methods_by_tpl_id_ok_test(config()) ->
    _.
get_invoice_payment_methods_by_tpl_id_ok_test(Config) ->
    mock_handle_function({ok, ?TERM_SET}),
    {ok, _} = capi_client_invoice_templates:get_invoice_payment_methods(?config(context, Config), ?STRING).

-spec get_account_by_id_ok_test(config()) ->
    _.
get_account_by_id_ok_test(Config) ->
    mock_handle_function({ok, ?ACCOUNT_STATE}),
    {ok, _} = capi_client_accounts:get_account_by_id(?config(context, Config), ?INTEGER).

-spec create_payment_ok_test(config()) ->
    _.
create_payment_ok_test(Config) ->
    mock_handle_function({ok, ?PAYPROC_PAYMENT}),
    Token = capi_utils:map_to_base64url(#{
        <<"type">> => <<"bank_card">>,
        <<"token">> => <<"Token">>,
        <<"payment_system">> => <<"visa">>,
        <<"bin">> => <<"Bin">>,
        <<"masked_pan">> => <<"Test1234">>
    }),
    Session = capi_utils:map_to_base64url(#{
        <<"clientInfo">> => #{
            <<"fingerprint">> => <<"Fingerprint">>,
            <<"ip_address">> => <<"127.0.0.1">>
        },
        <<"paymentSession">> => <<"PaymentSession">>
    }),
    Req = #{
        <<"paymentSession">> => Session,
        <<"paymentToolToken">> => Token,
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"contactInfo">> => #{
            <<"email">> => <<"bla@bla.ru">>
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING).

-spec get_payments_ok_test(config()) ->
    _.
get_payments_ok_test(Config) ->
    mock_handle_function({ok, ?PAYPROC_INVOICE}),
    {ok, _} = capi_client_payments:get_payments(?config(context, Config), ?STRING).

-spec get_payment_by_id_ok_test(config()) ->
    _.
get_payment_by_id_ok_test(Config) ->
    mock_handle_function({ok, ?PAYPROC_PAYMENT}),
    {ok, _} = capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec create_refund(config()) ->
    _.
create_refund(Config) ->
    mock_handle_function({ok, ?REFUND}),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), ?STRING, ?STRING, <<>>).

-spec get_refund_by_id(config()) ->
    _.
get_refund_by_id(Config) ->
    mock_handle_function({ok, ?REFUND}),
    {ok, _} = capi_client_payments:get_refund_by_id(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec get_refunds(config()) ->
    _.
get_refunds(Config) ->
    mock_handle_function({ok, ?PAYPROC_PAYMENT}),
    {ok, _} = capi_client_payments:get_refunds(?config(context, Config), ?STRING, ?STRING).

-spec cancel_payment_ok_test(config()) ->
    _.
cancel_payment_ok_test(Config) ->
    mock_handle_function(),
    ok = capi_client_payments:cancel_payment(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec capture_payment_ok_test(config()) ->
    _.
capture_payment_ok_test(Config) ->
    mock_handle_function(),
    ok = capi_client_payments:capture_payment(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec create_payment_tool_token_ok_test(_) ->
    _.
create_payment_tool_token_ok_test(Config) ->
    PutCardDataResult = #'PutCardDataResult'{
        bank_card = ?BANK_CARD,
        session_id = ?STRING
    },
    mock_handle_function({ok, PutCardDataResult}),
    Req = #{
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
    {ok, _, _} = capi_client_tokens:create_payment_tool_token(?config(context, Config), Req).

-spec get_my_party_ok_test(config()) ->
    _.
get_my_party_ok_test(Config) ->
    mock_handle_function({ok, ?PARTY}),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)).

-spec suspend_my_party_ok_test(config()) ->
    _.
suspend_my_party_ok_test(Config) ->
    mock_handle_function(),
    capi_client_parties:suspend_my_party(?config(context, Config)).

-spec activate_my_party_ok_test(config()) ->
    _.
activate_my_party_ok_test(Config) ->
    mock_handle_function(),
    capi_client_parties:activate_my_party(?config(context, Config)).

-spec get_shop_by_id_ok_test(config()) ->
    _.
get_shop_by_id_ok_test(Config) ->
    mock_handle_function({ok, ?SHOP}),
    capi_client_shops:get_shop_by_id(?config(context, Config), ?STRING).

-spec get_shops_ok_test(config()) ->
    _.
get_shops_ok_test(Config) ->
    mock_handle_function({ok, ?PARTY}),
    {ok, _} = capi_client_shops:get_shops(?config(context, Config)).

-spec suspend_shop_ok_test(config()) ->
    _.
suspend_shop_ok_test(Config) ->
    mock_handle_function(),
    capi_client_shops:suspend_shop(?config(context, Config), ?STRING).

-spec activate_shop_ok_test(config()) ->
    _.
activate_shop_ok_test(Config) ->
    mock_handle_function(),
    capi_client_shops:activate_shop(?config(context, Config), ?STRING).

-spec get_claim_by_id_ok_test(config()) ->
    _.
get_claim_by_id_ok_test(Config) ->
    mock_handle_function({ok, ?CLAIM}),
    {ok, _} = capi_client_claims:get_claim_by_id(?config(context, Config), ?INTEGER).

-spec get_claims_ok_test(config()) ->
    _.
get_claims_ok_test(Config) ->
    mock_handle_function({ok, [?CLAIM]}),
    {ok, _} = capi_client_claims:get_claims(?config(context, Config)).

-spec revoke_claim_ok_test(config()) ->
    _.
revoke_claim_ok_test(Config) ->
    mock_handle_function(),
    ok = capi_client_claims:revoke_claim_by_id(?config(context, Config), ?STRING, ?INTEGER, ?INTEGER).

-spec create_claim_ok_test(config()) ->
    _.
create_claim_ok_test(Config) ->
    mock_handle_function({ok, ?CLAIM}),
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
                <<"type">> => <<"PayoutToolBankAccount">>,
                <<"bankAccount">> => #{
                    <<"account">> => <<"12345678901234567890">>,
                    <<"bankName">> => <<"testBankName">>,
                    <<"bankPostAccount">> => <<"12345678901234567890">>,
                    <<"bankBik">> => <<"123456789">>
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
    mock_handle_function({ok, ?CONTRACT}),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?STRING).

-spec get_contracts_ok_test(config()) ->
    _.
get_contracts_ok_test(Config) ->
    mock_handle_function({ok, ?PARTY}),
    {ok, _} = capi_client_contracts:get_contracts(?config(context, Config)).

-spec get_contract_adjustments_ok_test(config()) ->
    _.
get_contract_adjustments_ok_test(_) ->
    % Access denied
    % mock_handle_function({ok, ?CONTRACT}),
    % {ok, _} = capi_client_contracts:get_contract_adjustments(?config(context, Config), ?STRING).
    ok.

-spec get_contract_adjustment_by_id_ok_test(config()) ->
    _.
get_contract_adjustment_by_id_ok_test(_) ->
    % Access denied
    % mock_handle_function({ok, ?CONTRACT}),
    % {ok, _} = capi_client_contracts:get_contract_adjustment_by_id(?config(context, Config), ?STRING, ?STRING).
    ok.

-spec get_payout_tools_ok_test(config()) ->
    _.
get_payout_tools_ok_test(Config) ->
    mock_handle_function({ok, ?CONTRACT}),
    {ok, _} = capi_client_payouts:get_payout_tools(?config(context, Config), ?STRING).

-spec get_payout_tool_by_id(config()) ->
    _.
get_payout_tool_by_id(Config) ->
    mock_handle_function({ok, ?CONTRACT}),
    {ok, _} = capi_client_payouts:get_payout_tool_by_id(?config(context, Config), ?STRING, ?STRING).

-spec create_webhook_ok_test(config()) ->
    _.
create_webhook_ok_test(Config) ->
    Fun = fun(X, _, _, _) ->
        case X of
            'GetShop' ->
                {ok, ?SHOP};
            'Create' ->
                {ok, ?WEBHOOK}
        end
    end,
    mock_handle_function(func, Fun),
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
    mock_handle_function({ok, [?WEBHOOK]}),
    {ok, _} = capi_client_webhooks:get_webhooks(?config(context, Config)).

-spec get_webhook_by_id(config()) ->
    _.
get_webhook_by_id(Config) ->
    mock_handle_function({ok, ?WEBHOOK}),
    {ok, _} = capi_client_webhooks:get_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec delete_webhook_by_id(config()) ->
    _.
delete_webhook_by_id(Config) ->
    Fun = fun(X, _, _, _) ->
        case X of
            'Get' ->
                {ok, ?WEBHOOK};
            'Delete' ->
                {ok, ok}
        end
    end,
    mock_handle_function(func, Fun),
    ok = capi_client_webhooks:delete_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec get_locations_names_ok_test(config()) ->
    _.
get_locations_names_ok_test(Config) ->
    mock_handle_function({ok, #{123 => ?STRING}}),
    {TestGeoID, _} = {53654, <<"Могадишо"/utf8>>},
    PreparedGeo = genlib_string:join($,,[genlib:to_binary(I) || I <- [TestGeoID]]),
    Query = #{
        <<"geoIDs">> => PreparedGeo,
        <<"language">> => <<"ru">>
    },
    {ok, _} = capi_client_geo:get_location_names(?config(context, Config), Query).

-spec search_invoices_ok_test(config()) ->
    _.
search_invoices_ok_test(Config) ->
    mock_handle_function({ok, ?STAT_RESPONSE_INVOICES}),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {invoiceStatus, <<"fulfilled">>},
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
        %%{cardNumberMask, <<"2222">>},  %%@FIXME cannot be used until getting the newest api client
        {paymentAmount, 10000}
    ],

    {ok, _, _} = capi_client_searches:search_invoices(?config(context, Config), ?STRING, Query).

-spec search_payments_ok_test(config()) ->
    _.
search_payments_ok_test(Config) ->
    mock_handle_function({ok, ?STAT_RESPONSE_PAYMENTS}),
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
search_payouts_ok_test(_) ->
    ok.

-spec get_payment_conversion_stats_ok_test(_) ->
    _.
get_payment_conversion_stats_ok_test(Config) ->
    mock_handle_function({ok, ?STAT_RESPONSE_RECORDS}),
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
    mock_handle_function({ok, ?STAT_RESPONSE_RECORDS}),
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
    mock_handle_function({ok, ?STAT_RESPONSE_RECORDS}),
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
    mock_handle_function({ok, ?STAT_RESPONSE_RECORDS}),
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
    mock_handle_function({ok, ?STAT_RESPONSE_RECORDS}),
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
    mock_handle_function({ok, [?REPORT]}),
    {ok, _} = capi_client_reports:get_reports(?config(context, Config), ?STRING, ?TIMESTAMP, ?TIMESTAMP).

-spec download_report_file_ok_test(_) ->
    _.
download_report_file_ok_test(Config) ->
        Fun = fun(X, _, _, _) ->
        case X of
            'GetReport' ->
                {ok, ?REPORT};
            'GeneratePresignedUrl' ->
                {ok, ?STRING}
        end
    end,
    mock_handle_function(func, Fun),
    {ok, _} = capi_client_reports:download_file(?config(context, Config), ?STRING, ?INTEGER, ?STRING).

-spec get_categories_ok_test(config()) ->
    _.
get_categories_ok_test(Config) ->
    mock_handle_function({ok, ?SNAPSHOT}),
    {ok, _} = capi_client_categories:get_categories(?config(context, Config)).


-spec get_category_by_ref_ok_test(config()) ->
    _.
get_category_by_ref_ok_test(Config) ->
    mock_handle_function({ok, ?SNAPSHOT}),
    {ok, _} = capi_client_categories:get_category_by_ref(?config(context, Config), ?INTEGER).

%%

get_token() ->
    ACL = [
        {[invoices, payments], read},
        {[invoices, payments], write},
        {[party], read},
        {[party], write},
        {[invoices], read},
        {[invoices], write},
        {[payment_tool_tokens], write}
    ],
    PartyID = ?STRING,
    Claims = #{?STRING => ?STRING},
    capi_authorizer_jwt:issue({{PartyID, capi_acl:from_list(ACL)}, Claims}, unlimited).

start_capi(Config) ->
    CapiEnv = [
        {ip, "::"},
        {port, 8080},
        {service_type, real},
        {authorizers, #{
            jwt => #{
                signee => capi,
                keyset => #{
                    capi     => get_keysource("keys/local/private.pem", Config)
                }
            }
        }}
    ],
    capi_ct_helper:start_app(capi, CapiEnv).

start_dummy_services(SupPid) ->
    ServiceURLs = lists:foldl(
        fun(Spec, Acc) ->
            Handler = start_service_handler(capi_dummy_service, Spec, SupPid),
            maps:merge(Handler, Acc)
        end,
        #{},
        service_specs()),
    capi_ct_helper:start_app(capi_woody_client, [{service_urls, ServiceURLs}]).

start_service_handler(Module, {Name, {ThriftModule, Service}}, SupPid) ->
    Port = get_random_port(),
    Path = "/dummy",
    {ok, IP} = inet:parse_address("0.0.0.0"),
    ChildSpec = woody_server:child_spec(
        {Name, Module},
        #{
            ip => IP,
            port => Port,
            event_handler => capi_woody_event_handler,
            handlers => [{Path, {{ThriftModule, Service}, {Module, #{}}}}]
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    #{Name => iolist_to_binary(["http://capi:", integer_to_list(Port), Path])}.

get_random_port() ->
    rand:uniform(32768) + 32767.

get_context(Token, Retries, Timeout) ->
    capi_client_lib:get_context("localhost:8080", Token, Retries, Timeout, ipv4).

service_specs() ->
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
        {reporting, {dmsl_reporting_thrift, 'Reporting'}}
    ].

get_keysource(Fn, Config) ->
    {pem_file, filename:join(?config(data_dir, Config), Fn)}.

mock_handle_function() ->
    mock_handle_function({ok, ok}).

mock_handle_function(Response) ->
    meck:new(capi_dummy_service, [passthrough]),
    meck:expect(capi_dummy_service, handle_function, 4, Response).

mock_handle_function(func, Function) ->
    meck:new(capi_dummy_service),
    meck:expect(capi_dummy_service, handle_function, Function).

get_lifetime() ->
    get_lifetime(0, 0, 7).

get_lifetime(YY, MM, DD) ->
    #{
       <<"years">>  => YY,
       <<"months">> => MM,
       <<"days">>   => DD
    }.