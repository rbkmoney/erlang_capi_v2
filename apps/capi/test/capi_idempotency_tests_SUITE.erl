-module(capi_idempotency_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("capi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([create_payment_ok_test/1]).
-export([create_payment_fail_test/1]).
-export([different_payment_tools_test/1]).
-export([second_request_with_idempotent_feature_test/1]).
-export([second_request_without_idempotent_feature_test/1]).
-export([create_invoice_ok_test/1]).
-export([create_invoice_legacy_fail_test/1]).
-export([create_invoice_fail_test/1]).
-export([create_invoice_idemp_cart_ok_test/1]).
-export([create_invoice_idemp_cart_fail_test/1]).
-export([create_invoice_idemp_bank_account_fail_test/1]).
-export([create_invoice_template_ok_test/1]).
-export([create_invoice_template_fail_test/1]).
-export([create_invoice_with_template_ok_test/1]).
-export([create_invoice_with_template_fail_test/1]).
-export([create_refund_idemp_ok_test/1]).
-export([create_refund_idemp_fail_test/1]).
-export([create_customer_ok_test/1]).
-export([create_customer_fail_test/1]).
-export([create_customer_binding_ok_test/1]).
-export([create_customer_binding_fail_test/1]).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-define(DIFFERENCE, -1).

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, payment_creation},
        {group, invoice_creation},
        {group, invoice_template_creation},
        {group, invoice_with_template_creation},
        {group, refund_creation},
        {group, invoice_template_creation},
        {group, customer_creation},
        {group, customer_binding_creation}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {payment_creation, [], [
            create_payment_ok_test,
            create_payment_fail_test,
            different_payment_tools_test,
            second_request_with_idempotent_feature_test,
            second_request_without_idempotent_feature_test
        ]},
        {invoice_creation, [], [
            create_invoice_ok_test,
            create_invoice_legacy_fail_test,
            create_invoice_fail_test,
            create_invoice_idemp_cart_fail_test,
            create_invoice_idemp_cart_ok_test,
            create_invoice_idemp_bank_account_fail_test
        ]},
        {invoice_template_creation, [], [
            create_invoice_template_ok_test,
            create_invoice_template_fail_test
        ]},
        {invoice_with_template_creation, [], [
            create_invoice_with_template_ok_test,
            create_invoice_with_template_fail_test
        ]},
        {refund_creation, [], [
            create_refund_idemp_ok_test,
            create_refund_idemp_fail_test
        ]},
        {customer_creation, [], [
            create_customer_ok_test,
            create_customer_fail_test
        ]},
        {customer_binding_creation, [], [
            create_customer_binding_ok_test,
            create_customer_binding_fail_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    ExtraEnv = [{idempotence_event_handler, {capi_ct_features_reader_event_handler, #{}}}],
    capi_ct_helper:init_suite(?MODULE, Config, ExtraEnv).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    _ = application:unset_env(capi, idempotence_event_handler),
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(payment_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    ExtraProperties = #{<<"ip_replacement_allowed">> => true},
    {ok, Token} = capi_ct_helper:issue_token([{[invoices], write}], unlimited, ExtraProperties),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        MockServiceSup
    ),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">> => ?TIMESTAMP,
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    {{ok, #{
            <<"invoiceAccessToken">> := #{<<"payload">> := InvAccToken}
        }},
        _} =
        with_feature_storage(fun() ->
            capi_client_invoices:create_invoice(capi_ct_helper:get_context(Token, ExtraProperties), Req)
        end),

    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(InvAccToken)}, {group_apps, Apps1} | Config];
init_per_group(GroupName, Config) when
    GroupName =:= invoice_creation orelse
        GroupName =:= refund_creation orelse
        GroupName =:= customer_binding_creation orelse
        GroupName =:= invoice_template_creation orelse
        GroupName =:= customer_creation orelse
        GroupName =:= invoice_with_template_creation
->
    BasePermissions = base_permissions(),
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    {ok, Token2} = capi_ct_helper:issue_token(<<"TEST2">>, BasePermissions, unlimited, #{}),
    Config2 = [{context_with_diff_party, capi_ct_helper:get_context(Token2)} | Config],
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{context, capi_ct_helper:get_context(Token)}, {group_apps, Apps1}, {group_test_sup, SupPid} | Config2];
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

base_permissions() ->
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

%% TESTS

-spec create_payment_ok_test(config()) -> _.
create_payment_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ContactInfo = #{},
    Jwe1 = get_encrypted_token(visa, ?EXP_DATE(2, 2020)),
    Jwe2 = get_encrypted_token(visa, ?EXP_DATE(2, 2020)),
    Req1 = payment_params(ExternalID, Jwe1, ContactInfo, undefined),
    Req2 = payment_params(ExternalID, Jwe2, ContactInfo, false),
    [
        {{ok, Response1}, Unused},
        {{ok, Response2}, _}
    ] = create_payment(BenderKey, [Req1, Req2], Config),
    ?assertEqual(
        [
            [<<"externalID">>],
            [<<"metadata">>, <<"bla">>],
            [<<"payer">>, <<"paymentSession">>],
            [<<"payer">>, <<"paymentTool">>, <<"bin">>],
            [<<"payer">>, <<"paymentTool">>, <<"cardholder_name">>],
            [<<"payer">>, <<"paymentTool">>, <<"masked_pan">>],
            [<<"payer">>, <<"paymentTool">>, <<"payment_system">>],
            [<<"processingDeadline">>]
        ],
        Unused
    ),
    ?assertEqual(Response1, Response2).

-spec create_payment_fail_test(config()) -> _.
create_payment_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Jwe1 = get_encrypted_token(visa, ?EXP_DATE(1, 2020)),
    Jwe2 = get_encrypted_token(visa, ?EXP_DATE(2, 2020)),
    Req1 = payment_params(ExternalID, Jwe1, #{}, undefined),
    Req2 = payment_params(ExternalID, Jwe2, #{}, false),
    [
        {{ok, _}, _},
        {Response, _}
    ] = create_payment(BenderKey, [Req1, Req2], Config),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response).

-spec different_payment_tools_test(config()) -> _.
different_payment_tools_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ContactInfo = #{},
    Jwe1 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Mr. Surname">>)}),
    Jwe2 = encrypt_payment_tool({digital_wallet, ?DIGITAL_WALLET(<<"+79876543210">>, <<"token id">>)}),
    Req1 = payment_params(ExternalID, Jwe1, ContactInfo, undefined),
    Req2 = payment_params(ExternalID, Jwe2, ContactInfo, false),
    [
        {{ok, _}, _},
        {Response2, Unused}
    ] = create_payment(BenderKey, [Req1, Req2], Config),
    ?assertEqual(
        [
            [<<"externalID">>],
            [<<"metadata">>, <<"bla">>],
            [<<"payer">>, <<"paymentSession">>],
            [<<"processingDeadline">>]
        ],
        Unused
    ),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2).

-spec second_request_without_idempotent_feature_test(config()) -> _.
second_request_without_idempotent_feature_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ContactInfo = #{},
    Jwe1 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Mr. Surname">>)}),
    Jwe2 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, undefined, <<"Mr. Surname">>)}),
    Req1 = payment_params(ExternalID, Jwe1, ContactInfo, undefined),
    Req2 = payment_params(ExternalID, Jwe2, ContactInfo, undefined),
    [
        {{ok, _}, _},
        {Response2, _}
    ] = create_payment(BenderKey, [Req1, Req2], Config),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2).

-spec second_request_with_idempotent_feature_test(config()) -> _.
second_request_with_idempotent_feature_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Jwe1 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), undefined)}),
    Jwe2 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Mr. Surname">>)}),
    Req1 = payment_params(ExternalID, Jwe1, #{}, undefined),
    Req2 = payment_params(ExternalID, Jwe2, #{}, undefined),
    [
        {{ok, Response1}, _},
        {{ok, Response2}, _}
    ] = create_payment(BenderKey, [Req1, Req2], Config),
    ?assertEqual(Response1, Response2).

-spec create_invoice_ok_test(config()) -> _.
create_invoice_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', {_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}}) ->
                {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
        ],
        Config
    ),
    Req = invoice_params(ExternalID),
    Unused = [
        [<<"description">>],
        [<<"externalID">>],
        [<<"metadata">>, <<"invoice_dummy_metadata">>]
    ],
    {{ok, #{<<"invoice">> := Invoice1}}, Unused1} = create_invoice_(Req, Config),
    {{ok, #{<<"invoice">> := Invoice2}}, Unused2} = create_invoice_(Req, Config),

    ?assertEqual(Unused, Unused2),
    ?assertEqual(Unused, Unused1),
    ?assertEqual(BenderKey, maps:get(<<"id">>, Invoice1)),
    ?assertEqual(ExternalID, maps:get(<<"externalID">>, Invoice1)),
    ?assertEqual(Invoice1, Invoice2).

-spec create_invoice_legacy_fail_test(config()) -> _.
create_invoice_legacy_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req = invoice_params(ExternalID),
    Unused = [
        [<<"description">>],
        [<<"externalID">>],
        [<<"metadata">>, <<"invoice_dummy_metadata">>]
    ],
    Req2 = Req#{<<"product">> => <<"test_product2">>},
    Ctx = capi_msgp_marshalling:marshal(#{<<"version">> => 1, <<"params_hash">> => erlang:phash2(Req)}),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', {_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}}) ->
                {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey, Ctx)} end}
        ],
        Config
    ),
    {{ok, Invoice1}, _} = create_invoice_(Req, Config),
    #{<<"invoice">> := #{<<"id">> := InvoiceID}} = Invoice1,
    {Response, Unused2} = create_invoice_(Req2, Config),
    ?assertEqual(Unused, Unused2),
    ?assertEqual(response_error(409, ExternalID, InvoiceID), Response).

-spec create_invoice_fail_test(config()) -> _.
create_invoice_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req1 = invoice_params(ExternalID),
    Req2 = Req1#{<<"product">> => <<"test_product2">>},
    [
        {{ok, #{<<"invoice">> := #{<<"id">> := InvoiceID}}}, _},
        {Response, _}
    ] = create_invoices(BenderKey, [Req1, Req2], Config),
    ?assertEqual(response_error(409, ExternalID, InvoiceID), Response).

-spec create_invoice_idemp_cart_ok_test(config()) -> _.
create_invoice_idemp_cart_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Params = invoice_params(ExternalID),
    Unused = [
        [<<"description">>],
        [<<"externalID">>],
        [<<"metadata">>, <<"invoice_dummy_metadata">>]
    ],
    Req1 = Params#{
        <<"amount">> => 10000,
        <<"cart">> => [
            #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 9000},
            #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 1000}
        ]
    },
    Req2 = Req1#{
        <<"cart">> => [
            #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 1000},
            #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 9000}
        ]
    },
    [
        {{ok, #{<<"invoice">> := Invoice1}}, UnusedParams1},
        {{ok, #{<<"invoice">> := Invoice2}}, UnusedParams2}
    ] = create_invoices(BenderKey, [Req1, Req2], Config),
    ?assertEqual(Invoice1, Invoice2),
    ?assertEqual(Unused, UnusedParams1),
    ?assertEqual(Unused, UnusedParams2).

-spec create_invoice_idemp_cart_fail_test(config()) -> _.
create_invoice_idemp_cart_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req = invoice_params(ExternalID),
    Unused = [
        [<<"description">>],
        [<<"externalID">>],
        [<<"metadata">>, <<"invoice_dummy_metadata">>]
    ],
    Req1 = Req#{
        <<"cart">> => [
            #{
                <<"product">> => <<"product#1">>,
                <<"quantity">> => 1,
                <<"price">> => ?INTEGER,
                <<"taxMode">> => #{<<"type">> => <<"InvoiceLineTaxVAT">>, <<"rate">> => <<"18%">>}
            }
        ]
    },
    Req2 = Req#{
        <<"cart">> => [
            #{
                <<"product">> => <<"product#1">>,
                <<"quantity">> => 2,
                <<"price">> => ?INTEGER
            }
        ]
    },
    Req3 = Req#{
        <<"cart">> => [
            #{
                <<"product">> => <<"product#1">>,
                <<"quantity">> => 1,
                <<"price">> => ?INTEGER
            }
        ]
    },
    [
        {{ok, _}, UnusedParams},
        {Response2, _},
        {Response3, _}
    ] = create_invoices(BenderKey, [Req1, Req2, Req3], Config),
    ?assertEqual(Unused, UnusedParams),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response3).

-spec create_invoice_idemp_bank_account_fail_test(config()) -> _.
create_invoice_idemp_bank_account_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req = invoice_params(ExternalID),
    Account1 = #{
        <<"accountType">> => <<"InvoiceRussianBankAccount">>,
        <<"account">> => <<"12345678901234567890">>,
        <<"bankBik">> => <<"123456789">>
    },
    Account2 = Account1#{<<"bankBik">> => <<"987654321">>},
    Req1 = Req#{<<"bankAccount">> => Account1},
    Req2 = Req#{<<"bankAccount">> => Account2},
    [
        {{ok, _}, _},
        {Response, _}
    ] = create_invoices(BenderKey, [Req1, Req2], Config),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response).

-spec create_invoice_template_ok_test(config()) -> _.
create_invoice_template_ok_test(Config) ->
    BenderKey = <<"create_invoice_template_ok_test_bender_key">>,
    Req1 = #{
        <<"externalID">> => genlib:unique(),
        <<"shopID">> => <<"1">>,
        <<"lifetime">> => #{
            <<"days">> => ?INTEGER,
            <<"months">> => ?INTEGER,
            <<"years">> => ?INTEGER
        },
        <<"partyID">> => <<"2">>,
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS,
        <<"description">> => <<"Sample text">>,
        <<"metadata">> => #{
            <<"key">> => <<"value">>
        }
    },
    Req2 = Req1#{<<"description">> => <<"whatever">>},

    Result = create_invoice_templates(BenderKey, [Req1, Req2], Config),

    [
        {{ok, #{<<"invoiceTemplate">> := Template1}}, UnusedParams1},
        {{ok, #{<<"invoiceTemplate">> := Template2}}, UnusedParams2}
    ] = Result,

    ?assertEqual(Template1, Template2),
    ?assertEqual(UnusedParams1, UnusedParams2),
    ?assertEqual(
        [
            [<<"description">>],
            [<<"externalID">>],
            [<<"metadata">>, <<"key">>],
            [<<"partyID">>]
        ],
        UnusedParams1
    ).

-spec create_invoice_template_fail_test(config()) -> _.
create_invoice_template_fail_test(Config) ->
    BenderKey = <<"create_invoice_template_fail_test_bender_key">>,
    ExternalID = genlib:unique(),
    Req1 = #{
        <<"externalID">> => ExternalID,
        <<"shopID">> => <<"1">>,
        <<"lifetime">> => #{
            <<"days">> => ?INTEGER,
            <<"months">> => ?INTEGER,
            <<"years">> => ?INTEGER
        },
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS,
        <<"description">> => <<"Sample text">>
    },
    Req2 = Req1#{<<"shopID">> => <<"2">>},

    [CreateResult1, CreateResult2] = create_invoice_templates(BenderKey, [Req1, Req2], Config),
    ?assertMatch({{ok, _}, _}, CreateResult1),
    {ActualCreateResult2, _UnusedParams} = CreateResult2,
    ?assertEqual(
        response_error(409, ExternalID, BenderKey),
        ActualCreateResult2
    ).

-spec create_invoice_with_template_ok_test(config()) -> _.
create_invoice_with_template_ok_test(Config) ->
    BenderKey = <<"create_invoice_with_template_ok_test">>,
    InvoiceTemplateID = ?STRING,
    Req1 = #{
        <<"externalID">> => genlib:unique(),
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?USD,
        <<"metadata">> => #{<<"key">> => <<"value">>}
    },
    Req2 = Req1#{<<"metadata">> => #{<<"key">> => <<"another_value">>}},

    Result = create_invoices_with_templates(
        BenderKey,
        [
            {InvoiceTemplateID, Req1},
            {InvoiceTemplateID, Req2}
        ],
        Config
    ),

    [
        {{ok, #{<<"invoice">> := Invoice1}}, UnusedParams1},
        {{ok, #{<<"invoice">> := Invoice2}}, UnusedParams2}
    ] = Result,

    ?assertEqual(Invoice1, Invoice2),
    ?assertEqual(UnusedParams1, UnusedParams2),
    ?assertEqual([[<<"externalID">>], [<<"metadata">>, <<"key">>]], UnusedParams1).

-spec create_invoice_with_template_fail_test(config()) -> _.
create_invoice_with_template_fail_test(Config) ->
    BenderKey = <<"create_invoice_with_template_fail_test">>,
    InvoiceTemplateID = ?STRING,
    ExternalID = genlib:unique(),
    Req1 = #{
        <<"externalID">> => ExternalID,
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?USD,
        <<"metadata">> => #{<<"key">> => <<"value">>}
    },
    Req2 = Req1#{<<"amount">> => ?INTEGER + 1},

    [CreateResult1, CreateResult2] = create_invoices_with_templates(
        BenderKey,
        [{InvoiceTemplateID, Req1}, {InvoiceTemplateID, Req2}],
        Config
    ),
    ?assertMatch({{ok, _}, _}, CreateResult1),
    {ActualCreateResult2, _UnusedParams} = CreateResult2,
    ?assertEqual(
        response_error(409, ExternalID, BenderKey),
        ActualCreateResult2
    ).

-spec create_refund_idemp_ok_test(config()) -> _.
create_refund_idemp_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req1 = #{
        <<"reason">> => ?STRING,
        <<"externalID">> => ExternalID,
        <<"id">> => ?STRING
    },
    Req2 = Req1#{
        <<"amount">> => 10000,
        <<"currency">> => <<"RUB">>
    },
    [Refund1, Refund2] = create_refunds(BenderKey, [Req1, Req2], Config),
    ?assertEqual(Refund1, Refund2).

-spec create_refund_idemp_fail_test(config()) -> _.
create_refund_idemp_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req1 = #{
        <<"reason">> => ?STRING,
        <<"externalID">> => ExternalID,
        <<"currency">> => <<"RUB">>,
        <<"cart">> => [#{<<"product">> => <<"dog">>, <<"quantity">> => 1, <<"price">> => 500}]
    },
    Req2 = Req1#{
        <<"cart">> => [
            #{<<"product">> => <<"dog">>, <<"quantity">> => 1, <<"price">> => 500},
            #{<<"product">> => <<"cat">>, <<"quantity">> => 1, <<"price">> => 500}
        ]
    },
    [{{ok, _Refund1}, Unused1}, {Response2, Unused2}] = create_refunds(BenderKey, [Req1, Req2], Config),
    Unused = [
        [<<"externalID">>],
        [<<"invoiceID">>],
        [<<"paymentID">>],
        [<<"reason">>]
    ],
    ?assertEqual(Unused, Unused1),
    ?assertEqual(Unused, Unused2),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2).

-spec create_customer_ok_test(config()) -> _.
create_customer_ok_test(Config) ->
    BenderKey = <<"create_customer_ok_test">>,
    Req1 = ?CUSTOMER_PARAMS#{<<"externalID">> => genlib:unique()},
    Req2 = Req1#{<<"externalID">> => genlib:unique()},

    Result = create_customers(BenderKey, [Req1, Req2], Config),

    [{{ok, #{<<"customer">> := Customer1}}, UnusedFeatures1}, {{ok, #{<<"customer">> := Customer2}}, UnusedFeatures2}] =
        Result,

    ?assertEqual(Customer1, Customer2),
    ?assertEqual(UnusedFeatures1, UnusedFeatures2),
    ?assertEqual(UnusedFeatures1, [[<<"externalID">>], [<<"metadata">>, <<"text">>]]).

-spec create_customer_fail_test(config()) -> _.
create_customer_fail_test(Config) ->
    BenderKey = <<"create_customer_fail_test">>,
    ExternalID = genlib:unique(),
    Req1 = ?CUSTOMER_PARAMS#{<<"externalID">> => ExternalID, <<"shopID">> => <<"1">>},
    Req2 = Req1#{<<"shopID">> => <<"2">>},

    [CustomerResult1, CustomerResult2] = create_customers(BenderKey, [Req1, Req2], Config),
    ?assertMatch({{ok, _}, _}, CustomerResult1),
    ?assertEqual(
        {response_error(409, ExternalID, BenderKey), [[<<"externalID">>], [<<"metadata">>, <<"text">>]]},
        CustomerResult2
    ).

-spec create_customer_binding_ok_test(config()) -> _.
create_customer_binding_ok_test(Config) ->
    BenderKey = <<"customer_binding_bender_key">>,
    Req1 = #{
        <<"externalID">> => genlib:unique(),
        <<"paymentResource">> => #{
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => ?TEST_PAYMENT_TOKEN
        }
    },
    Req2 = Req1#{<<"externalID">> => genlib:unique()},

    [BindingResult1, BindingResult2] = create_customer_bindings(BenderKey, [Req1, Req2], Config),
    ?assertMatch(
        {{ok, _}, [
            [<<"externalID">>],
            [<<"paymentResource">>, <<"paymentTool">>, <<"bin">>],
            [<<"paymentResource">>, <<"paymentTool">>, <<"masked_pan">>],
            [<<"paymentResource">>, <<"paymentTool">>, <<"payment_system">>]
        ]},
        BindingResult1
    ),
    ?assertEqual(BindingResult1, BindingResult2).

-spec create_customer_binding_fail_test(config()) -> _.
create_customer_binding_fail_test(Config) ->
    BenderKey = <<"customer_binding_bender_key">>,
    ExternalID = genlib:unique(),
    Req1 = #{
        <<"externalID">> => ExternalID,
        <<"paymentResource">> => #{
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => ?TEST_PAYMENT_TOKEN(visa, <<"TOKEN1">>)
        }
    },
    Req2 = Req1#{
        <<"paymentResource">> => #{
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => ?TEST_PAYMENT_TOKEN(mastercard, <<"TOKEN2">>)
        }
    },

    [BindingResult1, BindingResult2] = create_customer_bindings(BenderKey, [Req1, Req2], Config),
    ?assertMatch({{ok, _}, _}, BindingResult1),
    {ActualBindingResult2, _UnusedParams} = BindingResult2,
    ?assertEqual(
        response_error(409, ExternalID, BenderKey),
        ActualBindingResult2
    ).

%% Internal functions

create_payment(BenderKey, Requests, Config) ->
    Tid = capi_ct_helper_bender:create_storage(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE};
                ('StartPayment', {_, _, IPP}) ->
                    #payproc_InvoicePaymentParams{id = ID, external_id = EID, context = ?CONTENT} = IPP,
                    {ok, ?PAYPROC_PAYMENT(ID, EID)}
            end},
            {bender, fun('GenerateID', {_, _, CtxMsgPack}) ->
                capi_ct_helper_bender:get_internal_id(Tid, BenderKey, CtxMsgPack)
            end}
        ],
        Config
    ),
    Result = [create_payment_(Req, Config) || Req <- Requests],
    _ = capi_ct_helper_bender:del_storage(Tid),
    Result.

create_payment_(Req, Config) ->
    _AccPid = capi_ct_features_reader_event_handler:create_storage(),
    Res = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING),
    UnusedParams = capi_ct_features_reader_event_handler:get_unused_params(),
    capi_ct_features_reader_event_handler:delete_storage(),
    {Res, UnusedParams}.

create_invoices(BenderKey, Requests, Config) ->
    Tid = capi_ct_helper_bender:create_storage(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', {_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}}) ->
                {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
            end},
            {bender, fun('GenerateID', {_, _, CtxMsgPack}) ->
                capi_ct_helper_bender:get_internal_id(Tid, BenderKey, CtxMsgPack)
            end}
        ],
        Config
    ),
    Results = [create_invoice_(Req, Config) || Req <- Requests],
    _ = capi_ct_helper_bender:del_storage(Tid),
    Results.

create_invoice_(Req, Config) ->
    with_feature_storage(fun() ->
        capi_client_invoices:create_invoice(?config(context, Config), Req)
    end).

create_refunds(BenderKey, Requests, Config) ->
    Tid = capi_ct_helper_bender:create_storage(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT])};
                (
                    'RefundPayment',
                    {_, _, _, #payproc_InvoicePaymentRefundParams{id = ID, external_id = EID}}
                ) ->
                    {ok, ?REFUND(ID, EID)}
            end},
            {bender, fun('GenerateID', {_, _, CtxMsgPack}) ->
                capi_ct_helper_bender:get_internal_id(Tid, BenderKey, CtxMsgPack)
            end}
        ],
        Config
    ),
    Results = [create_refund_(Req, Config) || Req <- Requests],
    _ = capi_ct_helper_bender:del_storage(Tid),
    Results.

create_refund_(Req, Config) ->
    _AccPid = capi_ct_features_reader_event_handler:create_storage(),
    Res = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING),
    UnusedParams = capi_ct_features_reader_event_handler:get_unused_params(),
    capi_ct_features_reader_event_handler:delete_storage(),
    {Res, UnusedParams}.

payment_params(ExternalID, MakeRecurrent) ->
    genlib_map:compact(#{
        <<"externalID">> => ExternalID,
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> => MakeRecurrent,
        <<"metadata">> => ?JSON,
        <<"processingDeadline">> => <<"5m">>
    }).

payment_params(ExternalID, Jwe, ContactInfo, MakeRecurrent) ->
    Params = payment_params(ExternalID, MakeRecurrent),
    genlib_map:compact(Params#{
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => Jwe,
            <<"contactInfo">> => ContactInfo
        }
    }).

invoice_params(EID) ->
    #{
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"description">> => <<"test_invoice_description">>,
        <<"externalID">> => EID,
        <<"shopID">> => ?STRING,
        <<"dueDate">> => ?TIMESTAMP,
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"product">> => <<"test_product">>
    }.

get_encrypted_token(PS, ExpDate) ->
    get_encrypted_token(PS, ExpDate, undefined).

get_encrypted_token(PS, ExpDate, IsCvvEmpty) ->
    encrypt_payment_tool(
        {bank_card, #domain_BankCard{
            token = ?TEST_PAYMENT_TOKEN(PS),
            payment_system_deprecated = PS,
            bin = <<"411111">>,
            last_digits = <<"1111">>,
            exp_date = ExpDate,
            cardholder_name = <<"Degus Degusovich">>,
            is_cvv_empty = IsCvvEmpty
        }}
    ).

encrypt_payment_tool(PaymentTool) ->
    capi_crypto:create_encrypted_payment_tool_token(PaymentTool, undefined).

create_customers(BenderKey, Requests, Config) ->
    Context = ?config(context, Config),
    capi_ct_helper_bender:with_storage(
        fun(StorageID) ->
            _ = capi_ct_helper:mock_services(
                [
                    {customer_management, fun(
                        'Create',
                        {#payproc_CustomerParams{customer_id = CustomerID}}
                    ) ->
                        {ok, ?CUSTOMER(CustomerID)}
                    end},
                    {bender, fun('GenerateID', {_, _, CtxMsgPack}) ->
                        capi_ct_helper_bender:get_internal_id(StorageID, BenderKey, CtxMsgPack)
                    end}
                ],
                Config
            ),
            [
                with_feature_storage(fun() -> capi_client_customers:create_customer(Context, R) end)
                || R <- Requests
            ]
        end
    ).

create_customer_bindings(BenderKey, Requests, Config) ->
    Context = ?config(context, Config),
    capi_ct_helper_bender:with_storage(
        fun(StorageID) ->
            _ = capi_ct_helper:mock_services(
                [
                    {customer_management, fun
                        ('Get', _) ->
                            {ok, ?CUSTOMER};
                        (
                            'StartBinding',
                            {_, #payproc_CustomerBindingParams{
                                customer_binding_id = BindingID,
                                rec_payment_tool_id = RecPaymentToolID
                            }}
                        ) ->
                            {ok, ?CUSTOMER_BINDING(BindingID, RecPaymentToolID)}
                    end},

                    {bender, fun('GenerateID', {_, _, CtxMsgPack}) ->
                        capi_ct_helper_bender:get_internal_id(StorageID, BenderKey, CtxMsgPack)
                    end}
                ],
                Config
            ),
            [
                with_feature_storage(fun() -> capi_client_customers:create_binding(Context, ?STRING, R) end)
                || R <- Requests
            ]
        end
    ).

create_invoice_templates(BenderKey, Requests, Config) ->
    Context = ?config(context, Config),
    capi_ct_helper_bender:with_storage(
        fun(StorageID) ->
            _ = capi_ct_helper:mock_services(
                [
                    {invoice_templating, fun(
                        'Create',
                        {_, #payproc_InvoiceTemplateCreateParams{template_id = TemplateID}}
                    ) ->
                        {ok, ?INVOICE_TPL(TemplateID)}
                    end},
                    {bender, fun('GenerateID', {_Key, _, CtxMsgPack}) ->
                        capi_ct_helper_bender:get_internal_id(StorageID, BenderKey, CtxMsgPack)
                    end}
                ],
                Config
            ),
            [
                with_feature_storage(fun() -> capi_client_invoice_templates:create(Context, R) end)
                || R <- Requests
            ]
        end
    ).

create_invoices_with_templates(BenderKey, Requests, Config) ->
    Context = ?config(context, Config),
    capi_ct_helper_bender:with_storage(
        fun(StorageID) ->
            _ = capi_ct_helper:mock_services(
                [
                    {invoice_templating, fun('Get', _) ->
                        {ok, ?INVOICE_TPL}
                    end},
                    {invoicing, fun(
                        'CreateWithTemplate',
                        {_UserInfo, #payproc_InvoiceWithTemplateParams{id = ID, external_id = EID}}
                    ) ->
                        {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
                    end},
                    {bender, fun('GenerateID', {_, _, CtxMsgPack}) ->
                        capi_ct_helper_bender:get_internal_id(StorageID, BenderKey, CtxMsgPack)
                    end}
                ],
                Config
            ),
            [
                with_feature_storage(fun() ->
                    capi_client_invoice_templates:create_invoice(Context, InvoiceTemplateID, Request)
                end)
                || {InvoiceTemplateID, Request} <- Requests
            ]
        end
    ).

with_feature_storage(Fun) ->
    capi_ct_features_reader_event_handler:create_storage(),
    Result = Fun(),
    UnusedParams = capi_ct_features_reader_event_handler:get_unused_params(),
    capi_ct_features_reader_event_handler:delete_storage(),
    {Result, UnusedParams}.

response_error(409, EID, ID) ->
    {error,
        {409, #{
            <<"externalID">> => EID,
            <<"id">> => ID,
            <<"message">> => <<"This 'externalID' has been used by another request">>
        }}}.
