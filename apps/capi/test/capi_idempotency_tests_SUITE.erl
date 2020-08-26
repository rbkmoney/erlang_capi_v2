-module(capi_idempotency_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("capi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
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

-export([read_payment_features_test/1]).
-export([compare_payment_bank_card_test/1]).
-export([feature_multi_accessor_test/1]).
-export([compare_different_payment_tool_test/1]).
-export([read_invoice_features_test/1]).
-export([read_payment_customer_features_value_test/1]).
-export([compare_invoices_features_test/1]).
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
-export([create_refund_idemp_ok_test/1]).
-export([create_refund_idemp_fail_test/1]).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-define(DIFFERENCE, -1).

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, idempotence_features},
        {group, payment_creation},
        {group, invoice_creation},
        {group, refund_creation}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {idempotence_features, [], [
            read_payment_features_test,
            compare_payment_bank_card_test,
            feature_multi_accessor_test,
            compare_different_payment_tool_test,
            read_invoice_features_test,
            read_payment_customer_features_value_test,
            compare_invoices_features_test
        ]},
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
            create_invoice_idemp_cart_ok_test
        ]},
        {refund_creation, [], [
            create_refund_idemp_ok_test,
            create_refund_idemp_fail_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    ExtraEnv = [{idempotence_event_handler, {capi_ct_handler_event, #{}}}],
    capi_ct_helper:init_suite(?MODULE, Config, ExtraEnv).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(payment_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    ExtraProperties = #{<<"ip_replacement_allowed">> => true},
    {ok, Token} = capi_ct_helper:issue_token([{[invoices], write}], unlimited, ExtraProperties),
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end},
        {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
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
    } = capi_client_invoices:create_invoice(capi_ct_helper:get_context(Token, ExtraProperties), Req),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(InvAccToken)} | Config];
init_per_group(GroupName, Config)
when GroupName =:= invoice_creation
orelse GroupName =:= refund_creation ->
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

%% TESTS

-spec read_payment_features_test(config()) ->
    _.
read_payment_features_test(_Config) ->
    PaymentTool = payment_tool(),
    UnusedPaymentTool = maps:without([<<"token">>, <<"exp_date">>, <<"type">>], PaymentTool),
    Req = payment_params(<<"external id">>, <<"JWE TOKEN">>, #{}, false),
    Request = deep_merge(Req, #{<<"payer">> => #{<<"paymentTool">> => PaymentTool}}),
    Payer = maps:without([<<"payerType">>], maps:get(<<"payer">>, Request)),
    UnusedRequest = Request#{<<"payer">> => Payer#{<<"paymentTool">> => UnusedPaymentTool}},
    {Features, UnusedParams} = read_features(capi_feature_schemas:payment(), Request),
    ?assertMatch(#{
        <<"payer">> := #{
            <<"type">> := PayerType,
            <<"customer">> := undefined,
            <<"recurrent">> := undefined,
            <<"tool">> := #{
                <<"$type">> := ToolType,
                <<"bank_card">> := #{
                    <<"expdate">> := Date,
                    <<"token">> := Token
                },
                <<"crypto">> := #{
                    <<"currency">> := undefined
                },
                <<"mobile_commerce">> := #{
                    <<"operator">> := undefined,
                    <<"phone">> := undefined
                },
                <<"terminal">> := #{
                    <<"terminal_type">> := undefined
                },
                <<"wallet">> := #{
                    <<"id">> := undefined,
                    <<"provider">> := undefined,
                    <<"token">> := Token
                }
            }
        }
    }
    when is_integer(Token)
    and is_integer(Date)
    and is_integer(ToolType)
    and is_integer(PayerType), Features),
    ?assertEqual(UnusedRequest, UnusedParams).

-spec compare_payment_bank_card_test(config()) ->
    _.
compare_payment_bank_card_test(_) ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType    = <<"bank_card">>,
    Token1      = <<"cds token">>,
    Token2      = <<"cds token 2">>,
    CardHolder1 = <<"0x42">>,
    CardHolder2 = <<"Cake">>,
    ExpDate     = {exp_date, 02, 2022},
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => ToolType,
                <<"token">>           => Token1,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder1
            }
    }},
    Request2 = deep_merge(Request1, #{<<"payer">> => #{
        <<"paymentTool">> => #{
            <<"token">> => Token2,
            <<"cardholder_name">> => CardHolder2
        }
    }}),
    Schema = capi_feature_schemas:payment(),
    {F1, _} = read_features(Schema, Request1),
    {F2, _} = read_features(Schema, Request2),
    ?assertEqual(true, capi_idemp_features:compare(F1, F1)),
    {false, Diff} = capi_idemp_features:compare(F1, F2),
    ?assertEqual([
        <<"payer.paymentTool.token">>
    ], capi_idemp_features:list_diff_fields(Schema, Diff)).

-spec feature_multi_accessor_test(config()) ->
    _.
feature_multi_accessor_test(_) ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType    = <<"bank_card">>,
    Token1      = <<"cds token">>,
    Token2      = <<"cds token 2">>,
    CardHolder1 = <<"0x42">>,
    CardHolder2 = <<"Cake">>,
    ExpDate     = {exp_date, 02, 2022},
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{<<"wrapper">> => #{
                <<"type">>            => ToolType,
                <<"token">>           => Token1,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder1
            }
        }
    }},
    Request2 = deep_merge(Request1, #{<<"payer">> => #{
        <<"paymentTool">> => #{<<"wrapper">> => #{
            <<"token">> => Token2,
            <<"cardholder_name">> => CardHolder2
        }}
    }}),
    Schema = #{
        <<"payer">> => [<<"payer">>, #{
            <<"type">> => [<<"payerType">>],
            <<"tool">> => [<<"paymentTool">>, <<"wrapper">>, #{
                <<"$type">> => [<<"type">>],
                <<"bank_card">> => #{
                    <<"token">>      => [<<"token">>],
                    <<"expdate">>    => [<<"exp_date">>]
                }
            }]
        }]
    },
    {F1, _} = read_features(Schema, Request1),
    {F2, _} = read_features(Schema, Request2),
    ?assertEqual(true, capi_idemp_features:compare(F1, F1)),
    {false, Diff} = capi_idemp_features:compare(F1, F2),
    ?assertEqual([<<"payer.paymentTool.wrapper.token">>], capi_idemp_features:list_diff_fields(Schema, Diff)).

-spec compare_different_payment_tool_test(config()) -> _.
compare_different_payment_tool_test(_) ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType1   = <<"bank_card">>,
    ToolType2   = <<"wallet">>,
    Token1      = <<"cds token">>,
    Token2      = <<"wallet token">>,
    CardHolder  = <<"0x42">>,
    ExpDate     = {exp_date, 02, 2022},
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => ToolType1,
                <<"token">>           => Token1,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder
            }
    }},
    Request2 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>  => ToolType2,
                <<"token">> => Token2
            }
        }
    },

    Schema = capi_feature_schemas:payment(),
    {F1, _} = read_features(Schema, Request1),
    {F2, _} = read_features(Schema, Request2),
    ?assertEqual(true, capi_idemp_features:compare(F1, F1)),
    {false, Diff} = capi_idemp_features:compare(F1, F2),
    ?assertEqual([<<"payer.paymentTool">>], capi_idemp_features:list_diff_fields(Schema, Diff)).

-spec read_payment_customer_features_value_test(config()) ->
    _.
read_payment_customer_features_value_test(_) ->
    Req = payment_params(<<"external id">>, undefined),
    Request = Req#{
        <<"payer">> => #{
            <<"payerType">>  => <<"CustomerPayer">>,
            <<"customerID">> => <<"some customer id">>
        }
    },
    ReqUnused = maps:without([<<"payer">>], Request),
    {Features, Unused} = read_features(capi_feature_schemas:payment(), Request),
    ?assertMatch(#{
        <<"payer">> := #{
            <<"type">>      := PayerType,
            <<"customer">>  := CustomerID,
            <<"recurrent">> := undefined,
            <<"tool">>      := undefined
        }
    } when is_integer(PayerType) and is_integer(CustomerID), Features),
    ?assertEqual(ReqUnused, Unused).

-spec read_invoice_features_test(config()) ->
    _.
read_invoice_features_test(_) ->
    Prod1 = <<"blue duck">>,
    Prod2 = <<"yellow duck">>,
    Price = 1,
    {Invoice, Unused} = invoice_params(<<"external id">>),
    Request = Invoice#{<<"cart">> => [
        #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price},
        #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price, <<"not feature">> => <<"hmm">>}
    ]},
    {Features, UnusedParams} = read_features(capi_feature_schemas:invoice(), Request),
    ?assertMatch(#{
        <<"amount">>    := Amount,
        <<"currency">>  := Cur,
        <<"shop_id">>   := ID,
        <<"product">>   := Prod,
        <<"cart">>      := Cart
    }
    when is_integer(Amount)
    and is_integer(Cur)
    and is_integer(ID)
    and is_integer(Prod)
    and is_list(Cart), Features),
    ?assertMatch(Unused, UnusedParams).

-spec compare_invoices_features_test(config()) ->
    _.
compare_invoices_features_test(_) ->
    ShopID  = <<"shopus">>,
    Cur     = <<"RUB">>,
    Prod1   = <<"yellow duck">>,
    Prod2   = <<"blue duck">>,
    Price1  = 10000,
    Price2  = 20000,
    Product = #{
        <<"product">> => Prod1,
        <<"quantity">> => 1,
        <<"price">> => Price1,
        <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>,
            <<"rate">> => <<"10%">>
        }
    },
    Request1 = #{
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"cart">> => [Product]
    },
    Request2 = deep_merge(Request1, #{
        <<"cart">> => [#{<<"product">> => Prod2, <<"price">> => Price2}]
    }),
    Request3 = deep_merge(Request1, #{
        <<"cart">> => [#{<<"product">> => Prod2, <<"price">> => Price2, <<"quantity">> => undefined}]
    }),
    Schema = capi_feature_schemas:invoice(),
    {Invoice1, _} = read_features(Schema, Request1),
    {InvoiceChg1, _} = read_features(Schema, Request1#{<<"cart">> => [
        Product#{
            <<"price">> => Price2,
            <<"taxMode">> => #{
                <<"rate">> => <<"18%">>
            }}
    ]}),
    {Invoice2, _} = read_features(Schema, Request2),
    {InvoiceWithFullCart, _} = read_features(Schema, Request3),
    ?assertEqual({false, #{<<"cart">> => #{
        0 => #{
            <<"price">>     => ?DIFFERENCE,
            <<"product">>   => ?DIFFERENCE,
            <<"quantity">>  => ?DIFFERENCE,
            <<"tax">>       => ?DIFFERENCE
    }}}}, capi_idemp_features:compare(Invoice2, Invoice1)),
    ?assert(capi_idemp_features:compare(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(capi_idemp_features:compare(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(capi_idemp_features:compare(Invoice2, InvoiceWithFullCart)),
    % %% When second request didn't contain feature, this situation detected as conflict.
    ?assertMatch(
        {false, #{<<"cart">> := ?DIFFERENCE}},
        capi_idemp_features:compare(Invoice1#{<<"cart">> => undefined}, Invoice1)
    ),

    {false, Diff} = capi_idemp_features:compare(Invoice1, InvoiceChg1),
    ?assertEqual(
        [<<"cart.0.price">>, <<"cart.0.taxMode.rate">>],
        capi_idemp_features:list_diff_fields(Schema, Diff)
    ),
    ?assert(capi_idemp_features:compare(Invoice1, Invoice1#{<<"cart">> => undefined})).

-spec create_payment_ok_test(config()) ->
    _.
create_payment_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ContactInfo = #{},
    Jwe1 = get_encrypted_token(visa, ?EXP_DATE(2, 2020)),
    Jwe2 = get_encrypted_token(visa, ?EXP_DATE(2, 2020)),
    Req1 = payment_params(ExternalID, Jwe1, ContactInfo, undefined),
    Req2 = payment_params(ExternalID, Jwe2, ContactInfo, false),
    {
        {{ok, Response1}, Unused1},
        {{ok, Response2}, _}
    } = create_payment(BenderKey, Req1, Req2, Config),
    ?assertMatch(#{
        <<"externalID">> := <<"merch_id">>,
        <<"flow">> := #{<<"type">> := <<"PaymentFlowInstant">>},
        <<"metadata">> := #{<<"bla">> := "*"},
        <<"payer">> := #{
            <<"contactInfo">> := #{},
            <<"paymentSession">> := PS,
            <<"paymentTool">> := #{
                <<"bin">> := <<"411111">>,
                <<"cardholder_name">> := <<"Degus Degusovich">>,
                <<"masked_pan">> := <<"1111">>,
                <<"payment_system">> := visa
            }
        }
    } when is_binary(PS), Unused1),
    ?assertEqual(Response1, Response2).

-spec create_payment_fail_test(config()) ->
    _.
create_payment_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Jwe1 = get_encrypted_token(visa, ?EXP_DATE(1, 2020)),
    Jwe2 = get_encrypted_token(visa, ?EXP_DATE(2, 2020)),
    Req1 = payment_params(ExternalID, Jwe1, #{}, undefined),
    Req2 = payment_params(ExternalID, Jwe2, #{}, false),
    {{{ok, _}, _}, {Response, _}} = create_payment(BenderKey, Req1, Req2, Config),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response).

-spec different_payment_tools_test(config()) ->
    _.
different_payment_tools_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ContactInfo = #{},
    Jwe1 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Mr. Surname">>)}),
    Jwe2 = encrypt_payment_tool({digital_wallet, ?DIGITAL_WALLET(<<"+79876543210">>, <<"token id">>)}),
    Req1 = payment_params(ExternalID, Jwe1, ContactInfo, undefined),
    Req2 = payment_params(ExternalID, Jwe2, ContactInfo, false),
    {{{ok, _}, _}, {Response2, Unused}} = create_payment(BenderKey, Req1, Req2, Config),
      ?assertMatch(#{
        <<"externalID">> := <<"merch_id">>,
        <<"flow">> := #{<<"type">> := <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> := false,
        <<"metadata">> := #{<<"bla">> := "*"},
        <<"payer">> := #{
            <<"contactInfo">> := #{},
            <<"paymentSession">> := PS,
            <<"paymentToolToken">> := PT
        },
        <<"processingDeadline">> := <<"5m">>
    } when is_binary(PS) and is_binary(PT), Unused),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2).

-spec second_request_without_idempotent_feature_test(config()) ->
    _.
second_request_without_idempotent_feature_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ContactInfo = #{},
    Jwe1 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Mr. Surname">>)}),
    Jwe2 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, undefined, <<"Mr. Surname">>)}),
    Req1 = payment_params(ExternalID, Jwe1, ContactInfo, undefined),
    Req2 = payment_params(ExternalID, Jwe2, ContactInfo, undefined),
    {{{ok, _}, _}, {Response2, _}} = create_payment(BenderKey, Req1, Req2, Config),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2).

-spec second_request_with_idempotent_feature_test(config()) ->
    _.
second_request_with_idempotent_feature_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Jwe1 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), undefined)}),
    Jwe2 = encrypt_payment_tool({bank_card, ?BANK_CARD(visa, ?EXP_DATE(2, 2020), <<"Mr. Surname">>)}),
    Req1 = payment_params(ExternalID, Jwe1, #{}, undefined),
    Req2 = payment_params(ExternalID, Jwe2, #{}, undefined),
    {{{ok, Response1}, _}, {{ok, Response2}, _}} = create_payment(BenderKey, Req1, Req2, Config),
    ?assertEqual(Response1, Response2).

-spec create_invoice_ok_test(config()) ->
    _.
create_invoice_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', [_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}]) ->
            {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
    ], Config),
    {Params, Unused} = invoice_params(ExternalID),
    Req = maps:merge(Params, Unused),
    {{ok, #{<<"invoice">> := Invoice1}}, Unused1} = create_invoice_(Req, Config),
    {{ok, #{<<"invoice">> := Invoice2}}, Unused2} = create_invoice_(Req, Config),

    ?assertEqual(Unused, Unused2),
    ?assertEqual(Unused, Unused1),
    ?assertEqual(BenderKey,  maps:get(<<"id">>, Invoice1)),
    ?assertEqual(ExternalID, maps:get(<<"externalID">>, Invoice1)),
    ?assertEqual(Invoice1, Invoice2).

-spec create_invoice_legacy_fail_test(config()) ->
    _.
create_invoice_legacy_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    {Params, Unused} = invoice_params(ExternalID),
    Req = maps:merge(Params, Unused),
    Req2 = Req#{<<"product">> => <<"test_product2">>},
    Ctx = capi_msgp_marshalling:marshal(#{<<"version">> => 1, <<"params_hash">> => erlang:phash2(Req)}),
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', [_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}]) ->
            {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
        end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey, Ctx)} end}
    ], Config),
    {{ok, Invoice1}, _} = create_invoice_(Req, Config),
    #{<<"invoice">> := #{<<"id">> := InvoiceID}} = Invoice1,
    {Response, Unused2} = create_invoice_(Req2, Config),
    ?assertEqual(Unused, Unused2),
    ?assertEqual(response_error(409, ExternalID, InvoiceID), Response).

-spec create_invoice_fail_test(config()) ->
    _.
create_invoice_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    {Params, Unused} = invoice_params(ExternalID),
    Req1 = maps:merge(Params, Unused),
    Req2 = Req1#{<<"product">> => <<"test_product2">>},
    [
        {{ok, #{<<"invoice">> := #{<<"id">> := InvoiceID}}}, _},
        {Response, _}
    ] = create_invoice(BenderKey, [Req1, Req2], Config),
    ?assertEqual(response_error(409, ExternalID, InvoiceID), Response).

-spec create_invoice_idemp_cart_ok_test(config()) ->
    _.
create_invoice_idemp_cart_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    {Params, Unused} = invoice_params(ExternalID),
    Req1 = Params#{
        <<"amount">> => 10000,
        <<"cart">> => [
            #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 9000},
            #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 1000}
    ]},
    Req2 = Req1#{<<"cart">> => [
        #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 1000},
        #{<<"product">> => <<"product#1">>, <<"quantity">> => 1, <<"price">> => 9000}
    ]},
    [
        {{ok, #{<<"invoice">> := Invoice1}}, UnusedParams1},
        {{ok, #{<<"invoice">> := Invoice2}}, UnusedParams2}
    ] = create_invoice(BenderKey, [Req1, Req2], Config),
    ?assertEqual(Invoice1, Invoice2),
    ?assertEqual(Unused, UnusedParams1),
    ?assertEqual(Unused, UnusedParams2).

-spec create_invoice_idemp_cart_fail_test(config()) ->
    _.
create_invoice_idemp_cart_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    {Req, Unused} = invoice_params(ExternalID),
    Req1 = Req#{<<"cart">> => [#{
        <<"product">> => <<"product#1">>,
        <<"quantity">> => 1,
        <<"price">> => ?INTEGER,
        <<"taxMode">> => #{<<"type">> => <<"InvoiceLineTaxVAT">>, <<"rate">> => <<"18%">>}
    }]},
    Req2 = Req#{<<"cart">> => [#{
        <<"product">> => <<"product#1">>,
        <<"quantity">> => 2,
        <<"price">> => ?INTEGER
    }]},
    Req3 = Req#{<<"cart">> => [#{
        <<"product">> => <<"product#1">>,
        <<"quantity">> => 1,
        <<"price">> => ?INTEGER
    }]},
    [
        {{ok, _}, UnusedParams},
        {Response2, _},
        {Response3, _}
    ] = create_invoice(BenderKey, [Req1, Req2, Req3], Config),
    ?assertEqual(Unused, UnusedParams),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response3).

-spec create_refund_idemp_ok_test(config()) ->
    _.
create_refund_idemp_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req1 = #{
        <<"reason">> => ?STRING,
        <<"externalID">>  => ExternalID,
        <<"id">> => ?STRING
    },
    Req2 = Req1#{
        <<"amount">> => 10000,
        <<"currency">> => <<"RUB">>
    },
    [Refund1, Refund2] = create_refund(BenderKey, [Req1, Req2], Config),
    ?assertEqual(Refund1, Refund2).

-spec create_refund_idemp_fail_test(config()) ->
    _.
create_refund_idemp_fail_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    Req1 = #{
        <<"reason">> => ?STRING,
        <<"externalID">>  => ExternalID,
        <<"id">> => ?STRING,
        <<"currency">> => <<"RUB">>,
        <<"cart">> => [#{<<"product">> => <<"dog">>, <<"quantity">> => 1, <<"price">> => 500}]
    },
    Req2 = Req1#{
        <<"cart">> => [
            #{<<"product">> => <<"dog">>, <<"quantity">> => 1, <<"price">> => 500},
            #{<<"product">> => <<"cat">>, <<"quantity">> => 1, <<"price">> => 500}
        ]
    },
    [{{ok, _Refund1}, Unused1}, {Response2, Unused2}] = create_refund(BenderKey, [Req1, Req2], Config),
    Unused = #{
        <<"externalID">> => <<"merch_id">>,
        <<"id">> => <<"TEST">>,
        <<"invoiceID">> => <<"TEST">>,
        <<"paymentID">> => <<"TEST">>,
        <<"reason">> => <<"TEST">>
    },
    ?assertEqual(Unused, Unused1),
    ?assertEqual(Unused, Unused2),
    ?assertEqual(response_error(409, ExternalID, BenderKey), Response2).

%% Internal functions

create_payment(BenderKey, Req1, Req2, Config) ->
    Tid = capi_ct_helper_bender:create_storage(),
    capi_ct_helper:mock_services(
        [
            {invoicing, fun('StartPayment', [_, _, IPP]) ->
                #payproc_InvoicePaymentParams{id = ID, external_id = EID, context = ?CONTENT} = IPP,
                {ok, ?PAYPROC_PAYMENT(ID, EID)}
            end},
            {bender, fun('GenerateID', [_, _, CtxMsgPack]) ->
                capi_ct_helper_bender:get_internal_id(Tid, BenderKey, CtxMsgPack)
            end}
        ],
        Config
    ),
    Result1 = create_payment_(Req1, Config),
    Result2 = create_payment_(Req2, Config),
    capi_ct_helper_bender:del_storage(Tid),
    {Result1, Result2}.

create_payment_(Req, Config) ->
    _AccPid = capi_ct_handler_event:create_acc(),
    Res = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING),
    UnusedParams = capi_ct_handler_event:get_unused_params(),
    capi_ct_handler_event:del_storage(),
    {Res, UnusedParams}.

create_invoice(BenderKey, Requests, Config) ->
    Tid = capi_ct_helper_bender:create_storage(),
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', [_UserInfo, #payproc_InvoiceParams{id = ID, external_id = EID}]) ->
            {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
        end},
        {bender, fun('GenerateID', [_, _, CtxMsgPack]) ->
            capi_ct_helper_bender:get_internal_id(Tid, BenderKey, CtxMsgPack)
        end}
    ], Config),
    Results = lists:foldl(fun(Req, Acc) -> [create_invoice_(Req, Config) | Acc] end, [], Requests),
    capi_ct_helper_bender:del_storage(Tid),
    lists:reverse(Results).

create_invoice_(Req, Config) ->
    _AccPid = capi_ct_handler_event:create_acc(),
    Res = capi_client_invoices:create_invoice(?config(context, Config), Req),
    UnusedParams = capi_ct_handler_event:get_unused_params(),
    capi_ct_handler_event:del_storage(),
    {Res, UnusedParams}.

create_refund(BenderKey, Requests, Config) ->
    Tid = capi_ct_helper_bender:create_storage(),
    capi_ct_helper:mock_services([
        {invoicing,
            fun(
                'RefundPayment',
                [_, _, _, #payproc_InvoicePaymentRefundParams{id = ID, external_id = EID}]
            ) ->
                {ok, ?REFUND(ID, EID)}
        end},
        {bender, fun('GenerateID', [_, _, CtxMsgPack]) ->
            capi_ct_helper_bender:get_internal_id(Tid, BenderKey, CtxMsgPack)
        end}
    ], Config),
    Results = lists:foldl(fun(Req, Acc) ->
        [create_refund_(Req, Config) | Acc]
    end, [], Requests),
    capi_ct_helper_bender:del_storage(Tid),
    lists:reverse(Results).

create_refund_(Req, Config) ->
    _AccPid = capi_ct_handler_event:create_acc(),
    Res = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING),
    UnusedParams = capi_ct_handler_event:get_unused_params(),
    capi_ct_handler_event:del_storage(),
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

payment_tool() ->
    #{
        <<"type">> => <<"bank_card">>,
        <<"token">> => <<"cds token">>,
        <<"payment_system">> => <<"visa">>,
        <<"bin">> => <<"411111">>,
        <<"last_digits">> => <<"1111">>,
        <<"exp_date">> => {exp_date, 02, 2022},
        <<"cardholder_name">> => <<"Degus Degusovich">>,
        <<"is_cvv_empty">> => false
    }.

invoice_params(EID) ->
    Unused = #{
        <<"metadata">>    => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"dueDate">>     => ?TIMESTAMP,
        <<"description">> => <<"test_invoice_description">>,
        <<"externalID">>  => EID
    },
    {genlib_map:compact(maps:merge(Unused, #{
        <<"shopID">>      => ?STRING,
        <<"amount">>      => ?INTEGER,
        <<"currency">>    => ?RUB,
        <<"product">>     => <<"test_product">>
    })), Unused}.

get_encrypted_token(PS, ExpDate) ->
    get_encrypted_token(PS, ExpDate, undefined).
get_encrypted_token(PS, ExpDate, IsCvvEmpty) ->
    encrypt_payment_tool({bank_card, #domain_BankCard{
        token = ?TEST_PAYMENT_TOKEN(PS),
        payment_system = PS,
        bin = <<"411111">>,
        last_digits = <<"1111">>,
        exp_date = ExpDate,
        cardholder_name = <<"Degus Degusovich">>,
        is_cvv_empty = IsCvvEmpty
    }}).

encrypt_payment_tool(PaymentTool) ->
    capi_crypto:create_encrypted_payment_tool_token(PaymentTool).

response_error(409, EID, ID) ->
    {error, {409, #{
        <<"externalID">> => EID,
        <<"id">> => ID,
        <<"message">> => <<"This 'externalID' has been used by another request">>
    }}}.

read_features(Schema, Request) ->
    capi_ct_handler_event:create_acc(),
    Features = capi_idemp_features:read({capi_ct_handler_event, undefined}, Schema, Request),
    UnusedParams = capi_ct_handler_event:get_unused_params(),
    capi_ct_handler_event:del_storage(),
    {Features, UnusedParams}.

deep_merge(M1, M2) ->
    maps:fold(
        fun (K, V, MAcc) when is_map(V) ->
                Value = deep_merge(maps:get(K, MAcc, #{}), V),
                MAcc#{K => Value};
            (K, V, MAcc) ->
                MAcc#{K => V}
        end, M1, M2).
