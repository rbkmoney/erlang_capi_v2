-module(capi_invoice_access_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("damsel/include/dmsl_cds_thrift.hrl").
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
    get_invoice_ok_test/1,
    get_invoice_events_ok_test/1,
    get_invoice_payment_methods_ok_test/1,
    create_payment_ok_test/1,
    create_payment_qiwi_access_token_ok_test/1,
    create_payment_with_empty_cvv_ok_test/1,
    create_payment_with_googlepay_plain_ok_test/1,
    get_payments_ok_test/1,
    get_payment_by_id_ok_test/1,
    get_client_payment_status_test/1,
    cancel_payment_ok_test/1,
    capture_payment_ok_test/1,
    capture_partial_payment_ok_test/1,
    create_first_recurrent_payment_ok_test/1,
    create_second_recurrent_payment_ok_test/1,
    get_recurrent_payments_ok_test/1
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
        {group, operations_by_invoice_access_token_after_invoice_creation},
        {group, operations_by_invoice_access_token_after_token_creation}
    ].

invoice_access_token_tests() ->
    [
        get_invoice_ok_test,
        get_invoice_events_ok_test,
        get_invoice_payment_methods_ok_test,
        create_payment_ok_test,
        create_payment_qiwi_access_token_ok_test,
        create_payment_with_empty_cvv_ok_test,
        create_payment_with_googlepay_plain_ok_test,
        get_payments_ok_test,
        get_client_payment_status_test,
        get_payment_by_id_ok_test,
        cancel_payment_ok_test,
        capture_payment_ok_test,
        capture_partial_payment_ok_test,
        create_first_recurrent_payment_ok_test,
        create_second_recurrent_payment_ok_test,
        get_recurrent_payments_ok_test
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_invoice_access_token_after_invoice_creation, [],
            invoice_access_token_tests()
        },
        {operations_by_invoice_access_token_after_token_creation, [],
            invoice_access_token_tests()
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
init_per_group(operations_by_invoice_access_token_after_invoice_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    ExtraProperties = #{<<"ip_replacement_allowed">> => true},
    {ok, Token} = capi_ct_helper:issue_token([{[invoices], write}], unlimited, ExtraProperties),
    capi_ct_helper:mock_services([
        {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end},
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
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

init_per_group(operations_by_invoice_access_token_after_token_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    {ok, Token} = capi_ct_helper:issue_token([{[invoices], write}], unlimited),
    capi_ct_helper:mock_services([
        {invoicing, fun('Get', _)        -> {ok, ?PAYPROC_INVOICE} end},
        {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
    ], MockServiceSup),
    {ok, #{<<"payload">> := InvAccToken}
    } = capi_client_invoices:create_invoice_access_token(capi_ct_helper:get_context(Token), ?STRING),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(InvAccToken)} | Config];

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
-spec get_invoice_ok_test(config()) ->
    _.
get_invoice_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}], Config),
    {ok, _} = capi_client_invoices:get_invoice_by_id(?config(context, Config), ?STRING).

-spec get_invoice_events_ok_test(config()) ->
    _.
get_invoice_events_ok_test(Config) ->
    Inc = fun
        (X) when is_integer(X) -> X + 1;
        (_) -> 1
    end,
    _ = capi_ct_helper:mock_services([
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
    capi_ct_helper:mock_services([{invoicing,        fun('ComputeTerms', _) -> {ok, ?TERM_SET} end},
                                  {party_management, fun('Get', _)          -> {ok, ?PARTY}    end}], Config),
    {ok, _} = capi_client_invoices:get_invoice_payment_methods(?config(context, Config), ?STRING).

-spec create_payment_ok_test(config()) ->
    _.
create_payment_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    capi_ct_helper:mock_services(
        [
            {invoicing, fun('StartPayment', [_, _, IPP]) ->
                #payproc_InvoicePaymentParams{id = ID, external_id = EID, context = ?CONTENT} = IPP,
                {ok, ?PAYPROC_PAYMENT(ID, EID)}
            end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
        ],
        Config
    ),
    PaymentToolToken = ?TEST_PAYMENT_TOKEN,
    Req2 = #{
        <<"externalID">> => ExternalID,
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => PaymentToolToken,
            <<"contactInfo">> => #{
                <<"email">> => <<"bla@bla.ru">>
            }
        },
        <<"metadata">> => ?JSON,
        <<"processingDeadline">> => <<"5m">>
    },
    {ok, #{
        <<"id">> := BenderKey,
        <<"externalID">> := ExternalID
    }} = capi_client_payments:create_payment(?config(context, Config), Req2, ?STRING).

-spec create_payment_with_empty_cvv_ok_test(config()) ->
    _.
create_payment_with_empty_cvv_ok_test(Config) ->
    capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('StartPayment', [_UserInfo, _InvoiceID,
                    #payproc_InvoicePaymentParams{
                        payer = {payment_resource, #payproc_PaymentResourcePayerParams{
                            resource = #domain_DisposablePaymentResource{
                                payment_tool = {
                                    bank_card,
                                    #domain_BankCard{is_cvv_empty = true}
                                }
                            }
                        }}
                    }
                ]) -> {ok, ?PAYPROC_PAYMENT}
             end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
        ],
        Config
    ),
    PaymentToolToken = capi_utils:map_to_base64url(#{
        <<"type"          >> => <<"bank_card">>,
        <<"token"         >> => ?STRING,
        <<"payment_system">> => atom_to_binary(visa, utf8),
        <<"bin"           >> => <<"411111">>,
        <<"masked_pan"    >> => <<"1111">>,
        <<"is_cvv_empty"  >> => atom_to_binary(true, utf8)
    }),
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

-spec create_payment_qiwi_access_token_ok_test(_) ->
  _.
create_payment_qiwi_access_token_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {invoicing, fun
                ('StartPayment', [_UserInfo, _InvoiceID,
                    #payproc_InvoicePaymentParams{
                        payer = {payment_resource, #payproc_PaymentResourcePayerParams{
                            resource = #domain_DisposablePaymentResource{
                                payment_tool = {
                                    digital_wallet,
                                    #domain_DigitalWallet{ token = <<"benderkey0">> }
                                }
                            }
                        }}
                    }
                ]) -> {ok, ?PAYPROC_PAYMENT}
            end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
    ], Config),
    PaymentToolToken = capi_utils:map_to_base64url(#{
        <<"type"    >> => <<"digital_wallet">>,
        <<"provider">> => atom_to_binary(qiwi, utf8),
        <<"id"      >> => <<"+79876543210">>,
        <<"token"   >> => <<"benderkey0">>
    }),
    Req = #{
        <<"flow" >> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"payer">> => #{
            <<"payerType"       >> => <<"PaymentResourcePayer">>,
            <<"paymentSession"  >> => ?TEST_PAYMENT_SESSION,
            <<"paymentToolToken">> => PaymentToolToken,
            <<"contactInfo"     >> => #{ <<"email">> => <<"bla@bla.ru">> }
        }
    },
    {ok, _} = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING).

-spec create_payment_with_googlepay_plain_ok_test(_) ->
    _.
create_payment_with_googlepay_plain_ok_test(Config) ->
    capi_ct_helper:mock_services([
        {invoicing, fun
                ('StartPayment', [_UserInfo, _InvoiceID,
                    #payproc_InvoicePaymentParams{
                        payer = {payment_resource, #payproc_PaymentResourcePayerParams{
                            resource = #domain_DisposablePaymentResource{
                                payment_tool = {
                                    bank_card,
                                    #domain_BankCard{
                                        is_cvv_empty = undefined,
                                        token_provider = undefined,
                                        payment_system = mastercard
                                    }
                                }
                            }
                        }}
                    }
                ]) -> {ok, ?PAYPROC_PAYMENT}
            end},
        {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
    ], Config),
    PaymentToolToken = ?TEST_PAYMENT_TOKEN(mastercard),
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

-spec get_payments_ok_test(config()) ->
    _.
get_payments_ok_test(Config) ->
    Payment0 = ?PAYPROC_PAYMENT(?PAYMENT_WITH_CUSTOMER_PAYER, [?REFUND], [?ADJUSTMENT]),
    Payment1 = ?PAYPROC_PAYMENT(?PAYMENT_WITH_RECURRENT_PAYER, [?REFUND], [?ADJUSTMENT]),
    Payment2 = ?PAYPROC_PAYMENT(?PAYMENT, [?REFUND], [?ADJUSTMENT]),
    Result   = ?PAYPROC_INVOICE([Payment0, Payment1, Payment2]),
    capi_ct_helper:mock_services([{invoicing, fun('Get', _) -> {ok, Result} end}], Config),
    {ok, _} = capi_client_payments:get_payments(?config(context, Config), ?STRING).

-spec get_payment_by_id_ok_test(config()) ->
    _.
get_payment_by_id_ok_test(Config) ->
    Result = ?PAYPROC_PAYMENT(?PAYMENT_WITH_RECURRENT_PAYER, [?REFUND], [?ADJUSTMENT]),
    capi_ct_helper:mock_services([{invoicing, fun('GetPayment', _) -> {ok, Result} end}], Config),
    {ok, _} = capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec get_client_payment_status_test(config()) ->
    _.
get_client_payment_status_test(Config) ->
    {ok, #{
        <<"status">> := <<"failed">>,
        <<"error" >> := #{<<"code">> := <<"InvalidPaymentTool">>}
    }} = get_failed_payment_with_invalid_cvv(Config).

-spec cancel_payment_ok_test(config()) ->
    _.
cancel_payment_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('CancelPayment', _) -> {ok, ok} end}], Config),
    ok = capi_client_payments:cancel_payment(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec capture_payment_ok_test(config()) ->
    _.
capture_payment_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('CapturePaymentNew', _) -> {ok, ok} end}], Config),
    Req = #{
        <<"reason">> => ?STRING
    },
    ok = capi_client_payments:capture_payment(?config(context, Config), Req, ?STRING, ?STRING).

-spec capture_partial_payment_ok_test(config()) ->
    _.
capture_partial_payment_ok_test(Config) ->
    capi_ct_helper:mock_services([{invoicing, fun('CapturePaymentNew', _) -> {ok, ok} end}], Config),
    Req = #{
        <<"reason">> => ?STRING,
        <<"amount">> => 123,
        <<"currency">> => ?RUB
    },
    ok = capi_client_payments:capture_payment(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_first_recurrent_payment_ok_test(config()) ->
    _.
create_first_recurrent_payment_ok_test(Config) ->
    capi_ct_helper:mock_services(
        [
            {invoicing, fun('StartPayment', _) -> {ok, ?PAYPROC_PAYMENT} end},
            {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
        ],
        Config
    ),
    PaymentToolToken = ?TEST_PAYMENT_TOKEN,
    Req2 = #{
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> => true,
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

-spec create_second_recurrent_payment_ok_test(config()) ->
    _.
create_second_recurrent_payment_ok_test(Config) ->
    capi_ct_helper:mock_services(
        [
            {invoicing, fun('StartPayment', _) -> {ok, ?PAYPROC_PAYMENT} end},
            {bender,    fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(<<"bender_key">>)} end}
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

-spec get_recurrent_payments_ok_test(config()) ->
    _.
get_recurrent_payments_ok_test(Config) ->
    Invoice = ?PAYPROC_INVOICE([?PAYPROC_PAYMENT(?RECURRENT_PAYMENT, [?REFUND], [?ADJUSTMENT])]),
    capi_ct_helper:mock_services([{invoicing, fun('Get', _) -> {ok, Invoice} end}], Config),
    {ok, _} = capi_client_payments:get_payments(?config(context, Config), ?STRING).

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
        [
            {invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_FAILED_PAYMENT({failure, Failure})} end}
        ],
        Config
    ),
    % mock_services([{invoicing, fun('GetPayment', _) -> {ok, ?PAYPROC_PAYMENT} end}], Config),
    capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING).
