-module(capi_tests_real_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_accounter_thrift.hrl").
-include_lib("cp_proto/include/cp_reporting_thrift.hrl").


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init/1]).

%% test cases
-export([
    authorization_ok_test/1,
    authorization_error_no_header_test/1,
    authorization_error_expired_test/1,
    create_invoice_badard_test/1,
    create_invoice_no_shop_test/1,
    create_invoice_ok_test/1,
    create_invoice_w_cart/1,
    create_invoice_access_token_ok_test/1,
    create_payment_ok_test/1,
    create_payment_ok_w_access_token_test/1,
    create_payment_tool_token_ok_test/1,
    create_payment_terminal_tool_token/1,
    create_payment_tool_token_w_access_token_ok_test/1,
    get_invoice_by_id_ok_test/1,
    get_invoice_by_id_w_access_token_ok_test/1,
    get_random_invoice_w_access_token_failed_test/1,
    get_invoice_events_ok_test/1,
    rescind_invoice_ok_test/1,
    rescind_invoice_w_access_token_failed_test/1,
    fulfill_invoice_ok_test/1,
    get_payments_ok_test/1,
    get_payment_by_id_ok_test/1,
    create_payment_hold_ok_test/1,
    cancel_payment_ok_test/1,
    capture_payment_ok_test/1,
    %%%%
    create_refund/1,
    get_refund_by_id/1,
    get_refunds/1,
    get_refund_events/1,
    %%%%
    create_invoice_template_badard_test/1,
    create_invoice_template_no_shop_test/1,
    create_invoice_with_template_invalid_id_test/1,
    create_invoice_template_0_lifetime_test/1,
    create_invoice_template_ok_test/1,
    get_invoice_template_by_id_ok_test/1,
    create_invoice_with_template_ok_test/1,
    update_invoice_template_ok_test/1,
    update_invoice_template_0_lifetime_test/1,
    delete_invoice_template_ok_test/1,
    create_invoice_with_template_removed_template_test/1,
    %%%%
    search_invoices_ok_test/1,
    search_payments_ok_test/1,
    get_payment_conversion_stats_ok_test/1,
    get_payment_revenue_stats_ok_test/1,
    get_payment_geo_stats_ok_test/1,
    get_payment_rate_stats_ok_test/1,
    get_payment_method_stats_ok_test/1,
    %%%%
    get_my_party_ok_test/1,
    suspend_my_party_idempotent_ok_test/1,
    activate_my_party_idempotent_ok_test/1,
    get_claim_by_id_ok_test/1,
    revoke_claim_ok_test/1,
    get_claims_by_status_ok_test/1,
    get_categories_ok_test/1,
    get_category_by_id_ok_test/1,
    create_shop_ok_test/1,
    get_shop_by_id_ok_test/1,
    get_shops_ok_test/1,
    update_shop_ok_test/1,
    suspend_shop_ok_test/1,
    activate_shop_idempotent_ok_test/1,
    %%%%
    get_account_by_id_ok_test/1,
    %%%%
    create_contract_ok_test/1,
    get_contract_by_id_ok_test/1,
    get_contracts_ok_test/1,
    create_payout_tool_ok_test/1,
    get_payout_tools_ok_test/1,
    %%%%
    create_webhook_error_test/1,
    create_webhook_receive_events_test/1,
    get_locations_names_ok_test/1,
    %%%%
    get_reports/1,
    download_report_file/1
]).

-define(PROTOCOL, ipv4).

-define(KEYCLOAK_URL, "keycloak:8080").
-define(KEYCLOAK_USER, "demo_merchant").
-define(KEYCLOAK_PASSWORD, "test").

-define(CAPI_IP                     , "::").
-define(CAPI_PORT                   , 8080).
-define(CAPI_URL                    , "localhost:" ++ integer_to_list(?CAPI_PORT)).
-define(CAPI_SERVICE_TYPE           , real).
-define(CAPI_PARTY_MANAGEMENT_URL   , "http://hellgate:8022/v1/processing/partymgmt").
-define(CAPI_ACCOUNTER_URL          , "http://shumway:8022/accounter").
-define(CAPI_INVOICING_URL          , "http://hellgate:8022/v1/processing/invoicing").
-define(CAPI_INVOICE_TEMPLATING_URL , "http://hellgate:8022/v1/processing/invoice_templating").
-define(CAPI_WEBHOOK_MGR_URL        , "http://hooker:8022/hook").
-define(CAPI_REPOSITORY_URL         , "http://dominant:8022/v1/domain/repository").
-define(CAPI_CDS_STORAGE_URL        , "http://cds:8022/v1/storage").
-define(CAPI_MERCHANT_STAT_URL      , "http://magista:8022/stat").
-define(CAPI_REPORTING_URL          , "http://reporter:8022/reports").
-define(CAPI_GEO_IP_URL             , "http://columbus:8022/repo").
-define(CAPI_HOST_NAME              , "capi").

-define(MERCHANT_ID, <<"281220eb-a4ef-4d03-b666-bdec4b26c5f7">>).
-define(LIVE_CATEGORY_ID, 100).

-define(DEFAULT_PRODUCT         , <<"test_product">>).
-define(DEFAULT_DESCRIPTION     , <<"test_invoice_description">>).
-define(DEFAULT_META            , #{<<"invoice_dummy_metadata">> => <<"test_value">>}).
-define(DEFAULT_TPL_PRODUCT     , <<"test_invoice_template_product">>).
-define(DEFAULT_TPL_DESCRIPTION , <<"test_invoice_template_description">>).
-define(DEFAULT_TPL_META        , #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}).
-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.


-type config() :: [{atom(), any()}].

-define(setconfig(K, V, C), lists:keystore(K, 1, C, {K, V})).

-spec all() -> [
    {group, GroupName :: atom()}
].

all() ->
    [
        {group, authorization},
        {group, invoice_management},
        {group, invoice_template_management},
        {group, card_payment},
        {group, terminal_payment},
        {group, refund},
        {group, invoice_access_token_management},
        {group, statistics},
        {group, party_management},
        {group, contracts_management},
        {group, claims_management},
        {group, shops_management},
        {group, accounts_management},
        {group, webhook_management},
        {group, geo_ip},
        {group, cancel_payment},
        {group, capture_payment},
        {group, reports}
    ].

-spec groups() -> [
    Group :: {
        Name :: atom(),
        [parallel | sequence],
        [TestCase :: atom()]
    }
].

groups() ->
    [
        {authorization, [parallel], [
            authorization_ok_test,
            authorization_error_no_header_test,
            authorization_error_expired_test
        ]},
        {invoice_management, [sequence], [
            create_invoice_badard_test,
            create_invoice_no_shop_test,
            create_invoice_w_cart,
            create_invoice_ok_test,
            get_invoice_by_id_ok_test,
            rescind_invoice_ok_test
        ]},
        {invoice_template_management, [sequence], [
            create_invoice_template_badard_test,
            create_invoice_template_no_shop_test,
            create_invoice_with_template_invalid_id_test,
            create_invoice_template_0_lifetime_test,
            create_invoice_template_ok_test,
            get_invoice_template_by_id_ok_test,
            create_invoice_with_template_ok_test,
            update_invoice_template_ok_test,
            update_invoice_template_0_lifetime_test,
            delete_invoice_template_ok_test,
            create_invoice_with_template_removed_template_test
        ]},
        {card_payment, [sequence], [
            create_invoice_ok_test,
            create_payment_tool_token_ok_test,
            create_payment_ok_test,
            get_payments_ok_test,
            get_payment_by_id_ok_test,
            fulfill_invoice_ok_test,
            get_invoice_events_ok_test
        ]},
        {terminal_payment, [sequence], [
            create_invoice_ok_test,
            create_payment_terminal_tool_token,
            create_payment_ok_test,
            get_payments_ok_test,
            get_payment_by_id_ok_test,
            fulfill_invoice_ok_test,
            get_invoice_events_ok_test
        ]},
        {refund, [sequence], [
            create_invoice_ok_test,
            create_payment_tool_token_ok_test,
            create_payment_ok_test,
            create_refund,
            get_refund_by_id,
            get_refunds,
            get_refund_events
        ]},
        % TODO
        % Extremely sensitive stuff, must verify it better.
        % 1. Test that we're unauthorized to create payments on unrelated invoice.
        % 2. Test that token expires properly.
        {invoice_access_token_management, [sequence], [
            create_invoice_ok_test,
            create_invoice_access_token_ok_test,
            get_invoice_by_id_w_access_token_ok_test,
            get_random_invoice_w_access_token_failed_test,
            rescind_invoice_w_access_token_failed_test,
            create_payment_tool_token_w_access_token_ok_test,
            create_payment_ok_w_access_token_test
        ]},
        {statistics, [sequence], [
            search_invoices_ok_test,
            search_payments_ok_test,
            get_payment_conversion_stats_ok_test,
            get_payment_revenue_stats_ok_test,
            get_payment_geo_stats_ok_test,
            get_payment_rate_stats_ok_test,
            get_payment_method_stats_ok_test
        ]},
        {party_management, [sequence], [
            get_my_party_ok_test,
            suspend_my_party_idempotent_ok_test,
            activate_my_party_idempotent_ok_test
        ]},
        {contracts_management, [sequence], [
            create_contract_ok_test,
            get_contract_by_id_ok_test,
            create_payout_tool_ok_test,
            get_payout_tools_ok_test,
            get_contracts_ok_test
        ]},
        {claims_management, [sequence], [
            get_claim_by_id_ok_test,
            get_claims_by_status_ok_test,
            revoke_claim_ok_test
        ]},
        {shops_management, [sequence], [
            get_categories_ok_test,
            get_category_by_id_ok_test,
            create_shop_ok_test,
            get_shop_by_id_ok_test,
            get_shops_ok_test,
            activate_shop_idempotent_ok_test,
            update_shop_ok_test,
            suspend_shop_ok_test
        ]},
        {accounts_management, [sequence], [
            get_categories_ok_test,
            get_category_by_id_ok_test,
            create_shop_ok_test,
            get_account_by_id_ok_test
        ]},
        {webhook_management, [sequence], [
            create_webhook_error_test,
            create_webhook_receive_events_test
        ]},
        {geo_ip, [parallel], [
            get_locations_names_ok_test
        ]},
        {cancel_payment, [sequence], [
            create_invoice_ok_test,
            create_payment_tool_token_ok_test,
            create_payment_hold_ok_test,
            cancel_payment_ok_test
        ]},
        {capture_payment, [sequence], [
            create_invoice_ok_test,
            create_payment_tool_token_ok_test,
            create_payment_hold_ok_test,
            capture_payment_ok_test
        ]},
        {reports, [sequence], [
            get_reports,
            download_report_file
        ]}
    ].
%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({capi_client_lib, 'handle_response', '_'}, x),
    Apps =
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(cowlib) ++
        capi_ct_helper:start_app(woody) ++
        capi_ct_helper:start_app(cp_proto, [
            {service_urls, #{
                party_management   => ?CAPI_PARTY_MANAGEMENT_URL,
                accounter          => ?CAPI_ACCOUNTER_URL,
                invoicing          => ?CAPI_INVOICING_URL,
                invoice_templating => ?CAPI_INVOICE_TEMPLATING_URL,
                webhook_manager    => ?CAPI_WEBHOOK_MGR_URL,
                repository         => ?CAPI_REPOSITORY_URL,
                cds_storage        => ?CAPI_CDS_STORAGE_URL,
                merchant_stat      => ?CAPI_MERCHANT_STAT_URL,
                reporting          => ?CAPI_REPORTING_URL,
                geo_ip_service     => ?CAPI_GEO_IP_URL
            }}
        ]),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    Params = #{
        url  => ?KEYCLOAK_URL,
        user => ?KEYCLOAK_USER,
        password => ?KEYCLOAK_PASSWORD,
        retries => 10,
        timeout => 5000,
        protocol => ?PROTOCOL
    },
    {ok, Token} = capi_ct_helper:login(Params),
    Retries = 10,
    Timeout = 5000,
    Context = get_context(Token, Retries, Timeout),
    NewConfig = [{apps, lists:reverse(Apps)}, {context, Context}, {test_sup, SupPid} | Config],
    Proxies = [
        start_handler(capi_dummy_provider, 1, #{}, NewConfig),
        start_handler(capi_dummy_inspector, 2, #{<<"risk_score">> => <<"high">>}, NewConfig)
    ],
    populate_snapshot(Proxies),

    NewConfig.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    exit(?config(test_sup, C), shutdown),
    ok = cleanup(),
    [application:stop(App) || App <- proplists:get_value(apps, C)].

-spec init_per_group(Name :: atom(), config()) -> config().

init_per_group(Group = statistics, Config) ->
    Apps = start_capi(Group, Config),
    ShopID = create_and_activate_shop(Config),
    [{capi_apps, Apps}, {shop_id, ShopID} | Config];

init_per_group(Group, Config) ->
    Apps = start_capi(Group, Config),
    [{capi_apps, Apps} | Config].

start_capi(Group, Config) ->
    capi_ct_helper:start_app(capi, [
        {ip, ?CAPI_IP},
        {port, ?CAPI_PORT},
        {service_type, ?CAPI_SERVICE_TYPE},
        {authorizers, #{
            jwt => get_authorizer_opts(Group, Config)
        }}
    ]).

get_authorizer_opts(authorization, Config) ->
    #{
        signee => local,
        keyset => #{
            local    => get_keysource("keys/local/private.pem", Config)
        }
    };
get_authorizer_opts(_, Config) ->
    #{
        signee => capi,
        keyset => #{
            keycloak => get_keysource("keys/keycloak/public.pem", Config),
            capi     => get_keysource("keys/local/private.pem", Config)
        }
    }.

get_keysource(Fn, Config) ->
    {pem_file, filename:join(?config(data_dir, Config), Fn)}.

-spec end_per_group(Name :: atom(), config()) -> config().

end_per_group(_, Config) ->
    _ = [application:stop(App) || App <- ?config(capi_apps, Config)],
    lists:keydelete(capi_apps, 1, Config).

%% tests

-spec authorization_ok_test(config()) -> _.

authorization_ok_test(_Config) ->
    {ok, Token} = auth_token(capi_acl:from_list([]), {lifetime, 10}),
    Headers = [auth_header(Token), content_type_header(), req_id_header()],
    {ok, 200, _RespHeaders, _Body} = call(get, "/v1/processing/categories", #{}, Headers).

-spec authorization_error_no_header_test(config()) -> _.

authorization_error_no_header_test(_Config) ->
    Headers = [content_type_header(), req_id_header()],
    {ok, 401, _RespHeaders, _Body} = call(get, "/v1/processing/categories", #{}, Headers).

-spec authorization_error_expired_test(config()) -> _.

authorization_error_expired_test(_Config) ->
    {ok, Token} = auth_token(capi_acl:from_list([]), {lifetime, -10}),
    Headers = [auth_header(Token), content_type_header(), req_id_header()],
    {ok, 401, _RespHeaders, _Body} = call(get, "/v1/processing/categories", #{}, Headers).

-spec create_invoice_badard_test(config()) -> _.

create_invoice_badard_test(Config) ->
    Context = ?config(context, Config),
    Req = #{},
    {error, _} = capi_client_invoices:create_invoice(Context, Req).

-spec create_invoice_no_shop_test(config()) -> _.

create_invoice_no_shop_test(Config) ->
    Context = ?config(context, Config),
    Req = #{
        <<"shopID">> => <<"INVALID_SHOP_ID">>,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>,
        <<"metadata">> => ?DEFAULT_META,
        <<"dueDate">> => get_due_date(),
        <<"product">> => ?DEFAULT_PRODUCT,
        <<"description">> => ?DEFAULT_DESCRIPTION
    },
    {error, Resp} = capi_client_invoices:create_invoice(Context, Req),
    #{
        <<"code">> := <<"invalidShopID">>
    } = jsx:decode(Resp, [return_maps]).

-spec create_invoice_w_cart(config()) -> _.

create_invoice_w_cart(Config) ->
    ShopID = create_and_activate_shop(Config),
    Context = ?config(context, Config),
    Req0 = #{
        <<"shopID">> => ShopID,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>,
        <<"metadata">> => ?DEFAULT_META,
        <<"dueDate">> => get_due_date(),
        <<"product">> => ?DEFAULT_PRODUCT,
        <<"description">> => ?DEFAULT_DESCRIPTION
    },
    Req1 = maps:remove(<<"amount">>, Req0),
    Cart0 = [],
    {error, Error1} = capi_client_invoices:create_invoice(Context, Req0#{<<"cart">> => Cart0}),
    {error, Error1} = capi_client_invoices:create_invoice(Context, Req1#{<<"cart">> => Cart0}),
    #{<<"code">> := <<"invalidInvoiceCart">>} = jsx:decode(Error1, [return_maps]),
    Cart1 = [
        #{
            <<"product">> => ?DEFAULT_PRODUCT,
            <<"price">> => 1000,
            <<"quantity">> => 1
        }
    ],
    {error, Error2} = capi_client_invoices:create_invoice(Context, Req0#{<<"cart">> => Cart1}),
    #{<<"code">> := <<"invalidInvoiceCost">>} = jsx:decode(Error2, [return_maps]),
    {ok, #{<<"invoice">> := #{
        <<"cart">> := ResultCart1
    }}} = capi_client_invoices:create_invoice(Context, Req1#{<<"cart">> => Cart1}),
    true = invoice_cart_equal(ResultCart1, Cart1),
    Cart2 = [
        #{
            <<"product">> => ?DEFAULT_PRODUCT,
            <<"price">> => 1000,
            <<"quantity">> => 100,
            <<"taxMode">> => #{
                <<"type">> => <<"InvoiceLineTaxVAT">>,
                <<"rate">> => <<"18%">>
            }
        }
    ],
    {ok, #{<<"invoice">> := #{
        <<"cart">> := ResultCart2
    }}} = capi_client_invoices:create_invoice(Context, Req0#{<<"cart">> => Cart2}),
    true = invoice_cart_equal(ResultCart2, Cart2).

invoice_cart_equal([Line1 | C1], [Line2 | C2]) ->
    case invoice_cart_line_equal(Line1, Line2) of
        true ->
            invoice_cart_equal(C1, C2);
        false ->
            false
    end;
invoice_cart_equal([], []) ->
    true;
invoice_cart_equal(_, _) ->
    false.

invoice_cart_line_equal(Line1, Line2) ->
    genlib_map:get(<<"product">>, Line1) == genlib_map:get(<<"product">>, Line2) andalso
    genlib_map:get(<<"price">>, Line1) == genlib_map:get(<<"price">>, Line2) andalso
    genlib_map:get(<<"quantity">>, Line1) == genlib_map:get(<<"quantity">>, Line2) andalso
    genlib_map:get(<<"taxMode">>, Line1) == genlib_map:get(<<"taxMode">>, Line2).

-spec create_invoice_ok_test(config()) -> _.

create_invoice_ok_test(Config) ->
    #{
        <<"invoice">> := #{<<"id">> := InvoiceID},
        <<"invoiceAccessToken">> := #{<<"payload">> := TokenPayload}
    } = default_create_invoice(Config),
    Context = get_context(TokenPayload),
    {save_config, #{
        invoice_id      => InvoiceID,
        invoice_context => Context
    }}.

-spec create_invoice_access_token_ok_test(config()) -> _.

create_invoice_access_token_ok_test(Config) ->
    {create_invoice_ok_test, #{
        invoice_id := InvoiceID
    }} = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, Body} = capi_client_invoices:create_invoice_access_token(Context, InvoiceID),
    #{<<"payload">> := TokenPayload} = Body,
    InvoiceContext = get_context(TokenPayload),
    {save_config, #{
        invoice_id      => InvoiceID,
        invoice_context => InvoiceContext
    }}.

-spec get_invoice_by_id_w_access_token_ok_test(config()) -> _.

get_invoice_by_id_w_access_token_ok_test(Config) ->
    {create_invoice_access_token_ok_test,
        #{invoice_id := InvoiceID, invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {ok, _Body} = capi_client_invoices:get_invoice_by_id(Context, InvoiceID),
    {save_config, Info}.

-spec get_random_invoice_w_access_token_failed_test(config()) -> _.

get_random_invoice_w_access_token_failed_test(Config) ->
    {get_invoice_by_id_w_access_token_ok_test,
        #{invoice_id := InvoiceID, invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {error, _} = capi_client_invoices:get_invoice_by_id(Context, <<InvoiceID/binary, "BLARG">>),
    {save_config, Info}.

-spec rescind_invoice_w_access_token_failed_test(config()) -> _.

rescind_invoice_w_access_token_failed_test(Config) ->
    {get_random_invoice_w_access_token_failed_test,
        #{invoice_id := InvoiceID, invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {error, _} = capi_client_invoices:rescind_invoice(Context, InvoiceID, <<"pwnd">>),
    {save_config, Info}.

-spec create_payment_tool_token_w_access_token_ok_test(config()) -> _.

create_payment_tool_token_w_access_token_ok_test(Config) ->
    {rescind_invoice_w_access_token_failed_test,
        #{invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {ok, Token, Session} = default_tokenize_card(Context, Config),
    {save_config, Info#{
        token => Token,
        session => Session
    }}.

-spec create_payment_ok_w_access_token_test(config()) -> _.

create_payment_ok_w_access_token_test(Config) ->
    {create_payment_tool_token_w_access_token_ok_test, Info = #{
        session         := PaymentSession,
        token           := PaymentToolToken,
        invoice_id      := InvoiceID,
        invoice_context := Context
    }} = ?config(saved_config, Config),
    #{<<"id">> := PaymentID} = default_create_payment(
        InvoiceID,
        PaymentSession,
        PaymentToolToken,
        Context
    ),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"InvoiceStatusChanged">>,
            <<"status">> => <<"paid">>
        },
        3000,
        Context
    ),
    {save_config, Info#{
        payment_id => PaymentID
    }}.

-spec fulfill_invoice_ok_test(config()) -> _.

fulfill_invoice_ok_test(Config) ->
    {get_payment_by_id_ok_test, #{
        invoice_id := InvoiceID,
        invoice_context := Context
    } = Info} = ?config(saved_config, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"InvoiceStatusChanged">>,
            <<"status">> => <<"paid">>
        },
        3000,
        Context
    ),
    ok = default_fulfill_invoice(InvoiceID, Config),
    {save_config, Info}.

-spec rescind_invoice_ok_test(config()) -> _.

rescind_invoice_ok_test(Config) ->
    {get_invoice_by_id_ok_test, #{
        invoice_id := InvoiceID
    }} = ?config(saved_config, Config),
    ok = default_rescind_invoice(InvoiceID, Config).

-spec create_invoice_template_badard_test(config()) -> _.

create_invoice_template_badard_test(Config) ->
    Context = ?config(context, Config),
    Req = #{},
    {error, _} = capi_client_invoice_templates:create(Context, Req).

-spec create_invoice_template_no_shop_test(config()) -> _.

create_invoice_template_no_shop_test(Config) ->
    Context = ?config(context, Config),
    Req = #{
        <<"shopID">> => <<"INVALID_SHOP_ID">>,
        <<"cost">> => default_invoice_tpl_cost(fixed),
        <<"lifetime">> => get_lifetime(),
        <<"product">> => ?DEFAULT_TPL_PRODUCT,
        <<"description">> => ?DEFAULT_TPL_DESCRIPTION,
        <<"metadata">> => ?DEFAULT_TPL_META
    },
    {error, Resp} = capi_client_invoice_templates:create(Context, Req),
    #{
        <<"code">> := <<"invalidShopID">>
    } = jsx:decode(Resp, [return_maps]).

-spec create_invoice_with_template_invalid_id_test(config()) -> _.

create_invoice_with_template_invalid_id_test(Config) ->
    #{
        <<"invoiceTemplate">> := #{<<"id">> := _},
        <<"invoiceTemplateAccessToken">> := #{<<"payload">> := TokenPayload}
    } = default_create_invoice_tpl(Config),
    TplContext = get_context(TokenPayload),
    InvoiceTplID = <<"INVALID_INVOICE_TEMPLATE_ID">>,
    {error, _} = capi_client_invoice_templates:create_invoice(TplContext, InvoiceTplID, #{}).

-spec create_invoice_template_0_lifetime_test(config()) -> _.

create_invoice_template_0_lifetime_test(Config) ->
    Context = ?config(context, Config),
    ShopID = create_and_activate_shop(Config),
    Req = #{
        <<"shopID">> => ShopID,
        <<"cost">> => default_invoice_tpl_cost(fixed),
        <<"lifetime">> => get_lifetime(0, 0, 0),
        <<"product">> => ?DEFAULT_TPL_PRODUCT,
        <<"description">> => ?DEFAULT_TPL_DESCRIPTION,
        <<"metadata">> => ?DEFAULT_TPL_META
    },
    {error, Resp} = capi_client_invoice_templates:create(Context, Req),
    #{
        <<"code">> := <<"invalidRequest">>
    } = jsx:decode(Resp, [return_maps]).

-spec create_invoice_template_ok_test(config()) -> _.

create_invoice_template_ok_test(Config) ->
    #{
        <<"invoiceTemplate">> := #{<<"id">> := InvoiceTplID},
        <<"invoiceTemplateAccessToken">> := #{<<"payload">> := TokenPayload}
    } = default_create_invoice_tpl(Config),
    TplContext = get_context(TokenPayload),
    {save_config, #{
        invoice_tpl_id  => InvoiceTplID,
        invoice_tpl_context => TplContext
    }}.

-spec get_invoice_template_by_id_ok_test(config()) -> _.

get_invoice_template_by_id_ok_test(Config) ->
    {create_invoice_template_ok_test,
        #{invoice_tpl_id := InvoiceTplID, invoice_tpl_context := TplContext} = Info
    } = ?config(saved_config, Config),
    {ok, _Body} = capi_client_invoice_templates:get_template_by_id(TplContext, InvoiceTplID),
    {save_config, Info}.

-spec create_invoice_with_template_ok_test(config()) -> _.

create_invoice_with_template_ok_test(Config) ->
    {get_invoice_template_by_id_ok_test,
        #{invoice_tpl_id := InvoiceTplID, invoice_tpl_context := TplContext} = Info
    } = ?config(saved_config, Config),
    #{<<"invoice">> := #{<<"id">> := _InvoiceID}} = default_create_invoice_with_tpl(InvoiceTplID, TplContext),
    {save_config, Info}.

-spec update_invoice_template_ok_test(config()) -> _.

update_invoice_template_ok_test(Config) ->
    {create_invoice_with_template_ok_test,
        #{invoice_tpl_id := InvoiceTplID} = Info
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, InvoiceTpl} = capi_client_invoice_templates:get_template_by_id(Context, InvoiceTplID),

    Req0 = #{<<"cost">> => default_invoice_tpl_cost(unlim)},
    Expect0 = maps:merge(InvoiceTpl, Req0),
    {ok, Expect0} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req0),

    Req1 = #{<<"cost">> => default_invoice_tpl_cost(range)},
    Expect1 = maps:merge(InvoiceTpl, Req1),
    {ok, Expect1} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req1),

    Req2 = #{<<"product">> => <<"rubber duck">>},
    Expect2 = maps:merge(Expect1, Req2),
    {ok, Expect2} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req2),

    Req3 = #{<<"description">> => <<"only best rubber">>},
    Expect3 = maps:merge(Expect2, Req3),
    {ok, Expect3} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req3),

    Req4 = #{
        <<"product">> => <<"degu shampoo">>,
        <<"description">> => <<"fine soft sand for your pet">>
    },
    Expect4 = maps:merge(Expect3, Req4),
    {ok, Expect4} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req4),

    Req5 = #{<<"lifetime">> => get_lifetime(0, 1, 0)},
    Expect5 = maps:merge(Expect4, Req5),
    {ok, Expect5} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req5),

    Req6 = #{<<"metadata">> => #{<<"1">> => <<"2">>}},
    Expect6 = maps:merge(Expect5, Req6),
    {ok, Expect6} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req6),

    {save_config, Info}.

-spec update_invoice_template_0_lifetime_test(config()) -> _.

update_invoice_template_0_lifetime_test(Config) ->
    {update_invoice_template_ok_test,
        #{invoice_tpl_id := InvoiceTplID} = Info
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    Req = #{<<"lifetime">> => get_lifetime(0, 0, 0)},
    {error, Resp} = capi_client_invoice_templates:update(Context, InvoiceTplID, Req),
    #{
        <<"code">> := <<"invalidRequest">>
    } = jsx:decode(Resp, [return_maps]),
    {save_config, Info}.

-spec delete_invoice_template_ok_test(config()) -> _.

delete_invoice_template_ok_test(Config) ->
    {update_invoice_template_0_lifetime_test,
        #{invoice_tpl_id := InvoiceTplID} = Info
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    ok = capi_client_invoice_templates:delete(Context, InvoiceTplID),
    {save_config, Info}.

-spec create_invoice_with_template_removed_template_test(config()) -> _.

create_invoice_with_template_removed_template_test(Config) ->
    {delete_invoice_template_ok_test,
        #{invoice_tpl_id := InvoiceTplID, invoice_tpl_context := TplContext}
    } = ?config(saved_config, Config),
    {error, _} = capi_client_invoice_templates:create_invoice(TplContext, InvoiceTplID, #{}).

-spec create_payment_ok_test(config()) -> _.

create_payment_ok_test(Config) ->
    Info = case ?config(saved_config, Config) of
        {create_payment_tool_token_ok_test, Inf} ->
            Inf;
        {create_payment_terminal_tool_token, Inf} ->
            Inf;
        _ ->
            error(no_config_for_payment)
    end,
    #{
        session := PaymentSession,
        payment_tool_token := PaymentToolToken,
        invoice_context := Context,
        invoice_id := InvoiceID
    } = Info,
    #{<<"id">> := PaymentID} = default_create_payment(
        InvoiceID,
        PaymentSession,
        PaymentToolToken,
        Context
    ),
    {save_config, Info#{payment_id => PaymentID}}.

-spec create_payment_hold_ok_test(config()) -> _.

create_payment_hold_ok_test(Config) ->
    {create_payment_tool_token_ok_test, #{
        session := PaymentSession,
        payment_tool_token := PaymentToolToken,
        invoice_id := InvoiceID
    }} = ?config(saved_config, Config),
    Context = ?config(context, Config),
    #{<<"id">> := PaymentID} = default_create_payment(
        InvoiceID,
        PaymentSession,
        PaymentToolToken,
        Context,
        hold
    ),
    {save_config, #{payment_id => PaymentID, invoice_id => InvoiceID}}.

-spec cancel_payment_ok_test(config()) -> _.

cancel_payment_ok_test(Config) ->
    {create_payment_hold_ok_test,
        #{payment_id := PaymentID, invoice_id := InvoiceID}
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"status">> => <<"processed">>
        },
        3000,
        Context
    ),
    ok = default_cancel_payment(InvoiceID, PaymentID, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"status">> => <<"cancelled">>
        },
        3000,
        Context
    ).

-spec capture_payment_ok_test(config()) -> _.

capture_payment_ok_test(Config) ->
    {create_payment_hold_ok_test,
        #{payment_id := PaymentID, invoice_id := InvoiceID}
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"status">> => <<"processed">>
        },
        3000,
        Context
    ),
    ok = default_capture_payment(InvoiceID, PaymentID, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"status">> => <<"captured">>
        },
        3000,
        Context
    ).

-spec create_payment_tool_token_ok_test(config()) -> _.

create_payment_tool_token_ok_test(Config) ->
    {create_invoice_ok_test, #{
        invoice_context := Context
    } = Info} = ?config(saved_config, Config),
    {ok, Token, Session} = default_tokenize_card(Context, Config),
    Info1 = Info#{
        payment_tool_token => Token,
        session => Session
    },
    {save_config, Info1}.

-spec create_payment_terminal_tool_token(config()) -> _.

create_payment_terminal_tool_token(Config) ->
    {create_invoice_ok_test, #{
        invoice_context := Context
    } = Info} = ?config(saved_config, Config),
    {ok, Token, Session} = default_tokenize_payment_terminal(Context, Config),
    Info1 = Info#{
        payment_tool_token => Token,
        session => Session
    },
    {save_config, Info1}.

-spec get_invoice_by_id_ok_test(config()) -> _.

get_invoice_by_id_ok_test(Config) ->
    {create_invoice_ok_test, #{
        invoice_id := InvoiceID,
        invoice_context := Context
    } = Info} = ?config(saved_config, Config),
    {ok, _Body} = capi_client_invoices:get_invoice_by_id(Context, InvoiceID),
    {save_config, Info}.

-spec get_invoice_events_ok_test(config()) -> _.

get_invoice_events_ok_test(Config) ->
    {fulfill_invoice_ok_test, #{
        invoice_id := InvoiceID,
        invoice_context := Context
    }} = ?config(saved_config, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"InvoiceStatusChanged">>,
            <<"status">> => <<"fulfilled">>
        },
        3000,
        Context
    ),
    {ok, Events} = capi_client_invoices:get_invoice_events(Context, InvoiceID, 10),
    [
        #{
            <<"changes">> := [#{
                <<"changeType">> := <<"InvoiceCreated">>,
                <<"invoice">> := #{<<"id">> := InvoiceID}
            }]
        },
        #{
            <<"changes">> := [#{
                <<"changeType">> := <<"PaymentStarted">>,
                <<"payment">> := #{
                    <<"id">> := PaymentID,
                    <<"invoiceID">> := InvoiceID
                }
            }]
        },
        #{
            <<"changes">> := [#{
                <<"changeType">> := <<"PaymentStatusChanged">>,
                <<"status">> := <<"processed">>,
                <<"paymentID">> := PaymentID
            }]
        },
        #{
            <<"changes">> := [
                #{
                    <<"changeType">> := <<"PaymentStatusChanged">>,
                    <<"status">> := <<"captured">>,
                    <<"paymentID">> := PaymentID
                },
                #{
                    <<"changeType">> := <<"InvoiceStatusChanged">>,
                    <<"status">> := <<"paid">>
                }
            ]
        },
        #{
            <<"changes">> := [#{
                <<"changeType">> := <<"InvoiceStatusChanged">>,
                <<"status">> := <<"fulfilled">>
            }]
        }
    ] = Events.

-spec get_payments_ok_test(config()) -> _.

get_payments_ok_test(Config) ->
    {create_payment_ok_test, #{
        invoice_id := InvoiceID,
        invoice_context := Context,
        payment_id := PaymentID
    } = Info} = ?config(saved_config, Config),
    {ok, [#{<<"id">> := PaymentID}]} = capi_client_payments:get_payments(Context, InvoiceID),
    {save_config, Info}.

-spec get_payment_by_id_ok_test(config()) -> _.

get_payment_by_id_ok_test(Config) ->
    {get_payments_ok_test, #{
        payment_id := PaymentID,
        invoice_id := InvoiceID,
        invoice_context := Context
    } = Info} = ?config(saved_config, Config),
    {ok, _Body} = capi_client_payments:get_payment_by_id(Context, InvoiceID, PaymentID),
    {save_config, Info}.

-spec create_refund(config()) -> _.

create_refund(Config) ->
    {create_payment_ok_test, #{
        invoice_id := InvoiceID,
        invoice_context := Context,
        payment_id := PaymentID
    } = Info} = ?config(saved_config, Config),
    Reason = <<"CUZ I SAY SO!!!">>,
    {error, _} = capi_client_payments:create_refund(Context, <<"NOT_INVOICE_ID">>, PaymentID, Reason),
    {error, _} = capi_client_payments:create_refund(Context, InvoiceID, <<"NOT_PAYMENT_ID">>, Reason),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"status">> => <<"captured">>
        },
        3000,
        Context
    ),
    {ok, #{<<"id">> := RefundID}} = capi_client_payments:create_refund(Context, InvoiceID, PaymentID, Reason),
    {save_config, Info#{refund_id => RefundID}}.

-spec get_refund_by_id(config()) -> _.

get_refund_by_id(Config) ->
    {create_refund, #{
        invoice_id := InvoiceID,
        invoice_context := Context,
        payment_id := PaymentID,
        refund_id := RefundID
    } = Info} = ?config(saved_config, Config),
    {error, _} = capi_client_payments:get_refund_by_id(Context, <<"NOT_INVOICE_ID">>, PaymentID, RefundID),
    {error, _} = capi_client_payments:get_refund_by_id(Context, InvoiceID, <<"NOT_PAYMENT_ID">>, RefundID),
    {error, _} = capi_client_payments:get_refund_by_id(Context, InvoiceID, PaymentID, <<"NOT_REFUND_ID">>),
    {ok, _Refund} = capi_client_payments:get_refund_by_id(Context, InvoiceID, PaymentID, RefundID),
    {save_config, Info}.

-spec get_refunds(config()) -> _.

get_refunds(Config) ->
    {get_refund_by_id, #{
        invoice_id := InvoiceID,
        invoice_context := Context,
        payment_id := PaymentID,
        refund_id := RefundID
    } = Info} = ?config(saved_config, Config),
    {error, _} = capi_client_payments:get_refunds(Context, <<"NOT_INVOICE_ID">>, PaymentID),
    {error, _} = capi_client_payments:get_refunds(Context, InvoiceID, <<"NOT_PAYMENT_ID">>),
    {ok, [#{<<"id">> := RefundID} = Refund]} = capi_client_payments:get_refunds(Context, InvoiceID, PaymentID),
    {save_config, Info#{refund => Refund}}.

-spec get_refund_events(config()) -> _.

get_refund_events(Config) ->
    {get_refunds, #{
        invoice_id := InvoiceID,
        invoice_context := Context,
        payment_id := PaymentID,
        refund_id := RefundID,
        refund := Refund
    }} = ?config(saved_config, Config),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"RefundStarted">>,
            <<"paymentID">> => PaymentID,
            <<"refund">> => Refund
        },
        3000,
        Context
    ),
    wait_event_w_change(
        InvoiceID,
        #{
            <<"changeType">> => <<"RefundStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"refundID">> => RefundID,
            <<"status">> => <<"succeeded">>
        },
        3000,
        Context
    ).

-spec search_invoices_ok_test(config()) -> _.

search_invoices_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
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
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerEmail, <<"test@test_rbk.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        %%{cardNumberMask, <<"2222">>},  %%@FIXME cannot be used until getting the newest api client
        {paymentAmount, 10000}
    ],

    {ok, _, _} = capi_client_searches:search_invoices(Context, ShopID, Query).

-spec search_payments_ok_test(config()) -> _.

search_payments_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.0.1">>},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerEmail, <<"test@test_rbk.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        %%{cardNumberMask, <<"2222">>}, %%@FIXME cannot be used until getting the newest api client
        {paymentAmount, 10000}
    ],

    {ok, _, _} = capi_client_searches:search_payments(Context, ShopID, Query).

-spec get_payment_conversion_stats_ok_test(config()) -> _.

get_payment_conversion_stats_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _Body} = capi_client_analytics:get_payment_conversion_stats(Context, ShopID, Query).

-spec get_payment_revenue_stats_ok_test(config()) -> _.

get_payment_revenue_stats_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _Body} = capi_client_analytics:get_payment_revenue_stats(Context, ShopID, Query).

-spec get_payment_geo_stats_ok_test(config()) -> _.

get_payment_geo_stats_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _Body} = capi_client_analytics:get_payment_geo_stats(Context, ShopID, Query).

-spec get_payment_rate_stats_ok_test(config()) -> _.

get_payment_rate_stats_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1}
    ],
    {ok, _Body} = capi_client_analytics:get_payment_rate_stats(Context, ShopID, Query).


-spec get_payment_method_stats_ok_test(config()) -> _.

get_payment_method_stats_ok_test(Config) ->
    Context = ?config(context, Config),
    ShopID = ?config(shop_id, Config),
    Query = [
        {limit, 2},
        {offset, 0},
        {from_time, {{2015, 08, 11},{19, 42, 35}}},
        {to_time, {{2020, 08, 11},{19, 42, 35}}},
        {split_unit, minute},
        {split_size, 1},
        {paymentMethod, <<"bankCard">>}
    ],
    {ok, _Body} = capi_client_analytics:get_payment_method_stats(Context, ShopID, Query).

-spec get_my_party_ok_test(config()) -> _.

get_my_party_ok_test(Config) ->
    #{
        <<"isBlocked">> := _,
        <<"isSuspended">> := _,
        <<"id">> := ?MERCHANT_ID
    } = default_get_party(Config).

-spec suspend_my_party_idempotent_ok_test(config()) -> _.

suspend_my_party_idempotent_ok_test(Config) ->
    ok = default_suspend_my_party(Config),
    ok = default_suspend_my_party(Config),
    #{
        <<"isSuspended">> := true
    } = default_get_party(Config),
    Config.

-spec activate_my_party_idempotent_ok_test(config()) -> _.

activate_my_party_idempotent_ok_test(Config) ->
    ok = default_activate_my_party(Config),
    ok = default_activate_my_party(Config),
    #{
        <<"isSuspended">> := false
    } = default_get_party(Config).

-spec create_contract_ok_test(config()) -> _.

create_contract_ok_test(Config) ->
    ContractID = generate_contract_id(),
    Claim = default_create_contract(ContractID, Config),
    #{<<"changeset">> := [#{
        <<"partyModificationType">> := <<"ContractModification">>,
        <<"contractModificationType">> := <<"ContractCreation">>,
        <<"contractID">> := ContractID
    } | _]} = Claim,
    {ok, _} = default_approve_claim(Claim),
    {save_config, ContractID}.

-spec get_contract_by_id_ok_test(config()) -> _.

get_contract_by_id_ok_test(Config) ->
    {create_contract_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, #{
        <<"id">> := ContractID
    }} = capi_client_contracts:get_contract_by_id(Context, ContractID),
    {save_config, ContractID}.

-spec create_payout_tool_ok_test(config()) -> _.

create_payout_tool_ok_test(Config) ->
    {get_contract_by_id_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    PayoutToolID  = generate_payout_tool_id(),
    Claim = default_create_payout_tool(PayoutToolID, ContractID, Config),
    {ok, _} = default_approve_claim(Claim),
    {save_config, ContractID}.

-spec get_payout_tools_ok_test(config()) -> _.

get_payout_tools_ok_test(Config) ->
    {create_payout_tool_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, [#{
        <<"id">> := _PayoutToolID
    } | _]} = capi_client_payouts:get_payout_tools(Context, ContractID),
    {save_config, ContractID}.

-spec get_contracts_ok_test(config()) -> _.

get_contracts_ok_test(Config) ->
    {get_payout_tools_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, Contracts} = capi_client_contracts:get_contracts(Context),
    lists:member(
        ContractID,
        [C || #{ <<"id">> := C} <- Contracts]
    ) =:= true.

-spec get_claim_by_id_ok_test(config()) -> _.

get_claim_by_id_ok_test(Config) ->
    #{<<"id">> := ClaimID} = default_create_shop(generate_shop_id(), ?LIVE_CATEGORY_ID, Config),
    #{
        <<"id">> := ClaimID
    } = default_get_claim_by_id(ClaimID, Config),
    {save_config, ClaimID}.

-spec get_claims_by_status_ok_test(config()) -> _.

get_claims_by_status_ok_test(Config) ->
    {get_claim_by_id_ok_test,
        ClaimID
    } = ?config(saved_config, Config),
    Claims = default_get_claims_by_status(pending, Config),
    lists:any(
        fun (#{
                <<"id">> := ID,
                <<"status">> := <<"ClaimPending">>
            }) when ClaimID =:= ID ->
                true;
            (_) ->
                false
        end,
        Claims
    ),
    {save_config, ClaimID}.

-spec revoke_claim_ok_test(config()) -> _.

revoke_claim_ok_test(Config) ->
    {get_claims_by_status_ok_test,
        ClaimID
    } = ?config(saved_config, Config),
    ok = default_revoke_claim(ClaimID, Config),
    #{
        <<"id">> := ClaimID,
        <<"status">> := <<"ClaimRevoked">>
    } = default_get_claim_by_id(ClaimID, Config),
    Config.

-spec get_categories_ok_test(config()) -> _.

get_categories_ok_test(Config) ->
    [Category | _] = default_get_categories(Config),
    #{
        <<"name">> := _Name,
        <<"categoryID">> := CategoryID
    } = Category,
    {save_config, CategoryID}.

-spec get_category_by_id_ok_test(config()) -> _.

get_category_by_id_ok_test(Config) ->
    {get_categories_ok_test,
        CategoryID
    } = ?config(saved_config, Config),
    _ = default_get_category_by_id(CategoryID, Config),
    {save_config, CategoryID}.

-spec create_shop_ok_test(config()) -> _.

create_shop_ok_test(Config) ->
    {get_category_by_id_ok_test,
        CategoryID
    } = ?config(saved_config, Config),
    ShopID = generate_shop_id(),
    #{<<"id">> := ClaimID} = Claim = default_create_shop(ShopID, CategoryID, Config),
    {ok, _} = default_approve_claim(Claim),
    #{
        <<"id">> := ClaimID,
        <<"status">> := <<"ClaimAccepted">>,
        <<"changeset">> := [
            #{
                <<"partyModificationType">> := <<"ShopModification">>,
                <<"shopID">> := ShopID,
                <<"shopModificationType">> := <<"ShopCreation">>
            } | _
        ]
    } = default_get_claim_by_id(ClaimID, Config),
    {save_config, ShopID}.

-spec get_shop_by_id_ok_test(config()) -> _.

get_shop_by_id_ok_test(Config) ->
    {create_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    #{
        <<"id">> := ShopID
    } = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.

-spec get_shops_ok_test(config()) -> _.

get_shops_ok_test(Config) ->
    Context = ?config(context, Config),
    {ok, [
        #{
            <<"id">> := ShopID
        }
    | _]} = capi_client_shops:get_shops(Context),
    {save_config, ShopID}.


-spec update_shop_ok_test(config()) -> _.

update_shop_ok_test(Config) ->
    {activate_shop_idempotent_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    NewLocation = #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => <<"kill.me">>
    },
    Changeset = [#{
        <<"partyModificationType">> => <<"ShopModification">>,
        <<"shopID">> => ShopID,
        <<"shopModificationType">> => <<"ShopLocationChange">>,
        <<"location">> => NewLocation
    }],
    Claim = create_claim(Config, Changeset),
    {ok, _} = default_approve_claim(Claim),
    #{<<"location">> := NewLocation} = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.

-spec suspend_shop_ok_test(config()) -> _.

suspend_shop_ok_test(Config) ->
    {update_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    ok = default_suspend_shop(ShopID, Config),
    ok = default_suspend_shop(ShopID, Config),
    #{
        <<"isSuspended">> := true
    } = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.


-spec activate_shop_idempotent_ok_test(config()) -> _.

activate_shop_idempotent_ok_test(Config) ->
    {get_shops_ok_test,
        ShopID
    } = ?config(saved_config, Config),

    ok = default_activate_shop(ShopID, Config),
    ok = default_activate_shop(ShopID, Config),
    #{
        <<"isSuspended">> := false
    } = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.

-spec get_account_by_id_ok_test(config()) -> _.

get_account_by_id_ok_test(Config) ->
    {create_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    #{
        <<"account">> := #{
            <<"guaranteeID">> := GuaranteeID
        }
    } = default_get_shop_by_id(ShopID, Config),
    #{
        <<"id">> := _,
        <<"ownAmount">> := _,
        <<"availableAmount">> := _,
        <<"currency">> := _

    %% @FIXME changin Account ID to string is not ok
    } = default_get_shop_account_by_id(GuaranteeID, Config).

-spec create_webhook_error_test(config()) -> _.
create_webhook_error_test(Config) ->
    Context = ?config(context, Config),
    ShopID = -1, % nonexistent
    {error, _} = capi_client_webhooks:create_webhook(Context, #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => construct_invoices_scope(ShopID)
    }).

-spec create_webhook_receive_events_test(config()) -> _.
create_webhook_receive_events_test(Config) ->
    Context = ?config(context, Config),
    % % list is empty?
    % [] = capi_client_webhooks:get_webhooks(Context),
    % create successful?
    {ok, Shops} = capi_client_shops:get_shops(Context),
    Shop = get_latest(Shops),
    ShopID = maps:get(<<"id">>, Shop),
    WebhookParams = #{
        <<"url">>   => <<"http://localhost:8080/TODO">>,
        <<"scope">> => construct_invoices_scope(ShopID, ['InvoiceCancelled'])
    },
    {ok, Webhook = #{<<"id">> := WebhookID}} = capi_client_webhooks:create_webhook(Context, WebhookParams),
    {ok, Webhook} = capi_client_webhooks:get_webhook_by_id(Context, WebhookID),
    % list is not empty then?
    true = lists:member(Webhook, capi_client_webhooks:get_webhooks(Context)),
    % delete succeeded idempotently?
    ok = capi_client_webhooks:delete_webhook_by_id(Context, WebhookID),
    ok = capi_client_webhooks:delete_webhook_by_id(Context, WebhookID),
    [] = [W || #{<<"id">> := W} <- capi_client_webhooks:get_webhooks(Context), W =:= WebhookID],
    ok.

construct_invoices_scope(ShopID) ->
    construct_invoices_scope(ShopID, []).

construct_invoices_scope(ShopID, EventTypes) ->
    #{
        <<"topic">> => <<"InvoicesTopic">>,
        <<"shopID">> => ShopID,
        <<"eventTypes">> => lists:map(fun genlib:to_binary/1, EventTypes)
    }.

-spec get_locations_names_ok_test(config()) -> _.
get_locations_names_ok_test(Config) ->
    {TestGeoID, TestName} = {53654, <<"Могадишо"/utf8>>},
    [#{
        <<"geoID">> := TestGeoID,
        <<"name">> := TestName
    }] = get_locations_names([TestGeoID], <<"ru">>, Config).

-spec get_reports(config()) -> _.
get_reports(Config) ->
    Context = ?config(context, Config),
    {FromTime, ToTime} = get_reports_interval(),
    {ok, []} = capi_client_reports:get_reports(Context, <<"NO_SUCH_SHOP">>, FromTime, ToTime),
    {ok, Shops} = capi_client_shops:get_shops(Context),
    ShopID = maps:get(<<"id">>, get_latest(Shops)),
    {error, _} = capi_client_reports:get_reports(Context, ShopID, ToTime, FromTime),
    {ok, _} = capi_client_reports:get_reports(Context, ShopID, FromTime, ToTime).

-spec download_report_file(config()) -> _.
download_report_file(Config) ->
    Context = ?config(context, Config),
    {FromTime, ToTime} = get_reports_interval(),
    {ok, Shops} = capi_client_shops:get_shops(Context),
    ShopID = maps:get(<<"id">>, get_latest(Shops)),
    ReportID = default_generate_report(?MERCHANT_ID, ShopID),
    #{
        <<"files">> := [#{<<"id">> := FileID} | _]
    } = wait_report_w_id(Context, ShopID, FromTime, ToTime, ReportID),
    {ok, {redirect, _URL}} = capi_client_reports:download_file(Context, ShopID, ReportID, FileID).

%% helpers
call(Method, Path, Body, Headers) ->
    Url = get_url(Path),
    PreparedBody = jsx:encode(Body),
    {ok, Code, RespHeaders, ClientRef} = hackney:request(Method, Url, Headers, PreparedBody),
    {ok, Code, RespHeaders, get_body(ClientRef)}.

get_url(Path) ->
    ?CAPI_URL ++ Path.

auth_header(Token) ->
    {<<"Authorization">>, <<"Bearer ", Token/binary>>} .

content_type_header() ->
    {<<"Content-Type">>, <<"application/json; charset=utf-8">>}.

req_id_header() ->
    req_id_header(genlib:unique()).

req_id_header(ReqID) ->
    {<<"X-Request-ID">>, genlib:to_binary(ReqID)}.

auth_token(ACL, Expiration) ->
    Auth = {{?MERCHANT_ID, ACL}, #{}},
    capi_authorizer_jwt:issue(Auth, Expiration).

default_create_invoice(Config) ->
    ShopID = create_and_activate_shop(Config),
    Req = #{
        <<"shopID">> => ShopID,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>,
        <<"metadata">> => ?DEFAULT_META,
        <<"dueDate">> => get_due_date(),
        <<"product">> => ?DEFAULT_PRODUCT,
        <<"description">> => ?DEFAULT_DESCRIPTION
    },
    Context = ?config(context, Config),
    {ok, Body} = capi_client_invoices:create_invoice(Context, Req),
    Body.

default_create_invoice_tpl(Config) ->
    ShopID = create_and_activate_shop(Config),
    Req = #{
        <<"shopID">> => ShopID,
        <<"cost">> => default_invoice_tpl_cost(fixed),
        <<"lifetime">> => get_lifetime(),
        <<"product">> => ?DEFAULT_TPL_PRODUCT,
        <<"description">> => ?DEFAULT_TPL_DESCRIPTION,
        <<"metadata">> => ?DEFAULT_TPL_META
    },
    Context = ?config(context, Config),
    {ok, Body} = capi_client_invoice_templates:create(Context, Req),
    Body.

default_create_invoice_with_tpl(InvoiceTplID, Context) ->
    Req = #{},
    {ok, Body} = capi_client_invoice_templates:create_invoice(Context, InvoiceTplID, Req),
    Body.

default_invoice_tpl_cost(unlim) ->
    #{
       <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostUnlim">>
    };
default_invoice_tpl_cost(fixed) ->
    #{
       <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostFixed">>,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>
    };
default_invoice_tpl_cost(range) ->
    #{
        <<"invoiceTemplateCostType">> => <<"InvoiceTemplateCostRange">>,
        <<"currency">> => <<"RUB">>,
        <<"range">> => #{
            <<"upperBound">> => 100000,
            <<"lowerBound">> => 100
        }
    }.

default_create_contract(ContractID, Config) ->
    BankAccount = default_bank_account(),
    Changeset = [
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ContractID,
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
                <<"bankAccount">> => BankAccount
            }
        },
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ContractID,
            <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
            <<"payoutToolID">> => generate_payout_tool_id(),
            <<"currency">> => <<"RUB">>,
            <<"details">> => #{
                <<"type">> => <<"PayoutToolBankAccount">>,
                <<"bankAccount">> => BankAccount
            }
        }
    ],
    create_claim(Config, Changeset).

create_claim(Config, Changeset) ->
    Context = ?config(context, Config),
    {ok, Claim} = capi_client_claims:create_claim(Context, Changeset),
    Claim.

get_latest(Es) ->
    % Assuming the latest element will have highest numeric ID
    % or latest creation time
    hd(lists:sort(
        fun (#{<<"createdAt">> := T1}, #{<<"createdAt">> := T2}) ->
                compare_timestamps(T1, T2);
            (#{<<"id">> := ID1}, #{<<"id">> := ID2}) ->
                ID1 > ID2
        end,
        Es
    )).

compare_timestamps(T1, T2) ->
    rfc3339:parse_to_local_datetime(T1) > rfc3339:parse_to_local_datetime(T2).

default_create_payout_tool(PayoutToolID, ContractID, Config) ->
    Changeset = [
        #{
            <<"partyModificationType">> => <<"ContractModification">>,
            <<"contractID">> => ContractID,
            <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
            <<"payoutToolID">> => PayoutToolID,
            <<"currency">> => <<"RUB">>,
            <<"details">> => #{
                <<"type">> => <<"PayoutToolBankAccount">>,
                <<"bankAccount">> => default_bank_account()
            }
        }
    ],
    create_claim(Config, Changeset).

default_bank_account() ->
    #{
        <<"account">> => <<"12345678901234567890">>,
        <<"bankName">> => <<"testBankName">>,
        <<"bankPostAccount">> => <<"12345678901234567890">>,
        <<"bankBik">> => <<"123456789">>
    }.

get_context(Token) ->
    get_context(Token, 10, 5000).

get_context(Token, Retries, Timeout) ->
    capi_client_lib:get_context(?CAPI_URL, Token, Retries, Timeout, ?PROTOCOL).

default_tokenize_card(Context, _Config) ->
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
    capi_client_tokens:create_payment_tool_token(Context, Req).

default_tokenize_payment_terminal(Context, _Config) ->
    Req = #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"PaymentTerminalData">>,
            <<"provider">> => <<"euroset">>
        },
        <<"clientInfo">> => #{
            <<"fingerprint">> => <<"test fingerprint">>
        }
    },
    capi_client_tokens:create_payment_tool_token(Context, Req).

default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Context) ->
    default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Context, instant).

default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Context, FlowType) ->
    Flow = case FlowType of
        instant ->
            #{<<"type">> => <<"PaymentFlowInstant">>};
        hold ->
            #{<<"type">> => <<"PaymentFlowHold">>, <<"onHoldExpiration">> => <<"cancel">>}
    end,
    Req = #{
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => PaymentToolToken,
        <<"flow">> => Flow,
        <<"contactInfo">> => #{
            <<"email">> => <<"bla@bla.ru">>
        }
    },
    {ok, Body} = capi_client_payments:create_payment(Context, Req, InvoiceID),
    Body.

default_get_party(Config) ->
    Context = ?config(context, Config),
    {ok, Body} = capi_client_parties:get_my_party(Context),
    Body.

default_get_claim_by_id(ClaimID, Config) ->
    Context = ?config(context, Config),
    {ok, Body} = capi_client_claims:get_claim_by_id(Context, ClaimID),
    Body.

default_get_claims_by_status(Status, Config) ->
    Context = ?config(context, Config),
    {ok, Body} = capi_client_claims:get_claims_by_status(Context, Status),
    Body.

default_suspend_my_party(Config) ->
    Context = ?config(context, Config),
    Context = ?config(context, Config),
    capi_client_parties:suspend_my_party(Context).

default_activate_my_party(Config) ->
    Context = ?config(context, Config),
    capi_client_parties:activate_my_party(Context).

default_suspend_shop(ShopID, Config) ->
    Context = ?config(context, Config),
    capi_client_shops:suspend_shop(Context, ShopID).

default_activate_shop(ShopID, Config) ->
    Context = ?config(context, Config),
    capi_client_shops:activate_shop(Context,ShopID).

default_get_shop_by_id(ShopID, Config) ->
    Context = ?config(context, Config),
    {ok, R} = capi_client_shops:get_shop_by_id(Context, ShopID),
    R.

default_create_shop(ShopID, CategoryID, Config) ->
    Context = ?config(context, Config),
    ContractID = generate_contract_id(),
    Claim = default_create_contract(ContractID, Config),
    {ok, _} = default_approve_claim(Claim),
    {ok, PayoutTools} = capi_client_payouts:get_payout_tools(Context, ContractID),
    #{<<"id">> := PayoutToolID} = lists:last(PayoutTools),
    default_create_shop(ShopID, CategoryID, ContractID, PayoutToolID, Config).


default_create_shop(ShopID, CategoryID, ContractID, PayoutToolID, Config) ->
    Changeset = [
        genlib_map:compact(#{
            <<"partyModificationType">> => <<"ShopModification">>,
            <<"shopID">> => ShopID,
            <<"shopModificationType">> => <<"ShopCreation">>,
            <<"location">> => #{
                <<"locationType">> => <<"ShopLocationUrl">>,
                <<"url">> => <<"http://default.html">>
            },
            <<"details">> => #{
                <<"name">> => <<"OOOBlackMaster">>,
                <<"description">> => <<"Goods for education">>
            },
            <<"contractID">> => ContractID,
            <<"payoutToolID">> => PayoutToolID
        }),
        #{
            <<"partyModificationType">> => <<"ShopModification">>,
            <<"shopID">> => ShopID,
            <<"shopModificationType">> => <<"ShopCategoryChange">>,
            <<"categoryID">> => CategoryID
        }
    ],
    create_claim(Config, Changeset).

default_get_categories(Config) ->
    Context = ?config(context, Config),
    {ok, Body} = capi_client_categories:get_categories(Context),
    Body.

default_get_category_by_id(CategoryID, Config) ->
    Context = ?config(context, Config),
    {ok, Body} = capi_client_categories:get_category_by_ref(Context,  CategoryID),
    Body.

default_revoke_claim(ClaimID, Config) ->
    Context = ?config(context, Config),
    {ok, #{
        <<"id">> := ClaimID,
        <<"revision">> := ClaimRevision
    }} = capi_client_claims:get_claim_by_id(Context, ClaimID),
    Reason = "me want dat",
    capi_client_claims:revoke_claim_by_id(Context, Reason, ClaimID, ClaimRevision).

default_fulfill_invoice(InvoiceID, Config) ->
    Context = ?config(context, Config),
    Reason = "me want dat",
    capi_client_invoices:fulfill_invoice(Context,  InvoiceID, Reason).

default_rescind_invoice(InvoiceID, Config) ->
    Context = ?config(context, Config),
    Reason = "me want dat",
    capi_client_invoices:rescind_invoice(Context,  InvoiceID, Reason).

default_cancel_payment(InvoiceID, PaymentID, Config) ->
    Context = ?config(context, Config),
    Reason = "want to cancel",
    capi_client_payments:cancel_payment(Context, InvoiceID, PaymentID, Reason).

default_capture_payment(InvoiceID, PaymentID, Config) ->
    Context = ?config(context, Config),
    Reason = "want to capture",
    capi_client_payments:capture_payment(Context, InvoiceID, PaymentID, Reason).

get_locations_names(GeoIDs, Lang, Config) ->
    Context = ?config(context, Config),
    PreparedGeo = genlib_string:join($,,[genlib:to_binary(I) || I <- GeoIDs]),
    Query = #{
        <<"geoIDs">> => PreparedGeo,
        <<"language">> => Lang
    },
    {ok, R} = capi_client_geo:get_location_names(Context, Query),
    R.

%% @FIXME thats dirty
default_approve_claim(#{<<"id">> := ClaimID, <<"revision">> := ClaimRevision}) ->
    UserInfo = #payproc_UserInfo{
        id = ?MERCHANT_ID,
        type = {internal_user, #payproc_InternalUser{}}
    },
    call_service(
        party_management,
        'AcceptClaim',
        [UserInfo, ?MERCHANT_ID, ClaimID, ClaimRevision]
    ).

%% @FIXME dirty call
default_generate_report(PartyID, ShopID) ->
    ReqCtx = create_context(),
    {FromTime, ToTime} = get_reports_interval(),
    ReportRequest = #reports_ReportRequest{
        party_id = PartyID,
        shop_id = ShopID,
        time_range = #reports_ReportTimeRange{
            from_time = FromTime,
            to_time = ToTime
        }
    },
    {ok, ReportID} = call_service(
        reporting,
        'GenerateReport',
        [ReportRequest, provision_of_service],
        ReqCtx
    ),
    ReportID.

generate_contract_id() ->
    ID = genlib:to_binary(rand:uniform(100000000)),
    <<"CONTRACT_", ID/binary>>.

generate_shop_id() ->
    ID = genlib:to_binary(rand:uniform(100000000)),
    <<"SHOP_", ID/binary>>.

generate_payout_tool_id() ->
    ID = genlib:to_binary(rand:uniform(100000000)),
    <<"PAYOUTTOOL_", ID/binary>>.

-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(tmpl(ID), #domain_ContractTemplateRef{id = ID}).
-define(trms(ID), #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).
-define(trmacc(Cur, Stl),
    #domain_TerminalAccount{currency = ?cur(Cur), settlement = Stl}).

-define(cfpost(A1, A2, V),
    #domain_CashFlowPosting{
        source      = A1,
        destination = A2,
        volume      = V
    }
).

-define(fixed(A),
    {fixed, #domain_CashVolumeFixed{amount = A}}).
-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{parts = #'Rational'{p = P, q = Q}, 'of' = C}}).

-define(cash(Amount, Currency),
    #domain_Cash{amount = Amount, currency = Currency}).

populate_snapshot(Proxies) ->
    ReqCtx = create_context(),
    {ok, #'Snapshot'{version = Version}} = call_service(
        repository,
        'Checkout',
        [{head, #'Head'{}}],
        ReqCtx
    ),
    Ops = [
        {'insert', #'InsertOp'{
            object = O
        }}
        || O <- get_domain_fixture(Proxies)
    ],

    Commit = #'Commit'{
        ops = Ops
    },

    {ok, _Version} = call_service(
        repository,
        'Commit',
        [Version, Commit],
        ReqCtx
    ),
    timer:sleep(5000).

get_domain_fixture(Proxies) ->
    Context = create_context(),
    Accounts = lists:foldl(
        fun ({N, CurrencyCode}, M) ->
            M#{N => create_account(CurrencyCode, Context)}
        end,
        #{},
        [
            {system_settlement       , <<"RUB">>},
            {external_income         , <<"RUB">>},
            {external_outcome        , <<"RUB">>},
            {terminal_1_settlement   , <<"RUB">>},
            {terminal_2_settlement   , <<"RUB">>},
            {terminal_3_settlement   , <<"RUB">>}
        ]
    ),
    TermSetTest = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
            categories = {value, ordsets:from_list([?cat(1)])},
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard),
                ?pmt(payment_terminal, euroset)
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(4200000, ?cur(<<"RUB">>))}
                    }}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, payment_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ordsets:from_list([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {value, #domain_HoldLifetime{seconds = 1}}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ordsets:from_list([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                fees = {value, []}
            }
        }
    },
    TermSetLive = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            categories = {value, ordsets:from_list([?cat(?LIVE_CATEGORY_ID)])}
        }
    },
    Basic = [
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype           = #domain_PartyPrototypeRef{id = 42},
                providers                 = {value, [?prv(2), ?prv(3)]},
                system_account_set        = {value, ?sas(1)},
                external_account_set      = {value, ?eas(1)},
                default_contract_template = ?tmpl(2),
                inspector                 = {value, ?insp(1)},
                common_merchant_proxy     = ?prx(3)
            }
        }},
        {system_account_set, #domain_SystemAccountSetObject{
            ref = ?sas(1),
            data = #domain_SystemAccountSet{
                name = <<"Primaries">>,
                description = <<"Primaries">>,
                accounts = #{
                    ?cur(<<"RUB">>) => #domain_SystemAccount{
                        settlement = maps:get(system_settlement, Accounts)
                    }
                }
            }
        }},
        {external_account_set, #domain_ExternalAccountSetObject{
            ref = ?eas(1),
            data = #domain_ExternalAccountSet{
                name = <<"Primaries">>,
                description = <<"Primaries">>,
                accounts = #{
                    ?cur(<<"RUB">>) => #domain_ExternalAccount{
                        income  = maps:get(external_income , Accounts),
                        outcome = maps:get(external_outcome, Accounts)
                    }
                }
            }
        }},
        {party_prototype, #domain_PartyPrototypeObject{
            ref = #domain_PartyPrototypeRef{id = 42},
            data = #domain_PartyPrototype{
                shop = #domain_ShopPrototype{
                    shop_id = <<"TESTSHOP">>,
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    },
                    location = {url, <<"">>}
                },
                contract = #domain_ContractPrototype{
                    contract_id = <<"TESTCONTRACT">>,
                    test_contract_template = ?tmpl(1),
                    payout_tool = #domain_PayoutToolPrototype{
                        payout_tool_id = <<"TESTPAYOUTTOOL">>,
                        payout_tool_info = {bank_account, #domain_BankAccount{
                            account = <<"">>,
                            bank_name = <<"">>,
                            bank_post_account = <<"">>,
                            bank_bik = <<"">>
                        }},
                        payout_tool_currency = ?cur(<<"RUB">>)
                    }
                }
            }
        }},
        {inspector, #domain_InspectorObject{
            ref = #domain_InspectorRef{id = 1},
            data = #domain_Inspector{
                name = <<"Kovalsky">>,
                description = <<"Wold famous inspector Kovalsky at your service!">>,
                proxy = #domain_Proxy{
                    ref = ?prx(2),
                    additional = #{<<"risk_score">> => <<"low">>}
                }
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSetTest
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(2),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSetLive
                }]
            }
        }},
        {contract_template, #domain_ContractTemplateObject{
            ref = ?tmpl(1),
            data = #domain_ContractTemplate{terms = ?trms(1)}
        }},
        {contract_template, #domain_ContractTemplateObject{
            ref = ?tmpl(2),
            data = #domain_ContractTemplate{terms = ?trms(2)}
        }},
        {currency, #domain_CurrencyObject{
            ref = ?cur(<<"RUB">>),
            data = #domain_Currency{
                name = <<"Russian rubles">>,
                numeric_code = 643,
                symbolic_code = <<"RUB">>,
                exponent = 2
            }
        }},
        {category, #domain_CategoryObject{
            ref = ?cat(1),
            data = #domain_Category{
                name = <<"Categories">>,
                description = <<"Goods sold by category providers">>,
                type = test
            }
        }},
        {category, #domain_CategoryObject{
            ref = ?cat(?LIVE_CATEGORY_ID),
            data = #domain_Category{
                name = <<"Real deals">>,
                description = <<"For real men">>,
                type = live
            }
        }},
        {provider, #domain_ProviderObject{
            ref = #domain_ProviderRef{id = 2},
            data = #domain_Provider{
                name = <<"Drovider">>,
                description = <<"I'm out of ideas of what to write here">>,
                terminal = {value, [?trm(5), ?trm(6)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ordsets:from_list([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ordsets:from_list([
                        ?cat(?LIVE_CATEGORY_ID)
                    ])},
                    payment_methods = {value, ordsets:from_list([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(    1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(10000000, ?cur(<<"RUB">>))}
                    }},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, payment_amount)
                        )
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {value, #domain_HoldLifetime{seconds = 2}}
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, payment_amount)
                            )
                        ]}
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(5),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                risk_coverage = low
            }
        }},
        {provider, #domain_ProviderObject{
            ref = #domain_ProviderRef{id = 3},
            data = #domain_Provider{
                name = <<"Euroset">>,
                description = <<"First payment terminal provider">>,
                terminal = {value, [?trm(8)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ordsets:from_list([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ordsets:from_list([
                        ?cat(?LIVE_CATEGORY_ID)
                    ])},
                    payment_methods = {value, ordsets:from_list([
                        ?pmt(payment_terminal, euroset)
                    ])},
                    cash_limit = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(    1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(10000000, ?cur(<<"RUB">>))}
                    }},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, payment_amount)
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(8),
            data = #domain_Terminal{
                name = <<"Eurosucks 1">>,
                description = <<"Eurosucks 1">>,
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                risk_coverage = low
            }
        }},
        {payment_method, #domain_PaymentMethodObject{
            ref = ?pmt(bank_card, visa),
            data = #domain_PaymentMethodDefinition{
                name = <<"Visa bank card">>,
                description = <<"Visa is a major brand of cards issued by Visa">>
            }
        }},
        {payment_method, #domain_PaymentMethodObject{
            ref =  ?pmt(bank_card, mastercard),
            data = #domain_PaymentMethodDefinition{
                name = <<"Mastercard bank card">>,
                description = <<"For everything else, there's MasterCard.">>
            }
        }},
        {payment_method, #domain_PaymentMethodObject{
            ref = ?pmt(payment_terminal, euroset),
            data = #domain_PaymentMethodDefinition{
                name = <<"Euroset Terminal">>,
                description = <<"For old peoples, there's Euroset Terminal.">>
            }
        }},
        {proxy, #domain_ProxyObject{
            ref = ?prx(3),
            data = #domain_ProxyDefinition{
                name        = <<"Merchant proxy">>,
                description = <<"Merchant proxy that noone cares about">>,
                url     = <<>>,
                options = #{}
            }
        }}
    ],
    Basic ++ Proxies.

default_get_shop_account_by_id(AccountID, Config) ->
    Context = ?config(context, Config),
    {ok, Response} = capi_client_accounts:get_account_by_id(Context, AccountID),
    Response.

get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

create_context() ->
    woody_context:new().

call_service(Service, Function, Args) ->
    call_service(Service, Function, Args, create_context()).

call_service(Service, Function, Args, ReqCtx) ->
    cp_proto:call_service(Service, Function, Args, ReqCtx, capi_woody_event_handler).

create_and_activate_shop(Config) ->
    ShopID = generate_shop_id(),
    #{<<"id">> := ClaimID} = Claim = default_create_shop(ShopID, ?LIVE_CATEGORY_ID, Config),
    {ok, _} = default_approve_claim(Claim),
    #{
        <<"id">> := ClaimID,
        <<"status">> := <<"ClaimAccepted">>,
        <<"changeset">> := [
            #{
                <<"partyModificationType">> := <<"ShopModification">>,
                <<"shopID">> := ShopID,
                <<"shopModificationType">> := <<"ShopCreation">>
            } | _
        ]
    } = default_get_claim_by_id(ClaimID, Config),
    ok = default_activate_shop(ShopID, Config),
    ShopID.

start_service_handler(Module, C) ->
    start_service_handler(Module, Module, C).

start_service_handler(Name, Module, C) ->
    IP = "0.0.0.0",
    Port = get_random_port(),
    ChildSpec = capi_test_proxy:get_child_spec(Name, Module, IP, Port, #{}),
    {ok, _} = supervisor:start_child(?config(test_sup, C), ChildSpec),
    capi_test_proxy:get_url(Module, ?CAPI_HOST_NAME, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

cleanup() ->
    ReqCtx = create_context(),
    {ok, #'Snapshot'{domain = Domain, version = Version}} = call_service(
        repository,
        'Checkout',
        [{head, #'Head'{}}],
        ReqCtx
    ),
    Commit = #'Commit'{
        ops = [
            {remove, #'RemoveOp'{
                object = Object
            }} ||
                Object <- maps:values(Domain)
        ]
    },
    {ok, _Version} = call_service(
        repository,
        'Commit',
        [Version, Commit],
        ReqCtx
    ),
    ok.


create_account(CurrencyCode, Context) ->
    AccountPrototype = #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode
    },
    {ok, AccountID} = call_service(
        accounter,
        'CreateAccount',
        [AccountPrototype],
        Context
    ),
    AccountID.

start_handler(Module, ProxyID, ProxyOpts, Config) ->
    ProxyUrl = start_service_handler(Module, Config),
    construct_proxy(ProxyID, ProxyUrl, ProxyOpts).

construct_proxy_ref(ID) ->
    #domain_ProxyRef{id = ID}.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = construct_proxy_ref(ID),
        data = #domain_ProxyDefinition{
            name        = Url,
            description = Url,
            url         = Url,
            options     = Options
        }
    }}.

wait_event_w_change(InvoiceID, ChangePattern, TimeLeft, Context) ->
    wait_event_w_change(InvoiceID, ChangePattern, TimeLeft, 0, Context).

wait_event_w_change(InvoiceID, ChangePattern, TimeLeft, LastEventID, Context) when TimeLeft > 0 ->
    Started = genlib_time:ticks(),
    {ok, Events} = capi_client_invoices:get_invoice_events(Context, InvoiceID, LastEventID, 1),
    Filtered = lists:filter(
        fun(#{<<"changes">> := EventChanges}) ->
            is_changes_match_patterns(EventChanges, ChangePattern)
        end,
        Events
    ),
    case Filtered of
        [] ->
            timer:sleep(200),
            Now = genlib_time:ticks(),
            TimeLeftNext = TimeLeft - (Now - Started) div 1000,
            LastEventIDNext = get_last_event_id(Events, LastEventID),
            wait_event_w_change(InvoiceID, ChangePattern, TimeLeftNext, LastEventIDNext, Context);
        _ ->
            ok
    end;

wait_event_w_change(InvoiceID, ChangePattern, _, LastEventID, _Context) ->
    error({event_limit_exceeded, {InvoiceID, ChangePattern, LastEventID}}).

is_changes_match_patterns(Changes, Pattern) when is_list(Changes) ->
    lists:any(
        fun(Change) ->
            maps:merge(Change, Pattern) =:= Change
        end,
        Changes
    ).

get_last_event_id(Events, _) when length(Events) > 0 ->
    #{<<"id">> := LastEventID} = lists:last(Events),
    LastEventID;
get_last_event_id([], LastEventID) ->
    LastEventID.

get_due_date() ->
    {{Y, M, D}, Time} = calendar:local_time(),
    {ok, DueDate} = rfc3339:format({{Y + 1, M, D}, Time}),
    DueDate.

get_lifetime() ->
    get_lifetime(0, 0, 7).

get_lifetime(YY, MM, DD) ->
    #{
       <<"years">>  => YY,
       <<"months">> => MM,
       <<"days">>   => DD
     }.

get_reports_interval() ->
    {{Y, M, D}, Time} = calendar:local_time(),
    {ok, FromTime} = rfc3339:format({{Y - 1, M, D}, Time}),
    {ok, ToTime} = rfc3339:format({{Y + 1, M, D}, Time}),
    {FromTime, ToTime}.

wait_report_w_id(Context, ShopID, FromTime, ToTime, ReportID) ->
    wait_report_w_id(Context, ShopID, FromTime, ToTime, ReportID, 10000).
wait_report_w_id(Context, ShopID, FromTime, ToTime, ReportID, TimeLeft) when TimeLeft > 0->
    Started = genlib_time:ticks(),
    case capi_client_reports:get_reports(Context, ShopID, FromTime, ToTime) of
        {ok, Reports} ->
            case [R || #{<<"id">> := ID} = R <- Reports, ID =:= ReportID] of
                [Report] ->
                    Report;
                [] ->
                    timer:sleep(1000),
                    wait_report_w_id(
                        Context,
                        ShopID,
                        FromTime,
                        ToTime,
                        ReportID,
                        TimeLeft - (genlib_time:ticks() - Started) div 1000
                    )
            end;
        _ ->
            timer:sleep(1000),
            wait_report_w_id(
                Context,
                ShopID,
                FromTime,
                ToTime,
                ReportID,
                TimeLeft - (genlib_time:ticks() - Started) div 1000
            )
    end;
wait_report_w_id(_, _, _, _, _, _) ->
    error(report_not_found).

