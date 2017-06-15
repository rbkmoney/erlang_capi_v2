-module(capi_tests_real_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_accounter_thrift.hrl").


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
    create_invoice_access_token_ok_test/1,
    create_payment_ok_test/1,
    create_payment_ok_w_access_token_test/1,
    create_payment_tool_token_ok_test/1,
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
    get_locations_names_ok_test/1
]).

-define(KEYCLOAK_HOST, "keycloak").
-define(KEYCLOAK_PORT, 8080).
-define(KEYCLOAK_USER, "demo_merchant").
-define(KEYCLOAK_PASSWORD, "test").

-define(CAPI_IP                   , "::").
-define(CAPI_HOST                 , "localhost").
-define(CAPI_PORT                 , 8080).
-define(CAPI_SERVICE_TYPE         , real).
-define(CAPI_PARTY_MANAGEMENT_URL , "http://hellgate:8022/v1/processing/partymgmt").
-define(CAPI_ACCOUNTER_URL        , "http://shumway:8022/accounter").
-define(CAPI_INVOICING_URL        , "http://hellgate:8022/v1/processing/invoicing").
-define(CAPI_WEBHOOK_MGR_URL      , "http://hooker:8022/hook").
-define(CAPI_REPOSITORY_URL       , "http://dominant:8022/v1/domain/repository").
-define(CAPI_CDS_STORAGE_URL      , "http://cds:8022/v1/storage").
-define(CAPI_MERCHANT_STAT_URL    , "http://magista:8022/stat").
-define(CAPI_GEO_IP_URL           , "http://columbus:8022/repo").
-define(CAPI_HOST_NAME            , "capi").

-define(MERCHANT_ID, <<"281220eb-a4ef-4d03-b666-bdec4b26c5f7">>).
-define(LIVE_CATEGORY_ID, 100).

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
        {group, card_payment},
        {group, invoice_access_token_management},
        {group, statistics},
        {group, party_management},
        {group, contracts_management},
        {group, claims_management},
        {group, shops_management},
        {group, accounts_management},
        {group, webhook_management},
        {group, geo_ip}
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
            create_invoice_ok_test,
            get_invoice_by_id_ok_test,
            rescind_invoice_ok_test
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
        ]}
    ].
%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({api_client_lib, 'handle_response', '_'}, x),
    Apps =
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(cowlib) ++
        capi_ct_helper:start_app(woody) ++
        capi_ct_helper:start_app(api_client) ++
        capi_ct_helper:start_app(cp_proto, [
            {service_urls, #{
                party_management => ?CAPI_PARTY_MANAGEMENT_URL,
                accounter        => ?CAPI_ACCOUNTER_URL,
                invoicing        => ?CAPI_INVOICING_URL,
                webhook_manager  => ?CAPI_WEBHOOK_MGR_URL,
                repository       => ?CAPI_REPOSITORY_URL,
                cds_storage      => ?CAPI_CDS_STORAGE_URL,
                merchant_stat    => ?CAPI_MERCHANT_STAT_URL,
                geo_ip_service   => ?CAPI_GEO_IP_URL
            }}
        ]),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    Params = #{
        host => ?KEYCLOAK_HOST,
        port => ?KEYCLOAK_PORT,
        user => ?KEYCLOAK_USER,
        password => ?KEYCLOAK_PASSWORD,
        retries => 10,
        timeout => 5000
    },
    {ok, Token} = api_client_lib:login(Params),
    Retries = 10,
    Timeout = 5000,
    Context = api_client_lib:get_context(?CAPI_HOST, ?CAPI_PORT, Token, Retries, Timeout),
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
    {error, _} = api_client_invoices:create_invoice(Context, Req).

-spec create_invoice_no_shop_test(config()) -> _.

create_invoice_no_shop_test(Config) ->
    Context = ?config(context, Config),
    Req = #{
        <<"shopID">> => -1,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>,
        <<"metadata">> => #{
            <<"invoice_dummy_metadata">> => <<"test_value">>
        },
        <<"dueDate">> => get_due_date(),
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    {error, Resp} = api_client_invoices:create_invoice(Context, Req),
    #{
        <<"code">> := <<"invalidShopID">>
    } = jsx:decode(Resp, [return_maps]).

-spec create_invoice_ok_test(config()) -> _.

create_invoice_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    {save_config, InvoiceID}.

-spec create_invoice_access_token_ok_test(config()) -> _.

create_invoice_access_token_ok_test(Config) ->
    {create_invoice_ok_test, InvoiceID} = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, Body} = api_client_invoices:create_invoice_access_token(Context, InvoiceID),
    #{<<"payload">> := TokenPayload} = Body,
    InvoiceContext = api_client_lib:get_context(?CAPI_HOST, ?CAPI_PORT, TokenPayload, 10, 5000),
    {save_config, #{
        invoice_id      => InvoiceID,
        invoice_context => InvoiceContext
    }}.

-spec get_invoice_by_id_w_access_token_ok_test(config()) -> _.

get_invoice_by_id_w_access_token_ok_test(Config) ->
    {create_invoice_access_token_ok_test,
        #{invoice_id := InvoiceID, invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {ok, _Body} = api_client_invoices:get_invoice_by_id(Context, InvoiceID),
    {save_config, Info}.

-spec get_random_invoice_w_access_token_failed_test(config()) -> _.

get_random_invoice_w_access_token_failed_test(Config) ->
    {get_invoice_by_id_w_access_token_ok_test,
        #{invoice_id := InvoiceID, invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {error, _} = api_client_invoices:get_invoice_by_id(Context, <<InvoiceID/binary, "BLARG">>),
    {save_config, Info}.

-spec rescind_invoice_w_access_token_failed_test(config()) -> _.

rescind_invoice_w_access_token_failed_test(Config) ->
    {get_random_invoice_w_access_token_failed_test,
        #{invoice_id := InvoiceID, invoice_context := Context} = Info
    } = ?config(saved_config, Config),
    {error, _} = api_client_invoices:rescind_invoice(Context, InvoiceID, <<"pwnd">>),
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
        invoice_context := Context,
        session         := PaymentSession,
        token           := PaymentToolToken,
        invoice_id      := InvoiceID
    }} = ?config(saved_config, Config),
    #{<<"id">> := PaymentID} = default_create_payment(
        InvoiceID,
        PaymentSession,
        PaymentToolToken,
        Context,
        Config
    ),
    wait_event(
        InvoiceID,
        #{
            <<"eventType">> => <<"EventInvoiceStatusChanged">>,
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
    {get_payment_by_id_ok_test,
        #{invoice_id := InvoiceID} = Info
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),

    wait_event(
        InvoiceID,
        #{
            <<"eventType">> => <<"EventInvoiceStatusChanged">>,
            <<"status">> => <<"paid">>
        },
        3000,
        Context
    ),
    ok = default_fulfill_invoice(InvoiceID, Config),
    {save_config, Info}.

-spec rescind_invoice_ok_test(config()) -> _.

rescind_invoice_ok_test(Config) ->
    {get_invoice_by_id_ok_test,
        InvoiceID
    } = ?config(saved_config, Config),
    ok = default_rescind_invoice(InvoiceID, Config).


-spec create_payment_ok_test(config()) -> _.

create_payment_ok_test(Config) ->
    {create_payment_tool_token_ok_test, #{
        <<"session">> := PaymentSession,
        <<"token">> := PaymentToolToken,
        <<"invoiceID">> := InvoiceID
    }} = ?config(saved_config, Config),
    #{<<"id">> := PaymentID} = default_create_payment(
        InvoiceID,
        PaymentSession,
        PaymentToolToken,
        Config
    ),
    {save_config, #{payment_id => PaymentID, invoice_id => InvoiceID}}.


-spec create_payment_tool_token_ok_test(config()) -> _.

create_payment_tool_token_ok_test(Config) ->
    {create_invoice_ok_test,
        InvoiceID
    } = ?config(saved_config, Config),
    {ok, Token, Session} =  default_tokenize_card(Config),
    Info = #{
        <<"token">> => Token,
        <<"session">> => Session
    },
    {save_config, Info#{<<"invoiceID">> => InvoiceID}}.

-spec get_invoice_by_id_ok_test(config()) -> _.

get_invoice_by_id_ok_test(Config) ->
    {create_invoice_ok_test,
        InvoiceID
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, _Body} = api_client_invoices:get_invoice_by_id(Context, InvoiceID),
    {save_config, InvoiceID}.

-spec get_invoice_events_ok_test(config()) -> _.

get_invoice_events_ok_test(Config) ->
    {fulfill_invoice_ok_test,
        #{invoice_id := InvoiceID}
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    wait_event(
        InvoiceID,
        #{
            <<"eventType">> => <<"EventInvoiceStatusChanged">>,
            <<"status">> => <<"fulfilled">>
        },
        3000,
        Context
    ),
    {ok, Events} = api_client_invoices:get_invoice_events(Context, InvoiceID, 10),
    [
        #{
            <<"eventType">> := <<"EventInvoiceCreated">>,
            <<"invoice">> := #{
                <<"id">> := InvoiceID
            }
        },
        #{
            <<"eventType">> := <<"EventPaymentStarted">>
        },
        #{
            <<"eventType">> := <<"EventPaymentStatusChanged">>,
            <<"status">> := <<"processed">>
        },
        #{
            <<"eventType">> := <<"EventPaymentStatusChanged">>,
            <<"status">> := <<"captured">>
        },
        #{
            <<"eventType">> := <<"EventInvoiceStatusChanged">>,
            <<"status">> := <<"paid">>
        },
        #{
            <<"eventType">> := <<"EventInvoiceStatusChanged">>,
            <<"status">> := <<"fulfilled">>
        }
    ] = Events.

-spec get_payments_ok_test(config()) -> _.

get_payments_ok_test(Config) ->
    {create_payment_ok_test,
        #{payment_id := PaymentID, invoice_id := InvoiceID} = Info
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, [#{<<"id">> := PaymentID}]} = get_payments(InvoiceID, Context),
    {save_config, Info}.

-spec get_payment_by_id_ok_test(config()) -> _.

get_payment_by_id_ok_test(Config) ->
    {get_payments_ok_test,
        #{payment_id := PaymentID, invoice_id := InvoiceID} = Info
    } = ?config(saved_config, Config),
    Context = ?config(context, Config),
    {ok, _Body} = api_client_payments:get_payment_by_id(Context, InvoiceID, PaymentID),
    {save_config, Info}.

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
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerEmail, <<"test@test_rbk.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        %%{cardNumberMask, <<"2222">>},  %%@FIXME cannot be used until getting the newest api client
        {paymentAmount, 10000}
    ],

    {ok, _, _} = api_client_searches:search_invoices(Context, ShopID, Query).

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
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerEmail, <<"test@test_rbk.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        %%{cardNumberMask, <<"2222">>}, %%@FIXME cannot be used until getting the newest api client
        {paymentAmount, 10000}
    ],

    {ok, _, _} = api_client_searches:search_payments(Context, ShopID, Query).

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
    {ok, _Body} = api_client_analytics:get_payment_conversion_stats(Context, ShopID, Query).

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
    {ok, _Body} = api_client_analytics:get_payment_revenue_stats(Context, ShopID, Query).

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
    {ok, _Body} = api_client_analytics:get_payment_geo_stats(Context, ShopID, Query).

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
    {ok, _Body} = api_client_analytics:get_payment_rate_stats(Context, ShopID, Query).


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
    {ok, _Body} = api_client_analytics:get_payment_method_stats(Context, ShopID, Query).

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
    #{
        <<"claimID">> := ClaimID
    }  = default_create_contract(Config),
    default_approve_claim(ClaimID),
    #{
        <<"id">> := ClaimID,
        <<"changeset">> := ChangeSet
    } = default_get_claim_by_id(ClaimID, Config),
    #{
        <<"id">> := ContractID
    } = lists:foldl(
        fun
            (
                #{
                    <<"partyModificationType">> := <<"ContractCreation">>,
                    <<"contract">> := Contract
                }, _Acc
            ) ->
                Contract;
            (_, Acc) ->
                Acc

        end,
        undefined,
        ChangeSet
    ),
    {save_config, ContractID}.

-spec get_contract_by_id_ok_test(config()) -> _.

get_contract_by_id_ok_test(Config) ->
    {create_contract_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    #{
        <<"id">> := ContractID
    } = get_contract_by_id(ContractID, Config),
    {save_config, ContractID}.

-spec create_payout_tool_ok_test(config()) -> _.

create_payout_tool_ok_test(Config) ->
    {get_contract_by_id_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    #{
        <<"claimID">> := _
    } = default_create_payout_tool(ContractID, Config),
    {save_config, ContractID}.

-spec get_payout_tools_ok_test(config()) -> _.

get_payout_tools_ok_test(Config) ->
    {create_payout_tool_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    [#{
        <<"id">> := _PayoutToolID
    }] = get_payout_tools(ContractID, Config),
    {save_config, ContractID}.

-spec get_contracts_ok_test(config()) -> _.

get_contracts_ok_test(Config) ->
    {get_payout_tools_ok_test,
        ContractID
    } = ?config(saved_config, Config),
    lists:member(
        ContractID,
        [C || #{ <<"id">> := C} <- get_contracts(Config)]
    ) =:= true.

-spec get_claim_by_id_ok_test(config()) -> _.

get_claim_by_id_ok_test(Config) ->
    ClaimID = default_create_shop(?LIVE_CATEGORY_ID, Config),
    #{
        <<"id">> := ClaimID
    } = default_get_claim_by_id(ClaimID, Config),
    {save_config, ClaimID}.

-spec get_claims_by_status_ok_test(config()) -> _.

get_claims_by_status_ok_test(Config) ->
    {get_claim_by_id_ok_test,
        ClaimID
    } = ?config(saved_config, Config),
    [#{
        <<"id">> := ClaimID,
        <<"status">> := #{<<"status">> := <<"ClaimPending">>}
    }] = default_get_claims_by_status(pending, Config),
    {save_config, ClaimID}.

-spec revoke_claim_ok_test(config()) -> _.

revoke_claim_ok_test(Config) ->
    {get_claims_by_status_ok_test,
        ClaimID
    } = ?config(saved_config, Config),
    ok = default_revoke_claim(ClaimID, Config),
    #{
        <<"id">> := ClaimID,
        <<"status">> := #{<<"status">> := <<"ClaimRevoked">>}
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
    ClaimID = default_create_shop(CategoryID, Config),
    {ok, _} = default_approve_claim(ClaimID),
    #{
        <<"id">> := ClaimID,
        <<"status">> :=  #{<<"status">> := <<"ClaimAccepted">>},
        <<"changeset">> := [
            #{
                <<"partyModificationType">> := <<"ShopCreation">>,
                <<"shop">> := #{
                    <<"id">> := ShopID
                }
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
    [
        #{
            <<"id">> := ShopID
        }
    | _] = get_shops(Config),
    {save_config, ShopID}.


-spec update_shop_ok_test(config()) -> _.

update_shop_ok_test(Config) ->
    {activate_shop_idempotent_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    #{
        <<"details">> := ShopDetails
    } = default_get_shop_by_id(ShopID, Config),
    NewShopDetails = ShopDetails#{
        <<"location">> => #{
            <<"locationType">> => <<"ShopLocationUrl">>,
            <<"url">> => <<"kill.me">>
        }
    },
    Req = #{
        <<"details">> => NewShopDetails,
        <<"categoryID">> => 1
    },
    ClaimID = update_shop(Req, ShopID, Config),
    default_approve_claim(ClaimID),
    #{
        <<"details">> := NewShopDetails
    } = default_get_shop_by_id(ShopID, Config),
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
    } = default_get_shop_account_by_id(GuaranteeID, ShopID, Config).

-spec create_webhook_error_test(config()) -> _.
create_webhook_error_test(Config) ->
    Context = ?config(context, Config),
    ShopID = -1, % nonexistent
    {error, _} = api_client_webhooks:create_webhook(Context, #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => construct_invoices_scope(ShopID)
    }).

-spec create_webhook_receive_events_test(config()) -> _.
create_webhook_receive_events_test(Config) ->
    Context = ?config(context, Config),
    % % list is empty?
    % [] = api_client_webhooks:get_webhooks(Context),
    % create successful?
    Shop = get_latest(get_shops(Config)),
    ShopID = maps:get(<<"id">>, Shop),
    WebhookParams = #{
        <<"url">>   => <<"http://localhost:8080/TODO">>,
        <<"scope">> => construct_invoices_scope(ShopID, ['InvoiceCancelled'])
    },
    {ok, Webhook = #{<<"id">> := WebhookID}} = api_client_webhooks:create_webhook(Context, WebhookParams),
    {ok, Webhook} = api_client_webhooks:get_webhook_by_id(Context, WebhookID),
    % list is not empty then?
    true = lists:member(Webhook, api_client_webhooks:get_webhooks(Context)),
    % delete succeeded idempotently?
    ok = api_client_webhooks:delete_webhook_by_id(Context, WebhookID),
    ok = api_client_webhooks:delete_webhook_by_id(Context, WebhookID),
    [] = [W || #{<<"id">> := W} <- api_client_webhooks:get_webhooks(Context), W =:= WebhookID],
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

%% helpers
call(Method, Path, Body, Headers) ->
    Url = get_url(Path),
    PreparedBody = jsx:encode(Body),
    {ok, Code, RespHeaders, ClientRef} = hackney:request(Method, Url, Headers, PreparedBody),
    {ok, Code, RespHeaders, get_body(ClientRef)}.

get_url(Path) ->
    ?CAPI_HOST ++ ":" ++ integer_to_list(?CAPI_PORT) ++ Path.

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
        <<"metadata">> => #{
            <<"invoice_dummy_metadata">> => <<"test_value">>
        },
        <<"dueDate">> => get_due_date(),
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    Context = ?config(context, Config),
    {ok, Body} = api_client_invoices:create_invoice(Context, Req),
    Body.

default_create_contract(Config) ->
    Context = ?config(context, Config),
    Request = #{
        <<"contractor">> => #{
            <<"bankAccount">> => default_bank_account(),
            <<"legalEntity">> => #{
                <<"entityType">> => <<"RussianLegalEntity">>,
                <<"registeredName">> => <<"testRegisteredName">>,
                <<"registeredNumber">> => <<"1234567890123">>,
                <<"inn">> => <<"1234567890">>,
                <<"actualAddress">> => <<"testActualAddress">>,
                <<"postAddress">> => <<"testPostAddress">>,
                <<"representativePosition">> => <<"testRepresentativePosition">>,
                <<"representativeFullName">> => <<"testRepresentativeFullName">>,
                <<"representativeDocument">> => <<"testRepresentativeDocument">>
            }
        },
        <<"payoutToolParams">> => #{
            <<"currency">> => <<"RUB">>,
            <<"payoutToolType">> => <<"PayoutToolBankAccount">>,
            <<"bankAccount">> => default_bank_account()
        }
    },
    Params = #{body => Request},
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_contracts_api:create_contract(Host, Port, PreparedParams),
    handle_response(Response).

get_payments(InvoiceID, Context) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:get_payments(Host, Port, PreparedParams),
    api_client_lib:handle_response(Response).

get_contract_by_id(ContractID, Config) ->
    Context = ?config(context, Config),
    Params = #{
        binding => #{
            <<"contractID">> => ContractID
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_contracts_api:get_contract_by_id(Host, Port, PreparedParams),
    handle_response(Response).

get_contracts(Config) ->
    Context = ?config(context, Config),
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, #{}),
    Response = swag_client_contracts_api:get_contracts(Host, Port, PreparedParams),
    handle_response(Response).

get_shops(Config) ->
    Context = ?config(context, Config),
    Params = #{},
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:get_shops(Host, Port, PreparedParams),
    handle_response(Response).

get_latest(Es) ->
    % Assuming the latest element will have highest numeric ID
    hd(lists:sort(fun (#{<<"id">> := ID1}, #{<<"id">> := ID2}) -> compare_ids(ID1, ID2) end, Es)).

compare_ids(ID1, ID2) when is_binary(ID1) ->
    binary_to_integer(ID1) > binary_to_integer(ID2);
compare_ids(ID1, ID2) when is_integer(ID1) ->
    ID1 > ID2.

default_create_payout_tool(ContractID, Config) ->
    Context = ?config(context, Config),
    Params = #{
        binding => #{
            <<"contractID">> => ContractID
        },
        body => #{
            <<"currency">> => <<"RUB">>,
            <<"payoutToolType">> => <<"PayoutToolBankAccount">>,
            <<"bankAccount">> => default_bank_account()
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:create_payout_tool(Host, Port, PreparedParams),
    handle_response(Response).

default_bank_account() ->
    #{
        <<"account">> => <<"12345678901234567890">>,
        <<"bankName">> => <<"testBankName">>,
        <<"bankPostAccount">> => <<"12345678901234567890">>,
        <<"bankBik">> => <<"123456789">>
    }.

get_payout_tools(ContractID, Config) ->
    Context = ?config(context, Config),
    Params = #{
        binding => #{
            <<"contractID">> => ContractID
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:get_payout_tools(Host, Port, PreparedParams),
    handle_response(Response).

default_tokenize_card(Config) ->
    default_tokenize_card(?config(context, Config), Config).

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
    api_client_tokens:create_payment_tool_token(Context, Req).

default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config) ->
    default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, ?config(context, Config), Config).

default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Context, _Config) ->
    Req = #{
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => PaymentToolToken,
        <<"contactInfo">> => #{
            <<"email">> => <<"bla@bla.ru">>
        }
    },
    {ok, Body} = api_client_payments:create_payment(Context, Req, InvoiceID),
    Body.

default_get_party(Config) ->
    Context = ?config(context, Config),
    {ok, Body} = api_client_parties:get_my_party(Context),
    Body.

default_get_claim_by_id(ClaimID, Config) ->
    Context = ?config(context, Config),
    {ok, Body} = api_client_claims:get_claim_by_id(Context, ClaimID),
    Body.

default_get_claims_by_status(Status, Config) ->
    Context = ?config(context, Config),
    {ok, Body} = api_client_claims:get_claims_by_status(Context, Status),
    Body.

default_suspend_my_party(Config) ->
    Context = ?config(context, Config),
    Context = ?config(context, Config),
    api_client_parties:suspend_my_party(Context).

default_activate_my_party(Config) ->
    Context = ?config(context, Config),
    api_client_parties:activate_my_party(Context).

default_suspend_shop(ShopID, Config) ->
    Context = ?config(context, Config),
    api_client_shops:suspend_shop(Context, ShopID).

default_activate_shop(ShopID, Config) ->
    Context = ?config(context, Config),
    api_client_shops:activate_shop(Context,ShopID).

default_get_shop_by_id(ShopID, Config) ->
    Context = ?config(context, Config),
    Params = #{
        binding => #{
            <<"shopID">> => ShopID
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:get_shop_by_id(Host, Port, PreparedParams),
    {ok, R} = api_client_lib:handle_response(Response),
    R.

default_create_shop(CategoryID, Config) ->
    #{
        <<"claimID">> := ClaimID0
    } = default_create_contract(Config),
    default_approve_claim(ClaimID0),
    #{<<"id">> := ContractID} = get_latest(get_contracts(Config)),
    #{
        <<"claimID">> := ClaimID1
    } = default_create_payout_tool(ContractID, Config),
    default_approve_claim(ClaimID1),
    #{<<"id">> := PayoutToolID} = get_latest(get_payout_tools(ContractID, Config)),

    default_create_shop(CategoryID, ContractID, PayoutToolID, Config).


default_create_shop(CategoryID, ContractID, PayoutToolID, Config) ->
    Context = ?config(context, Config),
    Req = genlib_map:compact(#{
        <<"categoryID">> => CategoryID,
        <<"details">> => #{
            <<"name">> => <<"OOOBlackMaster">>,
            <<"description">> => <<"Goods for education">>
        },
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID
    }),
    {ok, ClaimID} = api_client_shops:create_shop(Context, Req),
    ClaimID.

update_shop(Req, ShopID, Config) ->
    Context = ?config(context, Config),
    {ok, ClaimID} = api_client_shops:update_shop(Context, Req,  ShopID),
    ClaimID.

default_get_categories(Config) ->
    Context = ?config(context, Config),
    {ok, Body} = api_client_categories:get_categories(Context),
    Body.

default_get_category_by_id(CategoryID, Config) ->
    Context = ?config(context, Config),
    {ok, Body} = api_client_categories:get_category_by_ref(Context,  CategoryID),
    Body.

default_revoke_claim(ClaimID, Config) ->
    Context = ?config(context, Config),
    Reason = "me want dat",
    api_client_claims:revoke_claim_by_id(Context, Reason, ClaimID).

default_fulfill_invoice(InvoiceID, Config) ->
    Context = ?config(context, Config),
    Reason = "me want dat",
    api_client_invoices:fulfill_invoice(Context,  InvoiceID, Reason).

default_rescind_invoice(InvoiceID, Config) ->
    Context = ?config(context, Config),
    Reason = "me want dat",
    api_client_invoices:rescind_invoice(Context,  InvoiceID, Reason).

get_locations_names(GeoIDs, Lang, Config) ->
    Context = ?config(context, Config),
    PreparedGeo = genlib_string:join($,,[genlib:to_binary(I) || I <- GeoIDs]),
    Params = #{
        qs_val => #{
            <<"geoIDs">> => PreparedGeo,
            <<"language">> => Lang
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response = swag_client_geo_api:get_locations_names(Host, Port, PreparedParams),
    {ok, R} = api_client_lib:handle_response(Response),
    R.

%% @FIXME thats dirty
default_approve_claim(ClaimID) ->
    UserInfo = #payproc_UserInfo{
        id = ?MERCHANT_ID,
        type = {internal_user, #payproc_InternalUser{}}
    },
    call_service(
        party_management,
        'AcceptClaim',
        [UserInfo, ?MERCHANT_ID, ClaimID]
    ).


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
                ?pmt(bank_card, mastercard)
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
            ]}
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
                providers                 = {value, [?prv(1), ?prv(2)]},
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
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    }
                },
                test_contract_template = ?tmpl(1)
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
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?trm(1), ?trm(2), ?trm(3)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(18, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_1_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 1">>
                },
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(2),
            data = #domain_Terminal{
                name = <<"Brominal 2">>,
                description = <<"Brominal 2">>,
                payment_method = ?pmt(bank_card, mastercard),
                category = ?cat(?LIVE_CATEGORY_ID),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(19, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_2_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 3">>
                },
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(3),
            data = #domain_Terminal{
                name = <<"Brominal 3">>,
                description = <<"Brominal 3">>,
                payment_method = ?pmt(bank_card, mastercard),
                category = ?cat(?LIVE_CATEGORY_ID),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(19, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 3">>
                },
                risk_coverage = low
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
                abs_account = <<"1234567890">>
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(5),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(?LIVE_CATEGORY_ID),
                cash_flow = [
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
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Drominal 1">>
                },
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(?LIVE_CATEGORY_ID),
                cash_flow = [
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
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Drominal 1">>
                },
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

default_get_shop_account_by_id(AccountID, ShopID, Config) ->
    Context = ?config(context, Config),
    Params = #{
        binding => #{
            <<"accountID">> => AccountID,
            <<"shopID">> => ShopID
        }
    },
    {Host, Port, PreparedParams} = api_client_lib:make_request(Context, Params),
    Response =  swag_client_accounts_api:get_account_by_id(Host, Port, PreparedParams),
    handle_response(Response).

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
    ClaimID = default_create_shop(?LIVE_CATEGORY_ID, Config),
    {ok, _} = default_approve_claim(ClaimID),
    #{
        <<"id">> := ClaimID,
        <<"status">> := #{<<"status">> := <<"ClaimAccepted">>},
        <<"changeset">> := [
            #{
                <<"partyModificationType">> := <<"ShopCreation">>,
                <<"shop">> := #{
                    <<"id">> := ShopID
                }
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

handle_response(Response) ->
    {ok, R} = api_client_lib:handle_response(Response),
    R.

wait_event(InvoiceID, Pattern, TimeLeft, Context) when TimeLeft > 0 ->
    Started = genlib_time:now(),
    Limit = 1000,
    {ok, Events} = api_client_invoices:get_invoice_events(Context, InvoiceID, Limit),
    Filtered = lists:filter(
        fun(E) ->
            Intersection = maps:with(maps:keys(Pattern), E),
            case Intersection of
                Pattern ->
                    true;
                _Unknown ->
                    false
            end
        end,
        Events
    ),
    case Filtered of
        [] ->
            timer:sleep(200),
            Now = genlib_time:now(),
            wait_event(InvoiceID, Pattern, TimeLeft - (Now - Started), Context);
        _ ->
            ok
    end;

wait_event(InvoiceID, Pattern, _, _Context) ->
    error({event_limit_exceeded, {InvoiceID, Pattern}}).

get_due_date() ->
    {{Y, M, D}, Time} = calendar:local_time(),
    {ok, DueDate} = rfc3339:format({{Y + 1, M, D}, Time}),
    DueDate.
