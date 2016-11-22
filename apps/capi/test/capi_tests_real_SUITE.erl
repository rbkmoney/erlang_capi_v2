-module(capi_tests_real_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").
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
    authorization_error_no_header_test/1,
    authorization_error_expired_test/1,
    create_invoice_badard_test/1,
    create_invoice_ok_test/1,
    create_payment_ok_test/1,
    create_payment_tool_token_ok_test/1,
    get_invoice_by_id_ok_test/1,
    get_invoice_events_ok_test/1,
    rescind_invoice_ok_test/1,
    fulfill_invoice_ok_test/1,
    get_payment_by_id_ok_test/1,
    %%%%
    get_invoices_stats_ok_test/1,
    get_payment_conversion_stats_ok_test/1,
    get_payment_revenue_stats_ok_test/1,
    get_payment_geo_stats_ok_test/1,
    get_payment_rate_stats_ok_test/1,
    get_payment_method_stats_ok_test/1,
    %%%%
    get_my_party_ok_test/1,
    suspend_my_party_ok_test/1,
    activate_my_party_ok_test/1,
    get_claim_by_id_ok_test/1,
    revoke_claim_ok_test/1,
    get_pending_claim_ok_test/1,
    get_categories_ok_test/1,
    get_category_by_ref_ok_test/1,
    create_shop_ok_test/1,
    update_shop_ok_test/1,
    suspend_shop_ok_test/1,
    activate_shop_ok_test/1,
    %%%%
    get_shop_accounts_ok_test/1,
    get_shop_account_by_id_ok_test/1

]).

-define(KEYCLOAK_HOST, "kk").
-define(KEYCLOAK_PORT, 8080).
-define(KEYCLOAK_USER, "demo_merchant").
-define(KEYCLOAK_PASSWORD, "test").

-define(CAPI_HOST, "0.0.0.0").
-define(CAPI_PORT, 8080).
-define(CAPI_SERVICE_TYPE, real).
-define(CAPI_CDS_STORAGE_URL, "http://cds:8022/v1/storage").
-define(CAPI_INVOICING_URL, "http://hellgate:8022/v1/processing/invoicing").
-define(CAPI_MERCHANT_STAT_URL, "http://magista:8022/stat").
-define(CAPI_PARTY_MANAGEMENT_URL, "http://hellgate:8022/v1/processing/partymgmt").
-define(CAPI_REPOSITORY_URL, "http://dominant:8022/v1/domain/repository").
-define(CAPI_ACCOUNTER_URL, "http://shumway:8022/accounter").
-define(CAPI_HOST_NAME, "capi").

-define(MERCHANT_ID, <<"281220eb-a4ef-4d03-b666-bdec4b26c5f7">>).


-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.


-type config() :: [{atom(), any()}].

-spec all() -> [
    {group, GroupName :: atom()}
].

all() ->
    [
        {group, authorization},
        {group, invoice_management},
        {group, card_payment},
        {group, statistics},
        {group, party_management},
        {group, claims_management},
        {group, shops_management},
        {group, accounts_management}
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
            authorization_error_no_header_test,
            authorization_error_expired_test
        ]},
        {invoice_management, [sequence], [
            create_invoice_badard_test,
            create_invoice_ok_test,
            get_invoice_by_id_ok_test,
            rescind_invoice_ok_test
        ]},
        {card_payment, [sequence], [
            create_invoice_ok_test,
            create_payment_tool_token_ok_test,
            create_payment_ok_test,
            get_payment_by_id_ok_test,
            fulfill_invoice_ok_test,
            get_invoice_events_ok_test
        ]},
        {statistics, [parallel], [
            get_invoices_stats_ok_test,
            get_payment_conversion_stats_ok_test,
            get_payment_revenue_stats_ok_test,
            get_payment_geo_stats_ok_test,
            get_payment_rate_stats_ok_test,
            get_payment_method_stats_ok_test
        ]},
        {party_management, [sequence], [
            get_my_party_ok_test,
            suspend_my_party_ok_test,
            activate_my_party_ok_test
        ]},
        {claims_management, [sequence], [
            get_claim_by_id_ok_test,
            get_pending_claim_ok_test,
            revoke_claim_ok_test
        ]},
        {shops_management, [sequence], [
            get_categories_ok_test,
            get_category_by_ref_ok_test,
            create_shop_ok_test,
            activate_shop_ok_test,
            update_shop_ok_test,
            suspend_shop_ok_test
        ]},
        {accounts_management, [sequence], [
            get_categories_ok_test,
            get_category_by_ref_ok_test,
            create_shop_ok_test,
            get_shop_accounts_ok_test,
            get_shop_account_by_id_ok_test
        ]}
    ].
%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    timer:sleep(20000),
    Apps =
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(cowlib) ++
        capi_ct_helper:start_app(woody) ++
        capi_ct_helper:start_app(cp_proto, [
            {service_urls, #{
                cds_storage => ?CAPI_CDS_STORAGE_URL,
                invoicing => ?CAPI_INVOICING_URL,
                merchant_stat => ?CAPI_MERCHANT_STAT_URL,
                party_management => ?CAPI_PARTY_MANAGEMENT_URL,
                repository => ?CAPI_REPOSITORY_URL,
                accounter => ?CAPI_ACCOUNTER_URL
            }}
        ]) ++
        capi_ct_helper:start_app(capi, [
            {host, ?CAPI_HOST},
            {port, ?CAPI_PORT},
            {service_type, ?CAPI_SERVICE_TYPE},
            {api_secret_path, filename:join(?config(data_dir, Config), "public_api_key.pem")}
        ]),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    Token = api_client_lib:login(?KEYCLOAK_HOST, ?KEYCLOAK_PORT, ?KEYCLOAK_USER, ?KEYCLOAK_PASSWORD),
    Context = api_client_lib:get_context(?CAPI_HOST, ?CAPI_PORT, Token),
    NewConfig = [{apps, lists:reverse(Apps)}, {context, Context}, {test_sup, SupPid} | Config],
    ProxyUrl = start_service_handler(capi_dummy_provider, NewConfig),
    populate_snapshot(ProxyUrl),

    NewConfig.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    exit(?config(test_sup, C), shutdown),
    ok = cleanup(),
    [application:stop(App) || App <- proplists:get_value(apps, C)].

-spec init_per_group(Name :: atom(), config()) -> config().

init_per_group(statistics, Config) ->
    ShopID = create_and_activate_shop(Config),
    [{shop_id, ShopID} | Config];

init_per_group(_, Config) ->
    Config.

-spec end_per_group(Name :: atom(), config()) -> config().

end_per_group(_, Config) ->
    Config.

%% tests
-spec authorization_error_no_header_test(config()) -> _.

authorization_error_no_header_test(_Config) ->
    {ok, 401, _RespHeaders, _Body} = call(get, "/v1/processing/invoices/22?limit=22", #{}, []).

-spec authorization_error_expired_test(config()) -> _.

authorization_error_expired_test(Config) ->
    Token = auth_token(#{}, genlib_time:unow() - 10, Config),
    AuthHeader = auth_header(Token),
    {ok, 401, _RespHeaders, _Body} = call(get, "/v1/processing/invoices/22?limit=22", #{}, [AuthHeader]).

-spec create_invoice_badard_test(config()) -> _.

create_invoice_badard_test(Config) ->
    Context = proplists:get_value(context, Config),
    Req = #{},
    {error, _} = api_client_invoices:create_invoice(Context, Req).

-spec create_invoice_ok_test(config()) -> _.

create_invoice_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    {save_config, InvoiceID}.

-spec fulfill_invoice_ok_test(config()) -> _.

fulfill_invoice_ok_test(Config) ->
    {get_payment_by_id_ok_test,
        #{invoice_id := InvoiceID} = Info
    } = ?config(saved_config, Config),
    #{} = default_fulfill_invoice(InvoiceID, Config),
    {save_config, Info}.

-spec rescind_invoice_ok_test(config()) -> _.

rescind_invoice_ok_test(Config) ->
    {get_invoice_by_id_ok_test,
        InvoiceID
    } = ?config(saved_config, Config),
    #{} = default_rescind_invoice(InvoiceID, Config).


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
    #{
        <<"token">> := _Token,
        <<"session">> := _Session
    } = Info = default_tokenize_card(Config),
    {save_config, Info#{<<"invoiceID">> => InvoiceID}}.

-spec get_invoice_by_id_ok_test(config()) -> _.

get_invoice_by_id_ok_test(Config) ->
    {create_invoice_ok_test,
        InvoiceID
    } = ?config(saved_config, Config),
    Context = proplists:get_value(context, Config),
    {ok, _Body} = api_client_invoices:get_invoice_by_id(Context, InvoiceID),
    {save_config, InvoiceID}.

-spec get_invoice_events_ok_test(config()) -> _.

get_invoice_events_ok_test(Config) ->
    {fulfill_invoice_ok_test,
        #{invoice_id := InvoiceID}
    } = ?config(saved_config, Config),
    Context = proplists:get_value(context, Config),
    Limit = 100,
    {ok, _Body} = api_client_invoices:get_invoice_events(Context, InvoiceID, Limit).

-spec get_payment_by_id_ok_test(config()) -> _.

get_payment_by_id_ok_test(Config) ->
    {create_payment_ok_test,
        #{payment_id := PaymentID, invoice_id := InvoiceID} = Info
    } = ?config(saved_config, Config),
    Context = proplists:get_value(context, Config),
    {ok, _Body} = api_client_payments:get_payment_by_id(Context, InvoiceID, PaymentID),
    {save_config, Info}.

-spec get_invoices_stats_ok_test(config()) -> _.

get_invoices_stats_ok_test(Config) ->
    Context = proplists:get_value(context, Config),
    ShopID = ?config(shop_id, Config),
    Qs = #{
        <<"limit">> => <<"2">>,
        <<"offset">> => <<"0">>,
        <<"fromTime">> => <<"2015-08-11T19:42:35Z">>,
        <<"toTime">> => <<"2020-08-11T19:42:35Z">>,
        <<"status">> => <<"unpaid">>
    },
    {ok, _Body} = api_client_analytics:get_invoices(Context, ShopID, Qs).

-spec get_payment_conversion_stats_ok_test(config()) -> _.

get_payment_conversion_stats_ok_test(Config) ->
    Context = proplists:get_value(context, Config),
    ShopID = ?config(shop_id, Config),
    Qs = #{
        <<"splitUnit">> => <<"minute">>,
        <<"splitSize">> => <<"1">>,
        <<"limit">> => <<"2">>,
        <<"offset">> => <<"0">>,
        <<"fromTime">> => <<"2015-08-11T19:42:35Z">>,
        <<"toTime">> => <<"2020-08-11T19:42:35Z">>
    },
    {ok, _Body} = api_client_analytics:get_payment_conversion_stats(Context, ShopID, Qs).

-spec get_payment_revenue_stats_ok_test(config()) -> _.

get_payment_revenue_stats_ok_test(Config) ->
    Context = proplists:get_value(context, Config),
    ShopID = ?config(shop_id, Config),
    Qs = #{
        <<"splitUnit">> => <<"minute">>,
        <<"splitSize">> => <<"1">>,
        <<"limit">> => <<"2">>,
        <<"offset">> => <<"0">>,
        <<"fromTime">> => <<"2015-08-11T19:42:35Z">>,
        <<"toTime">> => <<"2020-08-11T19:42:35Z">>
    },
    {ok, _Body} = api_client_analytics:get_payment_revenue_stats(Context, ShopID, Qs).

-spec get_payment_geo_stats_ok_test(config()) -> _.

get_payment_geo_stats_ok_test(Config) ->
    Context = proplists:get_value(context, Config),
    ShopID = ?config(shop_id, Config),
    Qs = #{
        <<"splitUnit">> => <<"minute">>,
        <<"splitSize">> => <<"1">>,
        <<"limit">> => <<"2">>,
        <<"offset">> => <<"0">>,
        <<"fromTime">> => <<"2015-08-11T19:42:35Z">>,
        <<"toTime">> => <<"2020-08-11T19:42:35Z">>
    },
    {ok, _Body} = api_client_analytics:get_payment_geo_stats(Context, ShopID, Qs).

-spec get_payment_rate_stats_ok_test(config()) -> _.

get_payment_rate_stats_ok_test(Config) ->
    Context = proplists:get_value(context, Config),
    ShopID = ?config(shop_id, Config),
    Qs = #{
        <<"splitUnit">> => <<"minute">>,
        <<"splitSize">> => <<"1">>,
        <<"limit">> => <<"2">>,
        <<"offset">> => <<"0">>,
        <<"fromTime">> => <<"2015-08-11T19:42:35Z">>,
        <<"toTime">> => <<"2020-08-11T19:42:35Z">>
    },
    {ok, _Body} = api_client_analytics:get_payment_rate_stats(Context, ShopID, Qs).


-spec get_payment_method_stats_ok_test(config()) -> _.

get_payment_method_stats_ok_test(Config) ->
    Context = proplists:get_value(context, Config),
    ShopID = ?config(shop_id, Config),
    Qs = #{
        <<"paymentMethod">> => <<"bank_card">>,
        <<"splitUnit">> => <<"minute">>,
        <<"splitSize">> => <<"1">>,
        <<"limit">> => <<"2">>,
        <<"offset">> => <<"0">>,
        <<"fromTime">> => <<"2015-08-11T19:42:35Z">>,
        <<"toTime">> => <<"2020-08-11T19:42:35Z">>
    },
    {ok, _Body} = api_client_analytics:get_payment_method_stats(Context, ShopID, Qs).

-spec get_my_party_ok_test(config()) -> _.

get_my_party_ok_test(Config) ->
    #{
        <<"isBlocked">> := false,
        <<"isSuspended">> := false,
        <<"partyID">> := ?MERCHANT_ID,
        <<"shops">> := _
    } = default_get_party(Config).

-spec suspend_my_party_ok_test(config()) -> _.

suspend_my_party_ok_test(Config) ->
    #{} = default_suspend_my_party(Config),
    #{
        <<"isSuspended">> := true
    } = default_get_party(Config),
    Config.

-spec activate_my_party_ok_test(config()) -> _.

activate_my_party_ok_test(Config) ->
    #{
        <<"claimID">> := _ClaimID
    } = default_activate_my_party(Config),
    #{
        <<"isSuspended">> := false
    } = default_get_party(Config).

-spec get_claim_by_id_ok_test(config()) -> _.

get_claim_by_id_ok_test(Config) ->
    #{
        <<"categoryRef">> := CategoryRef
    } = get_any_category(Config),
    #{
        <<"claimID">> := ClaimID
    } = default_create_shop(CategoryRef, Config),
    #{
        <<"id">> := ClaimID
    } = default_get_claim_by_id(ClaimID, Config),
    {save_config, ClaimID}.

-spec get_pending_claim_ok_test(config()) -> _.

get_pending_claim_ok_test(Config) ->
    {get_claim_by_id_ok_test,
        ClaimID
    } = ?config(saved_config, Config),
    #{
        <<"id">> := ClaimID,
        <<"status">> := #{<<"status">> := <<"ClaimPending">>}
    } = default_get_claim_by_status(pending, Config),
    {save_config, ClaimID}.

-spec revoke_claim_ok_test(config()) -> _.

revoke_claim_ok_test(Config) ->
    {get_pending_claim_ok_test,
        ClaimID
    } = ?config(saved_config, Config),
    #{} = default_revoke_claim(ClaimID, Config),
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
        <<"categoryRef">> := CategoryRef
    } = Category,
    {save_config, CategoryRef}.

-spec get_category_by_ref_ok_test(config()) -> _.

get_category_by_ref_ok_test(Config) ->
    {get_categories_ok_test,
        CategoryRef
    } = ?config(saved_config, Config),
    _ = default_get_category_by_ref(CategoryRef, Config),
    {save_config, CategoryRef}.

-spec create_shop_ok_test(config()) -> _.

create_shop_ok_test(Config) ->
    {get_category_by_ref_ok_test,
        CategoryRef
    } = ?config(saved_config, Config),
    #{
        <<"claimID">> := ClaimID
    } = default_create_shop(CategoryRef, Config),
    {ok, _} = default_approve_claim(ClaimID),
    #{
        <<"id">> := ClaimID,
        <<"status">> :=  #{<<"status">> := <<"ClaimAccepted">>},
        <<"changeset">> := [
            #{
                <<"modificationType">> := <<"ShopCreation">>,
                <<"shop">> := #{
                    <<"shopID">> := ShopID
                }
            } | _
        ]
    } = default_get_claim_by_id(ClaimID, Config),

    {save_config, ShopID}.

-spec update_shop_ok_test(config()) -> _.

update_shop_ok_test(Config) ->
    {activate_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    {ok, #{
        <<"shopDetails">> := ShopDetails
    }} = default_get_shop_by_id(ShopID, Config),
    NewShopDetails = ShopDetails#{
        <<"location">> => genlib:unique()
    },
    Req = #{
        <<"shopDetails">> => NewShopDetails,
        <<"categoryRef">> => 1
    },
    #{
        <<"claimID">> := ClaimID
    } = update_shop(Req, ShopID, Config),
    default_approve_claim(ClaimID),
    {ok, #{
        <<"shopDetails">> := NewShopDetails
    }} = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.

-spec suspend_shop_ok_test(config()) -> _.

suspend_shop_ok_test(Config) ->
    {update_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    #{
        <<"claimID">> := _
    } = default_suspend_shop(ShopID, Config),
    {ok, #{
        <<"isSuspended">> := true
    }} = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.


-spec activate_shop_ok_test(config()) -> _.

activate_shop_ok_test(Config) ->
    {create_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    #{
        <<"claimID">> := _
    } = default_activate_shop(ShopID, Config),
    {ok, #{
        <<"isSuspended">> := false
    }} = default_get_shop_by_id(ShopID, Config),
    {save_config, ShopID}.

-spec get_shop_accounts_ok_test(config()) -> _.

get_shop_accounts_ok_test(Config) ->
    {create_shop_ok_test,
        ShopID
    } = ?config(saved_config, Config),
    [ShopAccountSet | _] = default_get_shop_accounts(ShopID, Config),
    #{
        <<"generalID">> := _,
        <<"guaranteeID">> := _
    } = ShopAccountSet,
    {save_config, {ShopID, ShopAccountSet}}.

-spec get_shop_account_by_id_ok_test(config()) -> _.

get_shop_account_by_id_ok_test(Config) ->
    {get_shop_accounts_ok_test,
        {ShopID, ShopAccountSet}
    } = ?config(saved_config, Config),
    #{
        <<"guaranteeID">> := GuaranteeID
    }  = ShopAccountSet,
    #{
        <<"id">> := _,
        <<"ownAmount">> := _,
        <<"availableAmount">> := _,
        <<"currency">> := _
    } = default_get_shop_account_by_id(GuaranteeID, ShopID, Config).

%% helpers
call(Method, Path, Body, Headers) ->
    Url = get_url(Path),
    PreparedBody = jsx:encode(Body),
    {ok, Code, RespHeaders, ClientRef} = hackney:request(Method, Url, Headers, PreparedBody),
    {ok, Code, RespHeaders, get_body(ClientRef)}.

get_url(Path) ->
    ?CAPI_HOST ++ ":" ++ integer_to_list(?CAPI_PORT)  ++ Path.

auth_header(Token) ->
    {<<"Authorization">>, <<"Bearer ", Token/binary>>} .

auth_token(ResourseAccess, Exp, Config) ->
    Message = #{
        <<"sub">> => ?MERCHANT_ID,
        <<"resource_access">> => ResourseAccess,
        <<"exp">> => Exp
    },
    RSAPrivateJWK = jose_jwk:from_pem_file(filename:join(?config(data_dir, Config), "private_api_key.pem")),
    Signed = jose_jwk:sign(jsx:encode(Message), #{ <<"alg">> => <<"RS256">> }, RSAPrivateJWK),
    {_Alg, Payload} = jose_jws:compact(Signed),
    Payload.

default_create_invoice(Config) ->
    {{Y, M, D}, Time} = calendar:local_time(),
    {ok, DueDate} = rfc3339:format({{Y + 1, M, D}, Time}),
    ShopID = create_and_activate_shop(Config),
    Req = #{
        <<"shopID">> => ShopID,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>,
        <<"context">> => #{
            <<"invoice_dummy_context">> => <<"test_value">>
        },
        <<"dueDate">> => DueDate,
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_invoices:create_invoice(Context, Req),
    decode_body(Body).

default_tokenize_card(Config) ->
    Context = proplists:get_value(context, Config),
    Req = #{
        <<"paymentTool">> => #{
            <<"paymentToolType">> => <<"CardData">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"cardNumber">> => 4111111111111111,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => #{
            <<"fingerprint">> => <<"test fingerprint">>
        }
    },
    {ok, Body} = api_client_tokens:create_payment_tool_token(Context, Req),
    decode_body(Body).

default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config) ->
    Context = proplists:get_value(context, Config),
    Req = #{
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => PaymentToolToken,
        <<"contactInfo">> => #{
            <<"email">> => <<"bla@bla.ru">>
        }
    },
    {ok, Body} = api_client_payments:create_payment(Context, Req, InvoiceID),
    decode_body(Body).

default_get_party(Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_parties:get_my_party(Context),
    decode_body(Body).

default_get_claim_by_id(ClaimID, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_claims:get_claim_by_id(Context, ClaimID),
    decode_body(Body).

default_get_claim_by_status(Status, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_claims:get_claim_by_status(Context, Status),
    decode_body(Body).

default_suspend_my_party(Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_parties:suspend_my_party(Context),
    decode_body(Body).

default_activate_my_party(Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_parties:activate_my_party(Context),
    decode_body(Body).

default_suspend_shop(ShopID, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_shops:suspend_shop(Context, ShopID),
    decode_body(Body).

default_activate_shop(ShopID, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_shops:activate_shop(Context, ShopID),
    decode_body(Body).

default_get_shop_by_id(ShopID, Config) ->
    #{
        <<"shops">> := Shops
    } = default_get_party(Config),
    Result = lists:filter(
        fun
            (#{<<"shopID">> := ID}) ->
                case ID of
                    ShopID -> true;
                    _ -> false
                end;
            (_) -> false
        end,
        Shops
    ),
    case Result of
        [Shop] -> {ok, Shop};
        _ -> {error, {wrong_result, Result}}
    end.

default_create_shop(CategoryRef, Config) ->
    Context = proplists:get_value(context, Config),
    Req = #{
        <<"categoryRef">> => CategoryRef,
        <<"shopDetails">> => #{
            <<"name">> => <<"OOOBlackMaster">>,
            <<"description">> => <<"Goods for education">>
        },
        <<"contractor">> => #{
            <<"registeredName">> => <<"DefaultRegisteredName">>,
            <<"legalEntity">> => <<"DefaultLegalEntity">>
        }
    },
    {ok, Body} = api_client_shops:create_shop(Context, Req),
    decode_body(Body).

update_shop(Req, ShopID, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_shops:update_shop(Context, Req, ShopID),
    decode_body(Body).

default_get_categories(Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_categories:get_categories(Context),
    decode_body(Body).

default_get_category_by_ref(CategoryRef, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_categories:get_category_by_ref(Context, CategoryRef),
    decode_body(Body).

default_revoke_claim(ClaimID, Config) ->
    Context = proplists:get_value(context, Config),
    Reason = <<"me want dat">>,
    {ok, Body} = api_client_claims:revoke_claim_by_id(Context, Reason, ClaimID),
    decode_body(Body).

default_fulfill_invoice(InvoiceID, Config) ->
    Context = proplists:get_value(context, Config),
    Reason = <<"me want dat">>,
    {ok, Body} = api_client_invoices:fulfill_invoice(Context, InvoiceID, Reason),
    decode_body(Body).

default_rescind_invoice(InvoiceID, Config) ->
    Context = proplists:get_value(context, Config),
    Reason = <<"me want dat">>,
    {ok, Body} = api_client_invoices:rescind_invoice(Context, InvoiceID, Reason),
    decode_body(Body).

%% @FIXME thats dirty
default_approve_claim(ClaimID) ->
    UserInfo = #payproc_UserInfo{
        id = ?MERCHANT_ID
    },
    cp_proto:call_service_safe(
        party_management,
        'AcceptClaim',
        [UserInfo, ?MERCHANT_ID, ClaimID],
        create_context()
    ).

-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(pst(ID), #domain_PaymentsServiceTermsRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).

-define(trmacc(Cur, Rec, Com),
    #domain_TerminalAccountSet{currency = ?cur(Cur), receipt = Rec, compensation = Com}).

-define(cfpost(S, A1, D, A2, V),
    #domain_CashFlowPosting{
        source      = #domain_CashFlowAccount{party = S, designation = genlib:to_binary(A1)},
        destination = #domain_CashFlowAccount{party = D, designation = genlib:to_binary(A2)},
        volume      = V
    }
).

-define(fixed(A),
    {fixed, #domain_CashVolumeFixed{amount = A}}).
-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{parts = #'Rational'{p = P, q = Q}, 'of' = C}}).


populate_snapshot(ProxyUrl) ->
    {{ok, #'Snapshot'{version = Version}}, Context0} = cp_proto:call_service_safe(
        repository,
        'Checkout',
        [{head, #'Head'{}}],
        create_context()
    ),
    Ops = [
        {'insert', #'InsertOp'{
            object = O
        }}
        || O <- get_domain_fixture(ProxyUrl)
    ],
    Commit = #'Commit'{
        ops = Ops
    },

    {{ok, _Version}, _} = cp_proto:call_service_safe(
        repository,
        'Commit',
        [Version, Commit],
        Context0
    ),
    timer:sleep(8000).

get_domain_fixture(ProxyUrl) ->
    Context = create_context(),
    {Accounts, _Context} = lists:foldl(
        fun ({N, CurrencyCode}, {M, C0}) ->
            {AccountID, C1} = create_account(CurrencyCode, C0),
            {M#{N => AccountID}, C1}
        end,
        {#{}, Context},
        [
            {system_compensation     , <<"RUB">>},
            {terminal_1_receipt      , <<"RUB">>},
            {terminal_1_compensation , <<"RUB">>}
        ]
    ),
    [
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, [?prv(1)]},
                system_accounts = {value, [?sas(1)]}
            }
        }},
        {system_account_set, #domain_SystemAccountSetObject{
            ref = ?sas(1),
            data = #domain_SystemAccountSet{
                name = <<"Primaries">>,
                description = <<"Primaries">>,
                currency = ?cur(<<"RUB">>),
                compensation = maps:get(system_compensation, Accounts)
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
                default_services = #domain_ShopServices{
                    payments = #domain_PaymentsService{
                        domain_revision = 0,
                        terms = ?pst(1)
                    }
                }
            }
        }},
        {payments_service_terms, #domain_PaymentsServiceTermsObject{
            ref = ?pst(1),
            data = #domain_PaymentsServiceTerms{
                payment_methods = {value, ordsets:from_list([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                limits = {predicates, [
                    #domain_AmountLimitPredicate{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_AmountLimit{
                            min = {inclusive, 1000},
                            max = {exclusive, 4200000}
                        }}
                    }
                ]},
                fees = {predicates, [
                    #domain_CashFlowPredicate{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, [
                            ?cfpost(merchant, general, system, compensation, ?share(45, 1000, payment_amount))
                        ]}
                    }
                ]}
            }
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
                description = <<"Goods sold by category providers">>
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?trm(1)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                payment_method = #domain_PaymentMethodRef{id = {bank_card, visa}},
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(provider, receipt, merchant, general, ?share(1, 1, payment_amount)),
                    ?cfpost(system, compensation, provider, compensation, ?share(18, 1000, payment_amount))
                ],
                accounts = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_1_receipt, Accounts),
                    maps:get(terminal_1_compensation, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 1">>
                }
            }
        }},
        {proxy, #domain_ProxyObject{
            ref = ?prx(1),
            data = #domain_ProxyDefinition{
                url     = ProxyUrl,
                options = #{}
            }
        }}
    ].

default_get_shop_accounts(ShopID, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_shops:get_shop_accounts(Context, ShopID),
    decode_body(Body).

default_get_shop_account_by_id(AccountID, ShopID, Config) ->
    Context = proplists:get_value(context, Config),
    {ok, Body} = api_client_shops:get_account_by_id(Context, ShopID, AccountID),
    decode_body(Body).

get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

decode_body(Body) ->
    jsx:decode(Body, [return_maps]).

create_context() ->
    woody_client:new_context(genlib:unique(), capi_woody_event_handler).

create_and_activate_shop(Config) ->
    #{
        <<"categoryRef">> := CategoryRef
    } = get_any_category(Config),
    #{
        <<"claimID">> := ClaimID
    } = default_create_shop(CategoryRef, Config),
    {ok, _} = default_approve_claim(ClaimID),
    #{
        <<"id">> := ClaimID,
        <<"status">> := #{<<"status">> := <<"ClaimAccepted">>},
        <<"changeset">> := [
            #{
                <<"modificationType">> := <<"ShopCreation">>,
                <<"shop">> := #{
                    <<"shopID">> := ShopID
                }
            } | _
        ]
    } = default_get_claim_by_id(ClaimID, Config),
    #{
        <<"claimID">> := _
    } = default_activate_shop(ShopID, Config),
    ShopID.

get_any_category(Config) ->
    [Category | _] = default_get_categories(Config),
    Category.

start_service_handler(Module, C) ->
    Port = get_random_port(),
    Opts = #{},
    ChildSpec = capi_test_proxy:get_child_spec(Module, "0.0.0.0", Port, Opts),
    {ok, _} = supervisor:start_child(?config(test_sup, C), ChildSpec),
    capi_test_proxy:get_url(Module, ?CAPI_HOST_NAME, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

cleanup() ->
    {{ok, #'Snapshot'{domain = Domain, version = Version}}, Context0} = cp_proto:call_service_safe(
        repository,
        'Checkout',
        [{head, #'Head'{}}],
        create_context()
    ),
    Commit = #'Commit'{
        ops = [
            {remove, #'RemoveOp'{
                object = Object
            }} ||
                Object <- maps:values(Domain)
        ]
    },
    {{ok, _Version}, _} = cp_proto:call_service_safe(
        repository,
        'Commit',
        [Version, Commit],
        Context0
    ),
    ok.


create_account(CurrencyCode, Context0) ->
    AccountPrototype = #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode
    },
    {{ok, AccountID}, Context} = cp_proto:call_service_safe(
        accounter,
        'CreateAccount',
        [AccountPrototype],
        Context0
    ),
    {AccountID, Context}.
