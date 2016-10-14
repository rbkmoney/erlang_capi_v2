-module(capi_tests_real_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

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
    get_payment_by_id_ok_test/1,
    %%%%
    get_invoices_stats_ok_test/1,
    get_payment_conversion_stats_ok_test/1,
    get_payment_revenue_stats_ok_test/1,
    get_payment_geo_stats_ok_test/1,
    get_payment_rate_stats_ok_test/1,
    %%%%
    get_my_party_ok_test/1,
    suspend_my_party_ok_test/1,
    activate_my_party_ok_test/1,
    get_claim_by_id_ok_test/1,
    revoke_claim_ok_test/1,
    get_pendind_claim_ok_test/1,
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

-define(CAPI_HOST, "0.0.0.0").
-define(CAPI_PORT, 8080).
-define(CAPI_SERVICE_TYPE, real).
-define(CAPI_CDS_STORAGE_URL, "http://cds:8022/v1/storage").
-define(CAPI_INVOICING_URL, "http://hellgate:8022/v1/processing/invoicing").
-define(CAPI_MERCHANT_STAT_URL, "http://magista:8082/stat").
-define(CAPI_PARTY_MANAGEMENT_URL, "http://hellgate:8022/v1/processing/partymgmt").
-define(CAPI_REPOSITORY_URL, "http://dominant:8022/v1/domain/repository").

-define(MERCHANT_ID, <<"hg_tests_SUITE">>).

-type config() :: [{atom(), any()}].

-spec all() -> [
    {group, GroupName :: atom()}
].

all() ->
    [
        {group, authorization},
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
        {card_payment, [sequence], [
            create_invoice_badard_test,
            create_invoice_ok_test,
            create_payment_tool_token_ok_test
        ]},
        {statistics, [parallel], [
            get_invoices_stats_ok_test,
            get_payment_conversion_stats_ok_test,
            get_payment_revenue_stats_ok_test,
            get_payment_geo_stats_ok_test,
            get_payment_rate_stats_ok_test
        ]},
        {party_management, [sequence], [
            get_my_party_ok_test,
            suspend_my_party_ok_test,
            activate_my_party_ok_test
        ]},
        {claims_management, [sequence], [
            get_claim_by_id_ok_test,
            get_pendind_claim_ok_test,
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
                repository => ?CAPI_REPOSITORY_URL
            }}
        ]) ++
        capi_ct_helper:start_app(capi, [
            {host, ?CAPI_HOST},
            {port, ?CAPI_PORT},
            {service_type, ?CAPI_SERVICE_TYPE},
            {api_secret_path, filename:join(?config(data_dir, Config), "public_api_key.pem")}
        ]),
    {{ok, _}, _Context} = populate_categories(),

    [{apps, lists:reverse(Apps)} | Config].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- proplists:get_value(apps, C)].

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
    {ok, 400, _RespHeaders, _Body} = default_call(post, "/v1/processing/invoices", #{}, Config).

-spec create_invoice_ok_test(config()) -> _.

create_invoice_ok_test(Config) ->
    #{<<"id">> := _InvoiceID} = default_create_invoice(Config).

-spec create_payment_ok_test(config()) -> _.

create_payment_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    #{
        <<"session">> := PaymentSession,
        <<"token">> := PaymentToolToken
    } = default_tokenize_card(Config),
    #{<<"id">> := _PaymentID} = default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config).

-spec create_payment_tool_token_ok_test(config()) -> _.

create_payment_tool_token_ok_test(Config) ->
    #{<<"token">> := _Token, <<"session">> := _Session} = default_tokenize_card(Config).

-spec get_invoice_by_id_ok_test(config()) -> _.

get_invoice_by_id_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID),
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_invoice_events_ok_test(config()) -> _.

get_invoice_events_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    #{
        <<"session">> := PaymentSession,
        <<"token">> := PaymentToolToken
    } = default_tokenize_card(Config),
    #{<<"id">> := _PaymentID} = default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config),

    timer:sleep(1000),
    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID) ++ "/events/?limit=100",
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_payment_by_id_ok_test(config()) -> _.

get_payment_by_id_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    #{
        <<"session">> := PaymentSession,
        <<"token">> := PaymentToolToken
    } = default_tokenize_card(Config),
    #{<<"id">> := PaymentID} = default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config),

    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID) ++ "/payments/" ++  genlib:to_list(PaymentID),
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_invoices_stats_ok_test(config()) -> _.

get_invoices_stats_ok_test(Config) ->
    Qs = qs([
        {<<"limit">>, <<"2">>},
        {<<"offset">>, <<"0">>},
        {<<"fromTime">>, <<"2015-08-11T19:42:35Z">>},
        {<<"toTime">>, <<"2020-08-11T19:42:35Z">>}
    ]),
    Path = "/v1/analytics/shops/THRIFT-SHOP/invoices?" ++ Qs,
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_payment_conversion_stats_ok_test(config()) -> _.

get_payment_conversion_stats_ok_test(Config) ->
    Qs = qs([
        {<<"splitUnit">>, <<"minute">>},
        {<<"splitSize">>, <<"1">>},
        {<<"limit">>, <<"2">>},
        {<<"offset">>, <<"0">>},
        {<<"fromTime">>, <<"2015-08-11T19:42:35Z">>},
        {<<"toTime">>, <<"2020-08-11T19:42:35Z">>}
    ]),
    Path = "/v1/analytics/shops/THRIFT-SHOP/payments/stats/conversion?" ++ Qs,
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_payment_revenue_stats_ok_test(config()) -> _.

get_payment_revenue_stats_ok_test(Config) ->
    Qs = qs([
        {<<"splitUnit">>, <<"minute">>},
        {<<"splitSize">>, <<"1">>},
        {<<"limit">>, <<"2">>},
        {<<"offset">>, <<"0">>},
        {<<"fromTime">>, <<"2015-08-11T19:42:35Z">>},
        {<<"toTime">>, <<"2020-08-11T19:42:35Z">>}
    ]),
    Path = "/v1/analytics/shops/THRIFT-SHOP/payments/stats/revenue?" ++ Qs,
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_payment_geo_stats_ok_test(config()) -> _.

get_payment_geo_stats_ok_test(Config) ->
    Qs = qs([
        {<<"splitUnit">>, <<"minute">>},
        {<<"splitSize">>, <<"1">>},
        {<<"limit">>, <<"2">>},
        {<<"offset">>, <<"0">>},
        {<<"fromTime">>, <<"2015-08-11T19:42:35Z">>},
        {<<"toTime">>, <<"2020-08-11T19:42:35Z">>}
    ]),
    Path = "/v1/analytics/shops/THRIFT-SHOP/payments/stats/geo?" ++ Qs,
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

-spec get_payment_rate_stats_ok_test(config()) -> _.

get_payment_rate_stats_ok_test(Config) ->
    Qs = qs([
        {<<"splitUnit">>, <<"minute">>},
        {<<"splitSize">>, <<"1">>},
        {<<"limit">>, <<"2">>},
        {<<"offset">>, <<"0">>},
        {<<"fromTime">>, <<"2015-08-11T19:42:35Z">>},
        {<<"toTime">>, <<"2020-08-11T19:42:35Z">>}
    ]),
    Path = "/v1/analytics/shops/THRIFT-SHOP/customers/stats/rate?" ++ Qs,
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

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
    #{} = default_activate_my_party(Config),
    #{
        <<"isSuspended">> := false
    } = default_get_party(Config).

-spec get_claim_by_id_ok_test(config()) -> _.

get_claim_by_id_ok_test(Config) ->
    Config.

-spec revoke_claim_ok_test(config()) -> _.

revoke_claim_ok_test(Config) ->
    Config.

-spec get_pendind_claim_ok_test(config()) -> _.

get_pendind_claim_ok_test(Config) ->
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
    {{ok, #payproc_Claim{
        id = ClaimID,
        status = {accepted, _},
        changeset = [
            {shop_creation, #domain_Shop{
                id = ShopID
            }} | _
        ]
    }}, _Context} = default_get_claim_by_id(ClaimID, Config),

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
        <<"shopDetails">> => NewShopDetails
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

default_call(Method, Path, Body, Config) ->
    call(Method, Path, Body, [x_request_id_header(), default_auth_header(Config), json_content_type_header()]).

call(Method, Path, Body, Headers) ->
    Url = get_url(Path),
    PreparedBody = jsx:encode(Body),
    {ok, Code, RespHeaders, ClientRef} = hackney:request(Method, Url, Headers, PreparedBody),
    {ok, Code, RespHeaders, get_body(ClientRef)}.

get_url(Path) ->
    ?CAPI_HOST ++ ":" ++ integer_to_list(?CAPI_PORT)  ++ Path.

x_request_id_header() ->
    {<<"X-Request-ID">>, integer_to_binary(rand:uniform(100000))}.

default_auth_header(Config) ->
   auth_header(default_auth_token(Config)).

auth_header(Token) ->
    {<<"Authorization">>, <<"Bearer ", Token/binary>>} .

default_auth_token(Config) ->
    ResourceAccess = #{
        <<"common-api">> => #{
            <<"roles">> =>
                [
                    <<"invoices:create">>,
                    <<"payments:create">>,
                    <<"payment_tool_tokens:create">>,
                    <<"profiles:create">>,
                    <<"profiles:delete">>,
                    <<"invoices:get">>,
                    <<"invoices:fulfill">>,
                    <<"invoices:rescind">>,
                    <<"invoices.events:get">>,
                    <<"payments:get">>,
                    <<"invoices_stats:get">>,
                    <<"payments_conversion_stats:get">>,
                    <<"payments_revenue_stats:get">>,
                    <<"payments_geo_stats:get">>,
                    <<"payments_rate_stats:get">>,
                    <<"payments_instrument_stats:get">>,
                    <<"party:get">>,
                    <<"party:create">>,
                    <<"shops:activate">>,
                    <<"shop:create">>,
                    <<"shops:suspend">>,
                    <<"shops:update">>,
                    <<"party:suspend">>,
                    <<"party:activate">>,
                    <<"claims:get">>,
                    <<"claims:revoke">>,
                    <<"categories:get">>,
                    <<"accounts:get">>
                ]
        }
    },
    auth_token(ResourceAccess, default_token_expiration(), Config).

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

default_token_expiration() ->
    genlib_time:unow() + 60.

json_content_type_header() ->
    {<<"Content-Type">>, <<"application/json">>}.

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
    {ok, 201, _RespHeaders, Body} = default_call(post, "/v1/processing/invoices", Req, Config),
    decode_body(Body).

default_tokenize_card(Config) ->
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
    {ok, 201, _RespHeaders, Body} = default_call(post, "/v1/processing/payment_tools", Req, Config),
    decode_body(Body).

default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config) ->
    Req =  #{
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => PaymentToolToken,
        <<"contactInfo">> => #{
            <<"email">> => <<"bla@bla.ru">>
        }
    },
    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID) ++ "/payments",
    {ok, 201, _RespHeaders, Body} = default_call(post, Path, Req, Config),
    decode_body(Body).

default_get_party(Config) ->
    Path = "/v1/processing/me",
    {ok, 200, _RespHeaders, Body} = default_call(get, Path, #{}, Config),
    decode_body(Body).

default_get_claim_by_id(ClaimID, _Config) ->
    UserInfo = #payproc_UserInfo{
        id = ?MERCHANT_ID
    },
    cp_proto:call_service_safe(
        party_management,
        'GetClaim',
        [UserInfo, ?MERCHANT_ID, ClaimID],
        create_context()
    ).

default_suspend_my_party(Config) ->
    Path = "/v1/processing/me/suspend",
    {ok, 202, _RespHeaders, Body} = default_call(put, Path, #{}, Config),
    decode_body(Body).

default_activate_my_party(Config) ->
    Path = "/v1/processing/me/activate",
    {ok, 202, _RespHeaders, Body} = default_call(put, Path, #{}, Config),
    decode_body(Body).

default_suspend_shop(ShopID, Config) ->
    Path = "/v1/processing/shops/" ++ genlib:to_list(ShopID) ++ "/suspend",
    {ok, 202, _RespHeaders, Body} = default_call(put, Path, #{}, Config),
    decode_body(Body).

default_activate_shop(ShopID, Config) ->
    Path = "/v1/processing/shops/" ++ genlib:to_list(ShopID) ++ "/activate",
    {ok, 202, _RespHeaders, Body} = default_call(put, Path, #{}, Config),
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
    Path = "/v1/processing/shops",
    Req = #{
        <<"categoryRef">> => CategoryRef,
        <<"shopDetails">> => #{
            <<"name">> => <<"OOOBlackMaster">>,
            <<"description">> => <<"Goods for education">>
        }
    },
    {ok, 202, _RespHeaders, Body} = default_call(post, Path, Req, Config),
    decode_body(Body).

update_shop(Req, ShopID, Config) ->
    Path = "/v1/processing/shops/" ++ genlib:to_list(ShopID),
    {ok, 202, _RespHeaders, Body} = default_call(post, Path, Req, Config),
    decode_body(Body).

default_get_categories(Config) ->
    Path = "/v1/processing/categories",
    {ok, 200, _RespHeaders, Body} = default_call(get, Path, #{}, Config),
    decode_body(Body).

default_get_category_by_ref(CategoryRef, Config) ->
    Path = "/v1/processing/categories/" ++ genlib:to_list(CategoryRef),
    {ok, 200, _RespHeaders, Body} = default_call(get, Path, #{}, Config),
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

populate_categories() ->
     {{ok, #'Snapshot'{version = Version}}, Context0} = cp_proto:call_service_safe(
        repository,
        'Checkout',
        [{head, #'Head'{}}],
        create_context()
    ),
    Ops = [
        {'insert', #'InsertOp'{
            object = {category, #domain_CategoryObject{
                    ref = #domain_CategoryRef{
                        id = rand:uniform(10000)
                    },
                    data = #domain_Category{
                        name = genlib:unique()
                    }
                }}
        }} || _I <- lists:seq(1, 10)
    ],
    Commit = #'Commit'{
        ops = Ops
    },

    cp_proto:call_service_safe(
        repository,
        'Commit',
        [Version, Commit],
        Context0
    ).

default_get_shop_accounts(ShopID, Config) ->
    Path = "/v1/processing/shops/" ++ genlib:to_list(ShopID) ++  "/accounts",
    {ok, 200, _RespHeaders, Body} = default_call(get, Path, #{}, Config),
    decode_body(Body).

default_get_shop_account_by_id(AccountID, ShopID, Config) ->
    Path = "/v1/processing/shops/"
        ++ genlib:to_list(ShopID)
        ++ "/accounts/"
        ++ genlib:to_list(AccountID),
    {ok, 200, _RespHeaders, Body} = default_call(get, Path, #{}, Config),
    decode_body(Body).

get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

decode_body(Body) ->
    jsx:decode(Body, [return_maps]).

-spec qs([{binary(), binary() | true}]) -> list().
qs(QsVals) ->
    genlib:to_list(cow_qs:qs(QsVals)).

create_context() ->
    woody_client:new_context(genlib:unique(), capi_woody_event_handler).

create_and_activate_shop(Config) ->
    [Category | _] = default_get_categories(Config),
    #{
        <<"name">> := _Name,
        <<"categoryRef">> := CategoryRef
    } = Category,
    #{
        <<"claimID">> := ClaimID
    } = default_create_shop(CategoryRef, Config),
    {ok, _} = default_approve_claim(ClaimID),
    {{ok, #payproc_Claim{
        id = ClaimID,
        status = {accepted, _},
        changeset = [
            {shop_creation, #domain_Shop{
                id = ShopID
            }} | _
        ]
    }}, _Context} = default_get_claim_by_id(ClaimID, Config),
    #{
        <<"claimID">> := _
    } = default_activate_shop(ShopID, Config),
    ShopID.
