-module(capi_tests_mock_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
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
    get_payment_by_id_ok_test/1
]).

-define(CAPI_HOST, "0.0.0.0").
-define(CAPI_PORT, 8080).
-define(CAPI_SERVICE_TYPE, mock).

-type config() :: [{atom(), any()}].

-spec all() -> [
    TestCase :: atom()
].

all() ->
    [
        authorization_error_no_header_test,
        authorization_error_expired_test,
        create_invoice_badard_test,
        create_invoice_ok_test,
        create_payment_ok_test,
        create_payment_tool_token_ok_test,
        get_invoice_by_id_ok_test,
        get_invoice_events_ok_test,
        get_payment_by_id_ok_test
    ].

%%
%% starting/stopping
%%

-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    Apps =
        capi_ct_helper:start_app(lager) ++
        capi_ct_helper:start_app(woody) ++
        capi_ct_helper:start_app(capi, [
            {host, ?CAPI_HOST},
            {port, ?CAPI_PORT},
            {service_type, ?CAPI_SERVICE_TYPE},
            {api_secret_path, filename:join(?config(data_dir, Config), "public_api_key.pem")}
        ]),
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
                    <<"invoices.events:get">>,
                    <<"payments:get">>,
                    <<"profiles:get">>,
                    <<"profiles:get">>,
                    <<"profiles:update">>,
                    <<"invoices_stats:get">>,
                    <<"payments_conversion_stats:get">>,
                    <<"payments_revenue_stats:get">>,
                    <<"payments_geo_stats:get">>,
                    <<"payments_rate_stats:get">>
                ]
        }
    },
    auth_token(ResourceAccess, default_token_expiration(), Config).

auth_token(ResourseAccess, Exp, Config) ->
    Message = #{
        <<"sub">> => <<"hg_tests_SUITE">>,
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
    {<<"Content-Type">>, <<"application/json; charset=utf-8">>}.

default_create_invoice(Config) ->
    {{Y, M, D}, Time} = calendar:local_time(),
    {ok, DueDate} = rfc3339:format({{Y + 1, M, D}, Time}),
    Req = #{
        <<"shopID">> => <<"test_shop_id">>,
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
            <<"paymentToolType">> => <<"cardData">>,
            <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
            <<"cardNumber">> => 4111111111111111,
            <<"expDate">> => <<"08/27">>,
            <<"cvv">> => <<"232">>
        },
        <<"clientInfo">> => #{
            <<"fingerprint">> => <<"test fingerprint">>,
            <<"ipAddress">> => <<"127.0.0.1">>
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

get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

decode_body(Body) ->
    jsx:decode(Body, [return_maps]).

