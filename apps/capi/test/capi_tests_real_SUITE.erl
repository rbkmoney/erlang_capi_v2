-module(capi_tests_real_SUITE).

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
    get_payment_by_id_ok_test/1,
    %%%%
    get_invoices_stats_ok_test/1,
    get_payment_conversion_stats_ok_test/1,
    get_payment_revenue_stats_ok_test/1,
    get_payment_geo_stats_ok_test/1,
    get_payment_rate_stats_ok_test/1
]).

-define(CAPI_HOST, "0.0.0.0").
-define(CAPI_PORT, 8080).
-define(CAPI_SERVICE_TYPE, real).
-define(CAPI_CDS_URL, "http://localhost:8322").
-define(CAPI_HG_URL, "http://localhost:8122").
-define(CAPI_MERCHANT_STAT_URL, "http://192.168.40.129:8081").

all() ->
    [
        authorization_error_no_header_test,
        authorization_error_expired_test,
        create_invoice_badard_test,
        create_invoice_ok_test,
        create_payment_tool_token_ok_test,
        get_invoices_stats_ok_test,
        get_payment_conversion_stats_ok_test,
        get_payment_revenue_stats_ok_test,
        get_payment_geo_stats_ok_test,
        get_payment_rate_stats_ok_test

    ].

%%
%% starting/stopping
%%
init_per_suite(Config) ->
    {_, Seed} = calendar:local_time(),
    random:seed(Seed),
    test_configuration(Config),
    {ok, Apps1} = application:ensure_all_started(capi),
    {ok, Apps2} = application:ensure_all_started(hackney),
    [{apps, Apps1 ++ Apps2} | Config].

end_per_suite(C) ->
    [application_stop(App) || App <- proplists:get_value(apps, C)].

application_stop(App = sasl) ->
    %% hack for preventing sasl deadlock
    %% http://erlang.org/pipermail/erlang-questions/2014-May/079012.html
    error_logger:delete_report_handler(cth_log_redirect),
    _ = application:stop(App),
    error_logger:add_report_handler(cth_log_redirect),
    ok;
application_stop(App) ->
    application:stop(App).

%% tests
authorization_error_no_header_test(_Config) ->
    {ok, 401, _RespHeaders, _Body} = call(get, "/v1/processing/invoices/22?limit=22", #{}, []).

authorization_error_expired_test(Config) ->
    Token = auth_token(#{}, genlib_time:unow() - 10, Config),
    AuthHeader = auth_header(Token),
    {ok, 401, _RespHeaders, _Body} = call(get, "/v1/processing/invoices/22?limit=22", #{}, [AuthHeader]).

create_invoice_badard_test(Config) ->
    {ok, 400, _RespHeaders, _Body} = default_call(post, "/v1/processing/invoices", #{}, Config).

create_invoice_ok_test(Config) ->
    #{<<"id">> := _InvoiceID} = default_create_invoice(Config).

create_payment_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    #{
        <<"session">> := PaymentSession,
        <<"token">> := PaymentToolToken
    } = default_tokenize_card(Config),
    #{<<"id">> := _PaymentID} = default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config).

create_payment_tool_token_ok_test(Config) ->
    #{<<"token">> := _Token, <<"session">> := _Session} = default_tokenize_card(Config).

get_invoice_by_id_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID),
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

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

get_payment_by_id_ok_test(Config) ->
    #{<<"id">> := InvoiceID} = default_create_invoice(Config),
    #{
        <<"session">> := PaymentSession,
        <<"token">> := PaymentToolToken
    } = default_tokenize_card(Config),
    #{<<"id">> := PaymentID} = default_create_payment(InvoiceID, PaymentSession, PaymentToolToken, Config),

    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID) ++ "/payments/" ++  genlib:to_list(PaymentID),
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

get_invoices_stats_ok_test(Config) ->
    Path = "/v1/analytics/shops/THRIFT-SHOP/invoices?limit=2&offset=0&fromTime=2015-08-11T19%3A42%3A35Z&toTime=2020-08-11T19%3A42%3A35Z",
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

get_payment_conversion_stats_ok_test(Config) ->
    Path = "/v1/analytics/shops/THRIFT-SHOP/payments/stats/conversion?splitUnit=minute&splitSize=1&limit=2&offset=0&fromTime=2015-08-11T19%3A42%3A35Z&toTime=2020-08-11T19%3A42%3A35Z",
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

get_payment_revenue_stats_ok_test(Config) ->
    Path = "/v1/analytics/shops/THRIFT-SHOP/payments/stats/revenue?splitUnit=minute&splitSize=1&limit=2&offset=0&fromTime=2015-08-11T19%3A42%3A35Z&toTime=2020-08-11T19%3A42%3A35Z",
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

get_payment_geo_stats_ok_test(Config) ->
    Path = "/v1/analytics/shops/THRIFT-SHOP/payments/stats/geo?splitUnit=minute&splitSize=1&limit=2&offset=0&fromTime=2015-08-11T19%3A42%3A35Z&toTime=2020-08-11T19%3A42%3A35Z",
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).

get_payment_rate_stats_ok_test(Config) ->
    Path = "/v1/analytics/shops/THRIFT-SHOP/customers/stats/rate?splitUnit=minute&splitSize=1&limit=2&offset=0&fromTime=2015-08-11T19%3A42%3A35Z&toTime=2020-08-11T19%3A42%3A35Z",
    {ok, 200, _RespHeaders, _Body} = default_call(get, Path, #{}, Config).
%% helpers

test_configuration(Config) ->
    application:set_env(capi, host, ?CAPI_HOST),
    application:set_env(capi, port, ?CAPI_PORT),
    application:set_env(capi, service_type, ?CAPI_SERVICE_TYPE),
    application:set_env(capi, cds_url, ?CAPI_CDS_URL),
    application:set_env(capi, hg_url, ?CAPI_HG_URL),
    application:set_env(capi, merchant_stat_url, ?CAPI_MERCHANT_STAT_URL),
    application:set_env(capi, api_secret_path, filename:join(?config(data_dir, Config), "public_api_key.pem")).

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
    {<<"Content-Type">>, <<"application/json">>}.

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
        <<"paymentToolToken">> => PaymentToolToken
    },
    Path = "/v1/processing/invoices/" ++ genlib:to_list(InvoiceID) ++ "/payments",
    {ok, 201, _RespHeaders, Body} = default_call(post, Path, Req, Config),
    decode_body(Body).

get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

decode_body(Body) ->
    jsx:decode(Body, [return_maps]).

