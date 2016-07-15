-module(capi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% test cases
-export([
    authorization_error_test/1,
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

all() ->
    [
        authorization_error_test,
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
init_per_suite(C) ->
    {_, Seed} = calendar:local_time(),
    random:seed(Seed),
    test_configuration(),
    {ok, Apps1} = application:ensure_all_started(capi),
    {ok, Apps2} = application:ensure_all_started(hackney),
    [{apps, Apps1 ++ Apps2} | C].

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
authorization_error_test(_) ->
    {ok, 401, _RespHeaders, _ClientRef} = call(get, "/invoices/22?limit=22", #{}, []).

create_invoice_badard_test(_) ->
    Method = post,
    {ok, 400, _RespHeaders, _ClientRef} = default_call(Method, "/invoices", #{}),
    {skip, "Not implemented"}.

create_invoice_ok_test(_) ->
    Method = post,
    Req = #{
        <<"shopID">> => <<"test_shop_id">>,
        <<"amount">> => 100000,
        <<"currency">> => <<"RUB">>,
        <<"context">> => #{
            <<"invoice_dummy_context">> => <<"test_value">>
        },
        <<"dueDate">> => <<"2017-07-11 10:00:00">>,
        <<"product">> => <<"test_product">>,
        <<"description">> => <<"test_invoice_description">>
    },
    {ok, 201, _RespHeaders, _ClientRef} = default_call(Method, "/invoices", Req).

create_payment_ok_test(_) ->
    {skip, "Not implemented"}.

create_payment_tool_token_ok_test(_) ->
    Method = post,
    Req = #{
        <<"paymentToolType">> => <<"cardData">>,
        <<"cardHolder">> => <<"Alexander Weinerschnitzel">>,
        <<"cardNumber">> => 4111111111111111,
        <<"expDate">> => <<"08/27">>,
        <<"cvv">> => <<"232">>
    },
    {ok, 201, _RespHeaders, _ClientRef} = default_call(Method, "/payment_tools", Req).

get_invoice_by_id_ok_test(_) ->
    {skip, "Not implemented"}.

get_invoice_events_ok_test(_) ->
    {skip, "Not implemented"}.

get_payment_by_id_ok_test(_) ->
    {skip, "Not implemented"}.

%% helpers

test_configuration() ->
    application:set_env(capi, host, ?CAPI_HOST),
    application:set_env(capi, port, ?CAPI_PORT),
    application:set_env(capi, service_type, ?CAPI_SERVICE_TYPE).

default_call(Method, Path, Body) ->
    call(Method, Path, Body, [x_request_id_header(), auth_header(), json_content_type_header()]).

call(Method, Path, Body, Headers) ->
    Url = get_url(Path),
    PreparedBody = jsx:encode(Body),
    hackney:request(Method, Url, Headers, PreparedBody).

get_url(Path) ->
    ?CAPI_HOST ++ ":" ++ integer_to_list(?CAPI_PORT)  ++ Path.

x_request_id_header() ->
    {<<"X-Request-ID">>, integer_to_binary(rand:uniform(100000))}.

auth_header() ->
    {<<"Authorization">>, <<"Bearer ", (auth_token())/binary>>} .

auth_token() ->
    <<"I can't find JWT library :(">>.

json_content_type_header() ->
    {<<"Content-Type">>, <<"application/json">>}.
