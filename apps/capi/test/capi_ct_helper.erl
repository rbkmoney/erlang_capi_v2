-module(capi_ct_helper).

-export([start_app/1]).
-export([start_app/2]).

-export([login/1]).
-export([retry/3]).
%%

-type app_name() :: atom().

-type idempotency() :: idempotent | non_idempotent.
-export_type([idempotency/0]).

-spec start_app(app_name()) -> [app_name()].

start_app(lager = AppName) ->
    start_app(AppName, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, warning}
        ]}
    ]);

start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);

start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) -> [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec login(map()) ->
    term().
login(Params) ->
    #{
        url      := Url,
        user     := User,
        password := Password,
        retries  := Retries,
        timeout  := Timeout,
        protocol := Protocol
    } = Params,

    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],

    UsernamePrefix = list_to_binary("username="),
    PasswordPrefix = list_to_binary("&password="),
    Postfix        = list_to_binary("&client_id=common-api&grant_type=password"),
    BinaryName     = list_to_binary(User),
    BinaryPassword = list_to_binary(Password),

    Body = <<UsernamePrefix/binary, BinaryName/binary,
             PasswordPrefix/binary, BinaryPassword/binary,
             Postfix/binary>>,

    Url1 = Url ++ "/auth/realms/external/protocol/openid-connect/token",

    Strategy = genlib_retry:linear(Retries, Timeout),
    F = fun() -> hackney:request(post, Url1, Headers, Body, protocol_to_opt(Protocol)) end,

    case retry(F, Strategy, idempotent) of
        {ok, _Code, _RespHeaders, RespBody} ->
            {ok, maps:get(<<"access_token">>, maps:from_list(jsx:decode(get_body(RespBody))))};
        {error, Error} ->
            {error, Error}
    end.

protocol_to_opt(ipv4) ->
    [{connect_options, [inet4]}];
protocol_to_opt(ipv6) ->
    [{connect_options, [inet6]}].

-spec get_body(term()) ->
    term().
get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

-spec retry(fun(), genlib_retry:strategy(), idempotency()) ->
    {ok, pos_integer(), _RespHeaders, _RespBody} | {error, _Reason}.
retry(F, Strategy, Idempotency) ->
    case F() of
        {ok, Code, RespHeaders, RespBody} ->
            case to_retry(Code, Idempotency) of
                ok ->
                    ok = log_info(Code, RespBody),
                    {ok, Code, RespHeaders, RespBody};
                error ->
                    ok = log_error(finish, Code, RespBody),
                    {error, RespBody};
                retry ->
                    case genlib_retry:next_step(Strategy) of
                        {wait, Timeout, NewStrategy} ->
                            ok = log_error(retry, Code, RespBody),
                            ok = timer:sleep(Timeout),
                            retry(F, NewStrategy, Idempotency);
                        finish ->
                            ok = log_error(finish, Code, RespBody),
                            {error, timeout}
                    end
            end;
        {error, Error} ->
            case genlib_retry:next_step(Strategy) of
                {wait, Timeout, NewStrategy} ->
                    ok = log_error(retry, Error),
                    ok = timer:sleep(Timeout),
                    retry(F, NewStrategy, Idempotency);
                finish ->
                    ok = log_error(finish, Error),
                    {error, timeout}
            end
    end.

-spec to_retry(pos_integer(), idempotency()) ->
    ok | error | retry.
to_retry(Code, idempotent) ->
    case Code of
        Code when (Code div 100) == 2 -> ok;
        _ -> retry
    end;
to_retry(Code, non_idempotent) ->
    case Code of
        Code when (Code div 100) == 2 -> ok;
        Code when Code >= 400, Code =< 409 -> retry;
        501 -> retry;
        503 -> retry;
        505 -> retry;
        _ -> error
    end.

-spec log_info(pos_integer(), term()) ->
   ok.
log_info(Code, Body) ->
    error_logger:info_msg(
        "Request completed with result:~n
        Code:~p~n
        Body:~p~n",
        [Code, Body]
    ).

-spec log_error(atom(), term()) ->
    ok.
log_error(retry, Error) ->
    error_logger:error_msg(
        "Error occured:~n
        Error: ~p~n,
        RETRYING~n",
        [Error]
    );
log_error(finish, Error) ->
    error_logger:error_msg(
        "Error occured:~n
        Error: ~p~n,
        RETRY TIMEOUT~n",
        [Error]
    ).

-spec log_error(atom(), pos_integer(), term()) ->
    ok.
log_error(retry, Code, Body) ->
    error_logger:error_msg(
        "Error occured:~n
        Code:~p~n
        Body:~p~n
        RETRYING~n",
        [Code, Body]
    );
log_error(finish, Code, Body) ->
    error_logger:error_msg(
        "Error occured:~n
        Code:~p~n
        Body:~p~n
        RETRY TIMEOUT~n",
        [Code, Body]
    ).
