-module(capi_client_lib).

-export([get_context/5]).
-export([get_context/6]).
-export([login/1]).
-export([handle_response/1]).
-export([make_request/2]).
-export([make_analytics_search_query_string/1]).
-export([retry/3]).
-export([get_timestamp/0]).
-export([default_event_handler/0]).

-type context() :: #{
    url           := string(),
    token         := term(),
    retries       := integer(),
    timeout       := integer(),
    event_handler := event_handler(),
    protocol      := protocol()
}.
-export_type([context/0]).

-type event_handler() :: fun((event_type(), code(), duration()) -> ok).
-export_type([event_handler/0]).

-type event_type() :: atom().
-type code()       :: pos_integer().
-type duration()   :: non_neg_integer().

-type analytics_search_query() :: list().
-export_type([analytics_search_query/0]).

-type analytics_search_query_string() :: map().
-export_type([analytics_search_query_string/0]).

-type idempotency() :: idempotent | non_idempotent.
-export_type([idempotency/0]).

-type header() :: {binary(), binary()}.
-export_type([header/0]).

-type protocol() :: ipv4 | ipv6.
-export_type([protocol/0]).

-type protocol_opts() :: [{connect_options, [inet4 | inet6]}].
-export_type([protocol_opts/0]).

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

-spec protocol_to_opt(protocol()) ->
    protocol_opts().
protocol_to_opt(ipv4) ->
    [{connect_options, [inet4]}];
protocol_to_opt(ipv6) ->
    [{connect_options, [inet6]}].

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

-spec make_analytics_search_query_string(analytics_search_query()) -> analytics_search_query_string().
make_analytics_search_query_string(ParamList) ->
    lists:foldl(fun(Elem, Acc) -> maps:merge(Acc, prepare_param(Elem)) end, #{}, ParamList).

-spec prepare_param({atom(), term()}) ->
    map().
prepare_param(Param) ->
    case Param of
        {limit, P}          -> #{<<"limit">> => genlib:to_binary(P)};
        {offset, P}         -> #{<<"offset">> => genlib:to_binary(P)};
        {from_time, P}      -> #{<<"fromTime">> => genlib_format:format_datetime_iso8601(P)};
        {to_time, P}        -> #{<<"toTime">> => genlib_format:format_datetime_iso8601(P)};
        {status, P}         -> #{<<"status">> => genlib:to_binary(P)};
        {split_unit, P}     -> #{<<"splitUnit">> => genlib:to_binary(P)};
        {split_size, P}     -> #{<<"splitSize">> => genlib:to_binary(P)};
        {payment_method, P} -> #{<<"paymentMethod">> => genlib:to_binary(P)};
        {ParamName, P}      -> #{genlib:to_binary(ParamName) => P}
    end.

-spec make_request(context(), map()) ->
    {string(), map(), list()}.
make_request(Context, ParamsList) ->
    {Url, Headers} = get_http_params(Context),
    Opts           = get_hackney_opts(Context),
    PreparedParams = make_params(Headers, ParamsList),
    {Url, PreparedParams, Opts}.

-spec make_params(list(), map()) ->
    map().
make_params(Headers, RequestParams) ->
    Params = #{
        header  => maps:from_list(Headers),
        binding => #{},
        body    => #{},
        qs_val  => #{}
    },
    maps:merge(Params, RequestParams).

-spec handle_response({atom(), Code::integer(), RespHeaders::list(), Body::term()}) ->
    {ok, term()} | {error, term()}.
handle_response(Response) ->
    case Response of
        {ok, Code, _, Body} -> handle_response(Code, Body);
        {error, Error}      -> {error, Error}
    end.

-spec handle_response(integer(), term()) ->
    {ok, term()} | {error, term()}.
handle_response(204, _) ->
    {ok, undefined};
handle_response(Code, Body) when Code div 100 == 2 ->
    %% 2xx HTTP code
    {ok, decode_body(Body)};
handle_response(_, Body) ->
    {error, Body}.

-spec get_context(string(), term(), integer(), integer(), protocol()) ->
    context().
get_context(Url, Token, Retries, Timeout, Protocol) ->
    get_context(Url, Token, Retries, Timeout, Protocol, default_event_handler()).

-spec get_context(string(), term(), integer(), integer(), protocol(), event_handler()) ->
    context().
get_context(Url, Token, Retries, Timeout, Protocol, EventHandler) ->
    #{
        url           => Url,
        token         => Token,
        retries       => Retries,
        timeout       => Timeout,
        protocol      => Protocol,
        event_handler => EventHandler
    }.

-spec default_event_handler() ->
    event_handler().
default_event_handler() ->
    fun(_Type, _Code, _Duration) ->
        ok
    end.

-spec get_body(term()) ->
    term().
get_body(ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.

-spec get_http_params(context()) ->
    {string(), list()}.
get_http_params(Context) ->
    Url     = maps:get(url, Context),
    Headers = headers(Context),
    {Url, Headers}.

-spec get_hackney_opts(context()) ->
    list().
get_hackney_opts(Context) ->
    protocol_to_opt(maps:get(protocol, Context, ipv4)).

-spec headers(context()) ->
    list(header()).
headers(Context) ->
  [
        x_request_id_header(),
        auth_header(maps:get(token, Context)) | json_accept_headers()
  ].

-spec x_request_id_header() ->
    header().
x_request_id_header() ->
    {<<"X-Request-ID">>, integer_to_binary(rand:uniform(100000))}.

-spec auth_header(term()) ->
    header().
auth_header(Token) ->
    {<<"Authorization">>, <<"Bearer ", Token/binary>>} .

-spec json_accept_headers() ->
    list(header()).
json_accept_headers() ->
    [
        {<<"Accept">>, <<"application/json">>},
        {<<"Accept-Charset">>, <<"UTF-8">>},
        {<<"Content-Type">>, <<"application/json; charset=UTF-8">>}
    ].

-spec decode_body(term()) ->
    term().
decode_body(Body) when is_binary(Body) ->
    jsx:decode(Body, [return_maps]);
decode_body(Body) ->
    Body.

-spec get_timestamp() ->
    integer().
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).
