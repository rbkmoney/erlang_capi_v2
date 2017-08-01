-module(capi_client_lib).

-export([get_context/5]).
-export([get_context/6]).

-export([handle_response/1]).
-export([make_request/2]).

-export([make_search_query_string/1]).
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

-type search_query() :: list().
-export_type([search_query/0]).

-type query_string() :: map().
-export_type([query_string/0]).

-type header() :: {binary(), binary()}.
-export_type([header/0]).

-type protocol() :: ipv4 | ipv6.
-export_type([protocol/0]).

-type protocol_opts() :: [{connect_options, [inet4 | inet6]}].
-export_type([protocol_opts/0]).

-spec protocol_to_opt(protocol()) ->
    protocol_opts().
protocol_to_opt(ipv4) ->
    [{connect_options, [inet4]}];
protocol_to_opt(ipv6) ->
    [{connect_options, [inet6]}].

-spec make_search_query_string(search_query()) -> query_string().
make_search_query_string(ParamList) ->
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
handle_response(Code, _) when Code =:= 202; Code =:= 204 ->
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
