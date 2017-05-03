-module(capi_swagger_server).

-export([child_spec/1]).
-export([response_hook/4]).

-define(APP, capi).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-type params() :: module().

-spec child_spec(params()) -> supervisor:child_spec().

child_spec(LogicHandler) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(LogicHandler),
    AcceptorsPool = genlib_app:env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    ranch:child_spec(?MODULE, AcceptorsPool,
        Transport, TransportOpts, cowboy_protocol, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port     = genlib_app:env(?APP, port, ?DEFAULT_PORT),
    {ranch_tcp, [{ip, IP}, {port, Port}]}.

get_cowboy_config(LogicHandler) ->
    Dispatch = cowboy_router:compile(swagger_router:get_paths(LogicHandler)),
    [
        {env, [
            {dispatch, Dispatch},
            {cors_policy, capi_cors_policy}
        ]},
        {middlewares, [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ]},
        {onresponse, fun ?MODULE:response_hook/4}
    ].

-spec response_hook(cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
    cowboy_req:req().

response_hook(Code, Headers, _, Req) when Code >= 500 ->
    send_oops_resp(Code, Headers, get_oops_body_safe(Code), Req);
response_hook(_, _, _, Req) ->
    Req.

%% cowboy_req:reply/4 has a faulty spec in case of response body fun.
-dialyzer({[no_contracts, no_fail_call], send_oops_resp/4}).

send_oops_resp(_, _, undefined, Req) ->
    Req;
send_oops_resp(Code, Headers, File, Req) ->
    FileSize = filelib:file_size(File),
    F = fun(Socket, Transport) ->
        case Transport:sendfile(Socket, File) of
            {ok, _} ->
                ok;
            {error, Error} ->
                _ = lager:warning("Failed to send oops body: ~p", [Error]),
                ok
        end
    end,
    Headers1 = lists:keystore(<<"content-type">>, 1, Headers,
        {<<"content-type">>, <<"text/plain; charset=utf-8">>}),
    {ok, Req1} = cowboy_req:reply(Code, Headers1, {FileSize, F}, Req),
    Req1.

get_oops_body_safe(Code) ->
    try get_oops_body(Code)
    catch
        Error:Reason ->
            _ = lager:warning("Invalid oops body config for code: ~p. Error: ~p:~p", [Code, Error, Reason]),
            undefined
    end.

get_oops_body(Code) ->
    genlib_map:get(Code, genlib_app:env(?APP, oops_bodies, #{}), undefined).
