-module(capi_swagger_server).

-export([child_spec/1]).
-export([response_hook/4]).

%% cowboy_req:reply/4 has a faulty spec in case of response body fun.
-dialyzer({[no_contracts, no_fail_call], response_hook/4}).

-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_OOPS_BODY_FILE, "priv/oops_body").

-type params() :: module().

-spec response_hook(cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
    cowboy_req:req().

response_hook(500, Headers, _, Req) ->
    {ok, Req1} = cowboy_req:reply(500, Headers, get_oops_body(), Req),
    Req1;
response_hook(_, _, _, Req) ->
    Req.

-spec child_spec(params()) -> supervisor:child_spec().

child_spec(LogicHandler) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(LogicHandler),
    AcceptorsPool = genlib_app:env(?MODULE, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    ranch:child_spec(?MODULE, AcceptorsPool,
        Transport, TransportOpts, cowboy_protocol, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?MODULE, ip, ?DEFAULT_IP_ADDR)),
    Port     = genlib_app:env(capi, port, ?DEFAULT_PORT),
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

get_oops_body() ->
    File = genlib_app:env(?MODULE, oops_body_file, ?DEFAULT_OOPS_BODY_FILE),
    FileSize = filelib:file_size(File),
    F = fun(Socket, Transport) ->
            Transport:sendfile(Socket, File)
    end,
    {FileSize, F}.
