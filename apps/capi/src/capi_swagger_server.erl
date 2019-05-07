-module(capi_swagger_server).

-export([child_spec   /1]).
% -export([request_hook /1]).
% -export([response_hook/4]).

-define(APP, capi).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-define(START_TIME_TAG, processing_start_time).

-type params() :: {cowboy_router:routes(), module()}.

-spec child_spec(params()) ->
    supervisor:child_spec().
child_spec({HealthRoutes, LogicHandler}) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(HealthRoutes, LogicHandler),
    AcceptorsPool = genlib_app:env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    ranch:child_spec(?MODULE, AcceptorsPool,
        Transport, TransportOpts, cowboy_protocol, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port     = genlib_app:env(?APP, port, ?DEFAULT_PORT),
    {ranch_tcp, [{ip, IP}, {port, Port}]}.

get_cowboy_config(HealthRoutes, LogicHandler) ->
    Dispatch =
        cowboy_router:compile(squash_routes(
            HealthRoutes ++
            swag_server_router:get_paths(LogicHandler)
        )),
    #{
        env => #{
            dispatch => Dispatch,
            cors_policy => capi_cors_policy
        },
        middlewares => [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ]
    }.

squash_routes(Routes) ->
    orddict:to_list(lists:foldl(
        fun ({K, V}, D) -> orddict:update(K, fun (V0) -> V0 ++ V end, V, D) end,
        orddict:new(),
        Routes
    )).

% -spec request_hook(cowboy_req:req()) ->
%     cowboy_req:req().

% request_hook(Req) ->
%     cowboy_req:set_meta(?START_TIME_TAG, genlib_time:ticks(), Req).

% -spec response_hook(cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
%     cowboy_req:req().

% response_hook(Code, Headers, Body, Req) ->
%     try
%         {Code1, Headers1, Req1} = handle_response(Code, Headers, Req),
%         _ = log_access(Code1, Headers1, Body, Req1),
%         Req1
%     catch
%         Class:Reason:Stacktrace ->
%             Stack = genlib_format:format_stacktrace(StackTrace, [newlines]),
%             _ = lager:warning(
%                 "Response hook failed for: [~p, ~p, ~p]~nwith: ~p:~p~nstacktrace: ~ts",
%                 [Code, Headers, Req, Class, Reason, Stack]
%             ),
%             Req
%     end.



% log_access(Code, Headers, Body, Req) ->
%     LogFun = cowboy_access_log:get_response_hook(capi_access_lager_event),
%     LogFun(Code, Headers, Body, Req).
