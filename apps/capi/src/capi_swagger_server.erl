-module(capi_swagger_server).

-export([child_spec /1]).

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
    ranch:child_spec(?MODULE,
        Transport, TransportOpts, cowboy_clear, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port     = genlib_app:env(?APP, port, ?DEFAULT_PORT),
    AcceptorsPool = genlib_app:env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    {ranch_tcp, #{socket_opts => [{ip, IP}, {port, Port}], num_acceptors => AcceptorsPool}}.

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
        ],
        stream_handlers => [
            cowboy_stream_h, cowboy_access_log_h, capi_stream_handler
        ],
        sink => capi
    }.

squash_routes(Routes) ->
    orddict:to_list(lists:foldl(
        fun ({K, V}, D) -> orddict:update(K, fun (V0) -> V0 ++ V end, V, D) end,
        orddict:new(),
        Routes
    )).
