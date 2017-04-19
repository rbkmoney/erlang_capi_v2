%% @doc Top level supervisor.
%% @end

-module(capi_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-define(DEFAULT_SWAG_POOL_OPTS, #{
    max_count     => 10,
    init_count    => 10,
    cull_interval => {0, min}
}).

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    AuthorizerSpecs = get_authorizer_child_specs(),
    {LogicHandler, LogicHandlerSpecs} = get_logic_handler_info(),
    {ok, IP} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    PoolOpts = genlib_app:env(swagger, validator_pool_opts, ?DEFAULT_SWAG_POOL_OPTS),
    SwaggerSpec = swagger_server:child_spec(swagger, #{
        ip                => IP,
        port              => genlib_app:env(capi, port, 8080),
        net_opts          => [],
        logic_handler     => LogicHandler,
        cowboy_extra_opts => get_cowboy_extra_opts(),
        validator_pool_opts => PoolOpts
    }),
    {ok, {
        {one_for_all, 0, 1},
            AuthorizerSpecs ++ LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_authorizer_child_specs() -> [supervisor:child_spec()].

get_authorizer_child_specs() ->
    Authorizers = genlib_app:env(capi, authorizers, #{}),
    [
        get_authorizer_child_spec(jwt, maps:get(jwt, Authorizers))
    ].

-spec get_authorizer_child_spec(Name :: atom(), Options :: #{}) -> supervisor:child_spec().

get_authorizer_child_spec(jwt, Options) ->
    capi_authorizer_jwt:get_child_spec(Options).

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []} .

get_logic_handler_info() ->
    case genlib_app:env(capi, service_type) of
        mock ->
            Spec = genlib_app:permanent(
                {capi_mock_handler, capi_mock_handler, start_link},
                none,
                []
            ),
            {capi_mock_handler, [Spec]};
        real ->
            {capi_real_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.

get_cowboy_extra_opts() ->
    [
        {env, [{cors_policy, capi_cors_policy}]},
        {middlewares, [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
       ]}
    ].
