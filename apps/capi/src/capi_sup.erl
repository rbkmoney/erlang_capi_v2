%% @doc Top level supervisor.
%% @end

-module(capi_sup).
-behaviour(supervisor).

-define(APP, capi).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    AuthorizerSpecs = get_authorizer_child_specs(),
    {LogicHandler, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthRoutes = [{'_', [erl_health_handle:get_route(genlib_app:env(?APP, health_checkers, []))]}],
    SwaggerHandlerOpts = genlib_app:env(?APP, swagger_handler_opts, #{}),
    SwaggerSpec = capi_swagger_server:child_spec({HealthRoutes, LogicHandler, SwaggerHandlerOpts}),
    {ok, {
        {one_for_all, 0, 1},
            AuthorizerSpecs ++ LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_authorizer_child_specs() -> [supervisor:child_spec()].

get_authorizer_child_specs() ->
    Authorizers = genlib_app:env(?APP, authorizers, #{}),
    [
        get_authorizer_child_spec(jwt, maps:get(jwt, Authorizers))
    ].

-spec get_authorizer_child_spec(Name :: atom(), Options :: #{}) -> supervisor:child_spec().

get_authorizer_child_spec(jwt, Options) ->
    capi_authorizer_jwt:get_child_spec(Options).

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []} .

get_logic_handler_info() ->
    case genlib_app:env(?APP, service_type) of
        real ->
            {capi_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.
