%% @doc Top level supervisor.
%% @end

-module(capi_sup).
-behaviour(supervisor).

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
    HealthRoutes = [{'_', [erl_health_handle:get_route(genlib_app:env(capi, health_checkers, []))]}],
    SwaggerSpec = capi_swagger_server:child_spec({HealthRoutes, LogicHandler}),
    BlacklistSpecs = [capi_api_key_blacklist:child_spec()],
    {ok, {
        {one_for_all, 0, 1},
            AuthorizerSpecs ++ BlacklistSpecs ++ LogicHandlerSpecs ++ [SwaggerSpec]
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
