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
    LechiffreOpts = genlib_app:env(capi, lechiffre_opts),
    LechiffreSpec = lechiffre:child_spec(lechiffre, LechiffreOpts),
    {LogicHandler, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthCheck = enable_health_logging(genlib_app:env(?APP, health_check, #{})),
    AdditionalRoutes = [{'_', [erl_health_handle:get_route(HealthCheck), get_prometheus_route()]}],
    SwaggerHandlerOpts = genlib_app:env(?APP, swagger_handler_opts, #{}),
    SwaggerSpec = capi_swagger_server:child_spec({AdditionalRoutes, LogicHandler, SwaggerHandlerOpts}),
    BlacklistSpecs = capi_api_key_blacklist:child_spec(),
    UacConf = get_uac_config(),
    ok = uac:configure(UacConf),
    {ok, {
        {one_for_all, 0, 1},
        [LechiffreSpec] ++ LogicHandlerSpecs ++ [SwaggerSpec] ++ [BlacklistSpecs]
    }}.

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []}.
get_logic_handler_info() ->
    case genlib_app:env(?APP, service_type) of
        real ->
            {capi_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

get_uac_config() ->
    maps:merge(
        genlib_app:env(capi, access_conf),
        #{access => capi_auth:get_access_config()}
    ).

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.
