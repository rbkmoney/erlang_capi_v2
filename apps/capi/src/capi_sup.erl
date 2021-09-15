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
    HealthCheck = enable_health_logging(genlib_app:env(?APP, health_check, #{})),
    PartyClient = party_client:create_client(),
    PartyClientSpec = party_client:child_spec(party_client, PartyClient),
    {LogicHandler, []} = get_logic_handler_info(#{party_client => PartyClient}),
    AdditionalRoutes = [{'_', [erl_health_handle:get_route(HealthCheck), get_prometheus_route()]}],
    SwaggerHandlerOpts = genlib_app:env(?APP, swagger_handler_opts, #{}),
    SwaggerSpec = capi_swagger_server:child_spec(AdditionalRoutes, LogicHandler, SwaggerHandlerOpts),
    {ok,
        {
            {one_for_all, 0, 1},
            [LechiffreSpec, SwaggerSpec, PartyClientSpec]
        }}.

-spec get_logic_handler_info(capi_handler:handler_opts()) ->
    {Handler :: swag_server:logic_handler(_), [Spec :: supervisor:child_spec()] | []}.
get_logic_handler_info(HandlerOpts) ->
    {{capi_handler, HandlerOpts}, []}.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.
