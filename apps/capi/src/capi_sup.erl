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
    ok = capi_auth:init(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {LogicHandler, LogicHandlerSpec} = get_logic_handler_info(),
    {ok, IP} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    SwaggerSpec = swagger_server:child_spec(swagger, #{
        ip                => IP,
        port              => genlib_app:env(capi, port, 8080),
        net_opts          => [],
        logic_handler     => LogicHandler,
        cowboy_extra_opts => get_cowboy_extra_opts()
    }),
    {ok, {
        {one_for_all, 0, 1}, [SwaggerSpec | LogicHandlerSpec]
    }}.

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
