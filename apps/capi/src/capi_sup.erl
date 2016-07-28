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

-spec init([]) -> {ok, tuple()}.
init([]) ->
    {LogicHandler, LogicHandlerSpec} = get_logic_handler_info(),
    SwaggerSpec = swagger_server:child_spec(swagger, #{
        ip            => capi_utils:get_hostname_ip(genlib_app:env(capi, host, "0.0.0.0")),
        port          => genlib_app:env(capi, port, 8080),
        net_opts      => [],
        logic_handler => LogicHandler
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
        undefined -> exit(undefined_service_type)
    end.
