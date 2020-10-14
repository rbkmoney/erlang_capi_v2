%% @doc Public API and application startup.
%% @end

-module(capi).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%%

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    capi_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
