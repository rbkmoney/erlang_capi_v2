-module(capi_dummy_service).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), #{}) ->
    {ok, term()}.

handle_function(Function, _, _, #{result := Result}) ->
    case is_function(Result) of
        true ->
            Result(Function);
        _ ->
            Result
    end.