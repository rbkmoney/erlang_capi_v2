-module(capi_woody_event_handler).
-behaviour(woody_event_handler).

-export([handle_event/4]).

%%

-spec handle_event(woody_event_handler:event(), woody:rpc_id(), woody_event_handler:event_meta(), _) ->
    _.

handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}, _) ->
    lager:error(
        maps:to_list(RpcID),
        "[server] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );

handle_event(EventType, RpcID, EventMeta, _) ->
    lager:debug(maps:to_list(RpcID), "[server] ~s: ~p", [EventType, EventMeta]).
