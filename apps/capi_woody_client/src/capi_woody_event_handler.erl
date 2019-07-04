-module(capi_woody_event_handler).
-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").
-export([handle_event/4]).

-spec handle_event(Event, RpcID, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcID :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType, RpcID, EventMeta, _Opts) ->
    Msg = woody_event_handler:format_event(EventType, EventMeta, RpcID),
    format_event(EventType, RpcID, EventMeta, Msg).

%% common

-define(CLIENT, 'rpc.client').

format_event(EventType, RpcID, _EventMeta, Msg) when
    EventType == ?EV_INTERNAL_ERROR;
    EventType == ?EV_TRACE
->
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType
    }));

%% client

format_event(EventType = ?EV_CALL_SERVICE, RpcID, #{
    service  := Service,
    function := Function,
    type     := Type,
    metadata := Metadata
}, Msg) ->
    ok = enter(?CLIENT, maps:merge(Metadata, #{
        service  => Service,
        function => Function,
        type     => Type
    })),
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType
    }));

format_event(EventType = ?EV_CLIENT_SEND, RpcID, #{}, Msg) ->
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType
    }));

format_event(EventType = ?EV_CLIENT_RECEIVE, RpcID, #{status := Status}, Msg) ->
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType,
        status => Status
    }));

format_event(EventType = ?EV_SERVICE_RESULT, RpcID, #{status := Status}, Msg) ->
    _ = log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType,
        status => Status
    })),
    leave(?CLIENT);

%% server

format_event(_EventType, _RpcID, _EventMeta, _Msg) ->
    % skip safely, there's no woody servers around
    ok.

%%

log(RpcID, {Level, {Format, Args}}, MD) ->
    logger:log(Level, Format, Args, maps:merge(MD, rpc_id_to_md(RpcID))).

rpc_id_to_md(undefined) ->
    #{};
rpc_id_to_md(RpcID = #{}) ->
    RpcID.

%%

enter(Name, Meta) ->
    logger:update_process_metadata(collect(Name, Meta)).

leave(Name) ->
    logger:set_process_metadata(maps:remove(Name, capi_utils:get_process_metadata())).

collect(Name, Meta) ->
    % basically an update?
    PreparedMeta = maps:merge(find_scope(Name), Meta),
    maps:put(Name, PreparedMeta, capi_utils:get_process_metadata()).

find_scope(Name) ->
    case maps:find(Name, capi_utils:get_process_metadata()) of
        {ok, V = #{}} -> V;
        error         -> #{}
    end.