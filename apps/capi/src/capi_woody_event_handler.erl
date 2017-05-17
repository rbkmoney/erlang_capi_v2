-module(capi_woody_event_handler).
-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").
-export([handle_event/4]).

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType, RpcID, EventMeta, _Opts) ->
    Msg = woody_event_handler:format_event(EventType, EventMeta, RpcID),
    format_event(EventType, RpcID, EventMeta, #{event => EventType}, Msg).

-define(SCOPE, 'rpc.client').

%% common

format_event(EventType, RpcID, _EventMeta, MD, Msg) when
    EventType == ?EV_INTERNAL_ERROR;
    EventType == ?EV_TRACE
->
    log(RpcID, MD, Msg);

%% client

format_event(?EV_CALL_SERVICE, RpcID, #{
    service  := Service,
    function := Function,
    type     := Type,
    metadata := Metadata
}, MD, Msg) ->
    ok = enter(?SCOPE, maps:merge(Metadata, #{
        service  => Service,
        function => Function,
        type     => Type
    })),
    log(RpcID, MD, Msg);

format_event(?EV_CLIENT_SEND, RpcID, #{}, MD, Msg) ->
    log(RpcID, MD, Msg);

format_event(?EV_CLIENT_RECEIVE, RpcID, #{status := Status}, MD, Msg) ->
    log(RpcID, MD#{status => Status}, Msg);

format_event(?EV_SERVICE_RESULT, RpcID, #{status := Status}, MD, Msg) ->
    ok = log(RpcID, MD#{status => Status}, Msg),
    leave(?SCOPE);

%% server

format_event(_EventType, _RpcID, _EventMeta, _MD, _Msg) ->
    % skip safely, there's no woody servers around
    ok.

%%

log(RpcID, MD, {Level, {Format, Args}}) ->
    ok = set_md(?SCOPE, MD),
    _ = lager:log(Level, [{pid, self()}] ++ collect_md(RpcID), Format, Args),
    ok = unset_md(?SCOPE, MD),
    ok.

collect_md(MD = #{}) ->
    maps:fold(
        fun (K, V, Acc) -> lists:keystore(K, 1, Acc, {K, V}) end,
        lager:md(),
        MD
    ).

%%

enter(Name, Meta) ->
    lager:md(orddict:store(Name, maps:merge(find_md(Name), Meta), lager:md())).

set_md(Name, Meta) ->
    enter(Name, Meta).

unset_md(Name, Meta) ->
    lager:md(orddict:store(Name, maps:without(maps:keys(Meta), find_md(Name)), lager:md())).

leave(Name) ->
    lager:md(orddict:erase(Name, lager:md())).

find_md(Name) ->
    case orddict:find(Name, lager:md()) of
        {ok, V = #{}} -> V;
        error         -> #{}
    end.
