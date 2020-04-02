-module(capi_ct_helper_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").

-type tid() :: any().
-type internal_id() :: binary().
-type msg_pack() :: msgpack_thrift:'Value'().

-export([get_result/1]).
-export([get_result/2]).
-export([get_internal_id_result/2]).
-export([no_internal_id/0]).
-export([create_storage/0]).
-export([del_storage/1]).
-export([compare_context/3]).

-spec create_storage() -> tid().
-spec del_storage(tid()) -> ok.
-spec compare_context(tid(), internal_id(), msg_pack()) -> bender_thrift:bender_GenerationResult().

-spec get_result(binary()) -> bender_thrift:bender_GenerationResult().
-spec get_result(binary(), msgpack_thrift:'Value'() | undefined) -> bender_thrift:bender_GenerationResult().
-spec get_internal_id_result(binary(), msgpack_thrift:'Value'() | undefined) ->
    bender_thrift:bender_GetInternalIDResult().
-spec no_internal_id() -> bender_thrift:'InternalIDNotFound'().


create_storage() ->
    ets:new(bender_storage, [set, public]).

del_storage(Tid) ->
    ets:delete(Tid).

compare_context(Tid, InternalID, MsgPack) ->
    Ctx = capi_msgp_marshalling:unmarshal(MsgPack),
    ParamsBin = maps:get(<<"params_bin">>, Ctx),
    case ets:lookup(Tid, key) of
        [] ->
            ets:insert(Tid, {key, #{
                params_bin => ParamsBin,
                ctx => MsgPack
            }}),
            {ok, get_result(InternalID)};
        [{key, #{params_bin := ParamsBin, ctx := Ctx}}] ->
            {ok, get_result(InternalID, Ctx)};
        [{key, #{ctx := OtherCtx}}] ->
            {ok, get_result(InternalID, OtherCtx)}
    end.

get_result(ID) ->
    get_result(ID, undefined).

get_result(ID, Context) ->
    #bender_GenerationResult{
        internal_id = ID,
        context     = Context
}.

get_internal_id_result(ID, Ctx) ->
    #bender_GetInternalIDResult{
        internal_id = ID,
        context = Ctx
    }.

no_internal_id() ->
    #bender_InternalIDNotFound{}.
