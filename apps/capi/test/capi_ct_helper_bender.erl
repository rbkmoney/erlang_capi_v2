-module(capi_ct_helper_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").

-export([get_result/1]).
-export([get_result/2]).

-spec get_result(binary()) -> any().
-spec get_result(binary(), msgpack_thrift:'Value'() | undefined) -> bender_thrift:bender_GenerationResult().

-define(BENDER(InternalID, Context), #bender_GenerationResult{
    internal_id = InternalID,
    context     = Context
}).

get_result(ID) ->
    ?BENDER(ID, undefined).

get_result(ID, Context) ->
    ?BENDER(ID, Context).
