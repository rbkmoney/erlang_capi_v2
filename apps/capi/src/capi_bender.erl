-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().
-type request_data()  :: capi_hanlder:request_data().

-export([get_id/3]).

-spec get_id(binary(), request_data(), woody_context()) ->
    {ok, binary()} |
    {error, external_id_conflict}.

get_id(IdempKey, Params, ProcessContext) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    Ctx = capi_msgp_marshalling:marshal(#{<<"params_hash">> => erlang:phash2(Params)}),
    Args = [IdempKey, Snowflake, Ctx],
    case capi_woody_client:call_service(bender, 'GenerateID', Args, ProcessContext) of
        {ok, #bender_GenerationResult{internal_id = ID, context = undefined}} ->
            {ok, ID};
        {ok, #bender_GenerationResult{internal_id = ID, context = Ctx}} ->
            {ok, ID};
        {ok, _Other} ->
            {error, external_id_conflict}
    end.
