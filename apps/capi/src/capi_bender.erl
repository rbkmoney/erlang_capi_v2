-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().

-export([gen_by_snowflake/3]).
-export([gen_by_sequence/4]).

-define(SCHEMA_VER1, <<"ver1">>).

-spec gen_by_snowflake(binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, external_id_conflict}.

gen_by_snowflake(IdempotentKey, Hash, ProcessContext) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    generate_id([IdempotentKey, Snowflake, Hash], ProcessContext).

-spec gen_by_sequence(binary(), binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, external_id_conflict}.

gen_by_sequence(IdempotentKey, ParentID, Hash, ProcessContext) ->
    Sequence = {sequence, #bender_SequenceSchema{
        sequence_id = ParentID,
        minimum = 100
    }},
    generate_id([IdempotentKey, Sequence, Hash], ProcessContext).

%% Internal
generate_id([Key, BenderSchema, Hash], ProcessContext) ->
    Context = capi_msgp_marshalling:marshal(#{
        <<"version">>     => ?SCHEMA_VER1,
        <<"params_hash">> => Hash
    }),
    Args = [Key, BenderSchema, Context],
    Result = case capi_woody_client:call_service(bender, 'GenerateID', Args, ProcessContext) of
        {ok, #bender_GenerationResult{internal_id = InternalID, context = undefined}} -> {ok, InternalID};
        {ok, #bender_GenerationResult{internal_id = InternalID, context = Ctx}}       ->
            #{<<"params_hash">> := BenderHash} = capi_msgp_marshalling:unmarshal(Ctx),
            {ok, InternalID, BenderHash}
    end,
    case Result of
        {ok, ID}        -> {ok, ID};
        {ok, ID, Hash}  -> {ok, ID};
        {ok, _, _Other} -> {error, external_id_conflict}
    end.
