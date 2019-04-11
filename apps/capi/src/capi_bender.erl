-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().

-export([gen_by_snowflake/3]).
-export([gen_by_sequence/4]).

-spec gen_by_snowflake(binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, external_id_conflict}.

gen_by_snowflake(IdempotentKey, Hash, ProcessContext) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    Context = capi_msgp_marshalling:marshal(#{<<"params_hash">> => Hash}),
    generate_id([IdempotentKey, Snowflake, Context], ProcessContext).

-spec gen_by_sequence(binary(), binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, external_id_conflict}.

gen_by_sequence(IdempotentKey, ParentID, Hash, ProcessContext) ->
    Sequince = {sequince, #bender_SequenceSchema{
        sequence_id = ParentID,
        minimum = 100
    }},
    Context = capi_msgp_marshalling:marshal(#{<<"params_hash">> => Hash}),
    generate_id([IdempotentKey, Sequince, Context], ProcessContext).

%% Internal
generate_id([_, _, Context] = Args, ProcessContext) ->
    case capi_woody_client:call_service(bender, 'GenerateID', Args, ProcessContext) of
        {ok, #bender_GenerationResult{internal_id = ID, context = undefined}} ->
            {ok, ID};
        {ok, #bender_GenerationResult{internal_id = ID, context = Context}} ->
            {ok, ID};
        {ok, _Other} ->
            {error, external_id_conflict}
    end.
