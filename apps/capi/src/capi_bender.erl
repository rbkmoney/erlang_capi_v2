-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().

-export([gen_by_snowflake/3]).
-export([gen_by_sequence/4]).
-export([get_idempotent_key/3]).

-define(SCHEMA_VER1, 1).

-spec gen_by_snowflake(binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_snowflake(IdempotentKey, Hash, ProcessContext) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    generate_id([IdempotentKey, Snowflake, Hash], ProcessContext).

-spec gen_by_sequence(binary(), binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_sequence(IdempotentKey, ParentID, Hash, ProcessContext) ->
    Sequence = {sequence, #bender_SequenceSchema{
        sequence_id = ParentID,
        minimum = 100
    }},
    generate_id([IdempotentKey, Sequence, Hash], ProcessContext).

-spec get_idempotent_key(binary(), binary(), binary() | undefined) ->
    binary().

get_idempotent_key(Prefix, PartyID, undefined) ->
    get_idempotent_key(Prefix, PartyID, gen_external_id());
get_idempotent_key(Prefix, PartyID, ExternalID) ->
    <<"capi-v2/", Prefix/binary, "/", PartyID/binary, "/", ExternalID/binary>>.

%% Internal

gen_external_id() ->
    % showflake:serialize(showflake:new()).
    genlib:unique().

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
        {ok, ID, _Other} -> {error, {external_id_conflict, ID}}
    end.
