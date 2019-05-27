-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().

-export([gen_by_snowflake/3]).
-export([gen_by_sequence/4]).
-export([gen_by_constant/4]).
-export([get_idempotent_key/3]).

-define(SCHEMA_VER1, 1).

-spec gen_by_snowflake(binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_snowflake(IdempotentKey, Hash, WoodyCtx) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    generate_id(IdempotentKey, Snowflake, Hash, WoodyCtx).

-spec gen_by_sequence(binary(), binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_sequence(IdempotentKey, SequenceID, Hash, WoodyCtx) ->
    Sequence = {sequence, #bender_SequenceSchema{
        sequence_id = SequenceID,
        minimum = 100
    }},
    generate_id(IdempotentKey, Sequence, Hash, WoodyCtx).


-spec gen_by_constant(binary(), binary(), integer(), woody_context()) ->
    {ok,    binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_constant(IdempotentKey, ConstantID, Hash, WoodyCtx) ->
    Constant = {constant, #bender_ConstantSchema{internal_id = ConstantID}},
    generate_id(IdempotentKey, Constant, Hash, WoodyCtx).


-spec get_idempotent_key(atom() | binary(), binary(), binary() | undefined) ->
    binary().

get_idempotent_key(Prefix, PartyID, ExternalID) when is_atom(Prefix) ->
    get_idempotent_key(atom_to_binary(Prefix, utf8), PartyID, ExternalID);
get_idempotent_key(Prefix, PartyID, undefined) ->
    get_idempotent_key(Prefix, PartyID, gen_external_id());
get_idempotent_key(Prefix, PartyID, ExternalID) ->
    <<"capi/", Prefix/binary, "/", PartyID/binary, "/", ExternalID/binary>>.

%% Internal

gen_external_id() ->
    genlib:unique().

generate_id(Key, BenderSchema, Hash, WoodyCtx) ->
    Context = capi_msgp_marshalling:marshal(#{
        <<"version">>     => ?SCHEMA_VER1,
        <<"params_hash">> => Hash
    }),
    Args = [Key, BenderSchema, Context],
    Result = case capi_woody_client:call_service(bender, 'GenerateID', Args, WoodyCtx) of
        {ok, #bender_GenerationResult{internal_id = InternalID, context = undefined}} -> {ok, InternalID};
        {ok, #bender_GenerationResult{internal_id = InternalID, context = Ctx}}       ->
            #{<<"params_hash">> := BenderHash} = capi_msgp_marshalling:unmarshal(Ctx),
            {ok, InternalID, BenderHash}
    end,
    case Result of
        {ok, ID}         -> {ok, ID};
        {ok, ID, Hash}   -> {ok, ID};
        {ok, ID, _Other} -> {error, {external_id_conflict, ID}}
    end.