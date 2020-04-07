-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().
-type context_data() :: #{binary() => term()}.
-type bender_context() :: #{binary() => term()}.
-type sequence_params() :: #{minimum => integer()}.

-type params() :: #{
    legacy => integer(),
    current => binary()
}.

-export_type([
    bender_context/0,
    context_data/0
]).

-export([gen_by_snowflake/4]).
-export([gen_by_snowflake/3]).
-export([gen_by_sequence/4]).
-export([gen_by_sequence/5]).
-export([gen_by_sequence/6]).
-export([gen_by_constant/4]).
-export([gen_by_constant/5]).
-export([get_idempotent_key/3]).
-export([get_internal_id/2]).

-define(SCHEMA_VER1, 1). %% deprecated
-define(SCHEMA_VER2, 2).

-spec gen_by_snowflake(binary(), integer(), woody_context()) ->
    {ok, binary()} |
    {ok, binary(), binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_snowflake(IdempotentKey, Hash, WoodyContext) ->
    gen_by_snowflake(IdempotentKey, Hash, WoodyContext, #{}).

-spec gen_by_snowflake(binary(), integer(), woody_context(), context_data()) ->
    {ok, binary()} |
    {ok, binary(), binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_snowflake(IdempotentKey, Hash, WoodyContext, CtxData) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    generate_id(IdempotentKey, Snowflake, Hash, WoodyContext, CtxData).

-spec gen_by_sequence(binary(), binary(), params(), woody_context()) ->
    {ok, binary()} |
    {ok, binary(), binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext) ->
    gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, #{}).

-spec gen_by_sequence(binary(), binary(), params(), woody_context(), context_data()) ->
    {ok, binary()} |
    {external_id_busy, binary(), binary()}|
    {error, {external_id_conflict, binary()}}. %% legacy delete

gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, CtxData) ->
    gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, CtxData, #{}).

-spec gen_by_sequence(binary(), binary(), params(), woody_context(), context_data(), sequence_params()) ->
    {ok, binary()} |
    {ok, binary(), binary()} |
    {error, {external_id_conflict, binary()}}. %% legacy delete

gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, CtxData, SeqParams) ->
    Minimum = maps:get(minimum, SeqParams, undefined),
    Sequence = {sequence, #bender_SequenceSchema{
        sequence_id = SequenceID,
        minimum = Minimum
    }},
    generate_id(IdempotentKey, Sequence, Params, WoodyContext, CtxData).

-spec gen_by_constant(binary(), binary(), integer(), woody_context()) ->
    {ok,    binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_constant(IdempotentKey, ConstantID, Hash, WoodyContext) ->
    gen_by_constant(IdempotentKey, ConstantID, Hash, WoodyContext, #{}).

-spec gen_by_constant(binary(), binary(), integer(), woody_context(), context_data()) ->
    {ok,    binary()} |
    {error, {external_id_conflict, binary()}}.

gen_by_constant(IdempotentKey, ConstantID, Hash, WoodyContext, CtxData) ->
    Constant = {constant, #bender_ConstantSchema{internal_id = ConstantID}},
    generate_id(IdempotentKey, Constant, Hash, WoodyContext, CtxData).

-spec get_idempotent_key(atom() | binary(), binary(), binary() | undefined) ->
    binary().

get_idempotent_key(Prefix, PartyID, ExternalID) when is_atom(Prefix) ->
    get_idempotent_key(atom_to_binary(Prefix, utf8), PartyID, ExternalID);
get_idempotent_key(Prefix, PartyID, undefined) ->
    get_idempotent_key(Prefix, PartyID, gen_external_id());
get_idempotent_key(Prefix, PartyID, ExternalID) ->
    <<"capi/", Prefix/binary, "/", PartyID/binary, "/", ExternalID/binary>>.

-spec get_internal_id(binary(), woody_context()) ->
    {ok, binary(), context_data()} | {error, internal_id_not_found}.

get_internal_id(ExternalID, WoodyContext) ->
    case capi_woody_client:call_service(bender, 'GetInternalID', [ExternalID], WoodyContext) of
        {ok, #bender_GetInternalIDResult{
            internal_id = InternalID,
            context = Context
        }} ->
            UnmarshaledCtx = capi_msgp_marshalling:unmarshal(Context),
            {ok, InternalID, get_context_data(UnmarshaledCtx)};
        {exception, #bender_InternalIDNotFound{}} ->
            {error, internal_id_not_found}
    end.

%% Internal

gen_external_id() ->
    genlib:unique().

generate_id(Key, BenderSchema, Params, WoodyContext, CtxData) ->
    #{
        legacy := Hash,
        current := Bin
    } = Params,
    Context = capi_msgp_marshalling:marshal(#{
        <<"version">>       => ?SCHEMA_VER2,
        <<"params_bin">>    => Bin,
        <<"context_data">>  => CtxData
    }),
    Args = [Key, BenderSchema, Context],
    Result = case capi_woody_client:call_service(bender, 'GenerateID', Args, WoodyContext) of
        {ok, #bender_GenerationResult{internal_id = InternalID, context = undefined}} -> {ok, InternalID};
        {ok, #bender_GenerationResult{internal_id = InternalID, context = Ctx}} ->
            {ok, InternalID, capi_msgp_marshalling:unmarshal(Ctx)}
    end,
    case Result of
        {ok, ID} ->
            {ok, ID};
        {ok, ID, #{<<"version">> := ?SCHEMA_VER1} = DeprecatedCtx} ->
            idepmotent_conflict_legacy(ID, Hash, DeprecatedCtx);
        {ok, ID, #{<<"version">> := ?SCHEMA_VER2, <<"params_bin">> := ParamsBin}} ->
            {ok, ID, ParamsBin}
    end.

idepmotent_conflict_legacy(ID, Hash, #{<<"params_hash">> := Hash}) ->
    {ok, ID};
idepmotent_conflict_legacy(ID, _Hash, #{<<"params_hash">> := _OtherHash}) ->
    {error, {external_id_conflict, ID}}.

-spec get_context_data(bender_context()) -> undefined | context_data().

get_context_data(Context) ->
    maps:get(<<"context_data">>, Context, #{}).