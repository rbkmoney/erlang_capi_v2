-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type woody_context() :: woody_context:ctx().
-type context_data() :: #{binary() => term()}.
-type bender_context() :: #{binary() => term()}.
-type sequence_params() :: #{minimum => integer()}.
-type difference() :: {capi_idemp_features:difference(), capi_idemp_features:schema()} | undefined.

-type params()       :: {integer(), capi_idemp_features:features()}.
-type params_value() :: term().

-export_type([bender_context/0]).
-export_type([context_data/0]).
-export_type([difference/0]).

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

-spec gen_by_snowflake(binary(), params(), woody_context()) ->
    {ok, binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_snowflake(IdempotentKey, Params, WoodyContext) ->
    gen_by_snowflake(IdempotentKey, Params, WoodyContext, #{}).

-spec gen_by_snowflake(binary(), params(), woody_context(), context_data()) ->
    {ok, binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_snowflake(IdempotentKey, Params, WoodyContext, CtxData) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    generate_id(IdempotentKey, Snowflake, Params, WoodyContext, CtxData).

-spec gen_by_sequence(binary(), binary(), params(), woody_context()) ->
    {ok, binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext) ->
    gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, #{}).

-spec gen_by_sequence(binary(), binary(), params(), woody_context(), context_data()) ->
    {ok, binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, CtxData) ->
    gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, CtxData, #{}).

-spec gen_by_sequence(binary(), binary(), params(), woody_context(), context_data(), sequence_params()) ->
    {ok, binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_sequence(IdempotentKey, SequenceID, Params, WoodyContext, CtxData, SeqParams) ->
    Minimum = maps:get(minimum, SeqParams, undefined),
    Sequence = {sequence, #bender_SequenceSchema{
        sequence_id = SequenceID,
        minimum = Minimum
    }},
    generate_id(IdempotentKey, Sequence, Params, WoodyContext, CtxData).

-spec gen_by_constant(binary(), binary(), params(), woody_context()) ->
    {ok,    binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_constant(IdempotentKey, ConstantID, Params, WoodyContext) ->
    gen_by_constant(IdempotentKey, ConstantID, Params, WoodyContext, #{}).

-spec gen_by_constant(binary(), binary(), params(), woody_context(), context_data()) ->
    {ok,    binary()} |
    {ok, binary(), params_value()} |
    {error, {external_id_conflict, binary(), difference()}}.

gen_by_constant(IdempotentKey, ConstantID, Params, WoodyContext, CtxData) ->
    Constant = {constant, #bender_ConstantSchema{internal_id = ConstantID}},
    generate_id(IdempotentKey, Constant, Params, WoodyContext, CtxData).

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

generate_id(Key, BenderSchema, {Hash, Features}, WoodyContext, CtxData) ->
    BenderCtx = create_bender_ctx(Features, CtxData),
    Args = [Key, BenderSchema, capi_msgp_marshalling:marshal(BenderCtx)],
    Result = case capi_woody_client:call_service(bender, 'GenerateID', Args, WoodyContext) of
        {ok, #bender_GenerationResult{internal_id = InternalID, context = undefined}} -> {ok, InternalID};
        {ok, #bender_GenerationResult{internal_id = InternalID, context = Ctx}} ->
            {ok, InternalID, capi_msgp_marshalling:unmarshal(Ctx)}
    end,
    case Result of
        {ok, ID} ->
            {ok, ID};
        {ok, ID, #{<<"version">> := ?SCHEMA_VER1} = DeprecatedCtx} ->
            check_idempotent_conflict_deprecated(ID, Hash, DeprecatedCtx);
        {ok, ID, #{<<"version">> := ?SCHEMA_VER2} = SavedBenderCtx} ->
            check_idempotent_conflict(ID, BenderCtx, SavedBenderCtx)
    end.

create_bender_ctx(Features, Ctx) ->
    #{
        <<"version">>      => ?SCHEMA_VER2,
        <<"features">>     => Features,
        <<"context_data">> => Ctx
    }.

features(#{<<"version">> := ?SCHEMA_VER2, <<"features">> := Features}) ->
    Features.

check_idempotent_conflict(ID, BenderCtx, SavedBenderCtx) ->
    Features = features(BenderCtx),
    OtherFeatures = features(SavedBenderCtx),
    case capi_idemp_features:equal_features(Features, OtherFeatures) of
        true ->
            {ok, ID};
        {false, Difference} ->
            {error, {external_id_conflict, ID, Difference}}
    end.

%% Deprecated idempotent context

check_idempotent_conflict_deprecated(ID, Hash, #{<<"params_hash">> := Hash}) ->
    {ok, ID};
check_idempotent_conflict_deprecated(ID, _Hash, #{<<"params_hash">> := _OtherHash}) ->
    {error, {external_id_conflict, ID, undefined}}.

-spec get_context_data(bender_context()) -> undefined | context_data().

get_context_data(Context) ->
    maps:get(<<"context_data">>, Context, #{}).
