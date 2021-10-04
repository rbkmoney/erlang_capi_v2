-module(capi_bender).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type id() :: binary().
-type idempotent_key_prefix() :: binary() | atom().
-type external_id() :: binary().
-type issuer_id() :: dmsl_domain_thrift:'PartyID'() | dmsl_payment_processing_thrift:'UserID'().
-type idempotent_key() :: binary().
-type idempotent_key_params() :: {idempotent_key_prefix(), issuer_id(), external_id() | undefined}.
-type identity() :: {identity, identity_features(), identity_schema()}.
-type identity_params() ::
    {schema, identity_schema(), feat:request()}
    | {schema, identity_schema(), feat:request(), HashedRequest :: feat:request()}
    | identity().

-type identity_features() :: feat:features().
-type identity_schema() :: feat:schema().
-type woody_context() :: woody_context:ctx().
-type context_data() :: #{binary() => term()}.
-type bender_context() :: #{binary() => term()}.

-type sequence_id() :: binary().
-type sequence_params() :: #{minimum => integer()}.
-type difference() :: feat:difference().

-type constant_id() :: binary().

-type external_id_conflict_v1() :: {external_id_conflict, id(), undefined, identity_schema()}.
-type external_id_conflict_v2() :: {external_id_conflict, id(), difference(), identity_schema()}.
-type external_id_conflict() :: external_id_conflict_v1() | external_id_conflict_v2().
-type generation_error() :: external_id_conflict().

-export_type([id/0]).
-export_type([external_id/0]).
-export_type([issuer_id/0]).
-export_type([idempotent_key_params/0]).
-export_type([identity_params/0]).
-export_type([context_data/0]).
-export_type([woody_context/0]).
-export_type([difference/0]).
-export_type([sequence_id/0]).
-export_type([sequence_params/0]).
-export_type([constant_id/0]).
-export_type([external_id_conflict/0]).
-export_type([bender_context/0]).

-export([gen_snowflake/3]).
-export([gen_snowflake/4]).
-export([try_gen_snowflake/3]).
-export([try_gen_snowflake/4]).
-export([gen_sequence/4]).
-export([gen_sequence/5]).
-export([gen_sequence/6]).
-export([try_gen_sequence/6]).
-export([gen_constant/4]).
-export([gen_constant/5]).
-export([try_gen_constant/4]).
-export([try_gen_constant/5]).
-export([make_identity/1]).
-export([get_internal_id/2]).

%% deprecated
-define(SCHEMA_VER2, 2).
-define(SCHEMA_VER3, 3).

-spec gen_snowflake(idempotent_key_params() | undefined, identity_params(), woody_context()) ->
    {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_snowflake(IdempotentKey, Identity, WoodyContext) ->
    Context = #{},
    gen_snowflake(IdempotentKey, Identity, WoodyContext, Context).

-spec gen_snowflake(idempotent_key_params() | undefined, identity_params(), woody_context(), context_data()) ->
    {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_snowflake(IdempotentKey, Identity, WoodyContext, Context) ->
    IdSchema = {snowflake, #bender_SnowflakeSchema{}},
    generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, Context).

-spec try_gen_snowflake(idempotent_key_params() | undefined, identity_params(), woody_context()) -> id().
try_gen_snowflake(IdempotentKey, Identity, WoodyContext) ->
    Context = #{},
    try_gen_snowflake(IdempotentKey, Identity, WoodyContext, Context).

-spec try_gen_snowflake(idempotent_key_params() | undefined, identity_params(), woody_context(), context_data()) ->
    id().
try_gen_snowflake(IdempotentKey, Identity, WoodyContext, Context) ->
    IdSchema = {snowflake, #bender_SnowflakeSchema{}},
    try_generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, Context).

-spec gen_sequence(idempotent_key_params() | undefined, identity_params(), sequence_id(), woody_context()) ->
    {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_sequence(IdempotentKey, Identity, SequenceID, WoodyContext) ->
    SequenceParams = #{},
    gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyContext).

-spec gen_sequence(
    idempotent_key_params() | undefined,
    identity_params(),
    sequence_id(),
    sequence_params(),
    woody_context()
) -> {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyContext) ->
    Context = #{},
    gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyContext, Context).

-spec gen_sequence(
    idempotent_key_params() | undefined,
    identity_params(),
    sequence_id(),
    sequence_params(),
    woody_context(),
    context_data()
) -> {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyContext, Context) ->
    IdSchema = build_sequence_schema(SequenceID, SequenceParams),
    generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, Context).

-spec try_gen_sequence(
    idempotent_key_params() | undefined,
    identity_params(),
    sequence_id(),
    sequence_params(),
    woody_context(),
    context_data()
) -> id() | no_return().
try_gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyContext, ContextData) ->
    IdSchema = build_sequence_schema(SequenceID, SequenceParams),
    try_generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, ContextData).

-spec gen_constant(idempotent_key_params(), identity_params(), constant_id(), woody_context()) ->
    {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_constant(IdempotentKey, Identity, ConstantID, WoodyContext) ->
    Context = #{},
    gen_constant(IdempotentKey, Identity, ConstantID, WoodyContext, Context).

-spec gen_constant(idempotent_key_params(), identity_params(), constant_id(), woody_context(), context_data()) ->
    {ok, id()} | {ok, id(), context_data()} | {error, generation_error()}.
gen_constant(IdempotentKey, Identity, ConstantID, WoodyContext, Context) ->
    IdSchema = {constant, #bender_ConstantSchema{internal_id = ConstantID}},
    generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, Context).

-spec try_gen_constant(idempotent_key_params(), identity_params(), constant_id(), woody_context()) -> id().
try_gen_constant(IdempotentKey, Identity, ConstantID, WoodyContext) ->
    Context = #{},
    try_gen_constant(IdempotentKey, Identity, ConstantID, WoodyContext, Context).

-spec try_gen_constant(idempotent_key_params(), identity_params(), constant_id(), woody_context(), context_data()) ->
    id().
try_gen_constant(IdempotentKey, Identity, ConstantID, WoodyContext, Context) ->
    IdSchema = {constant, #bender_ConstantSchema{internal_id = ConstantID}},
    try_generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, Context).

-spec make_identity(identity_params()) -> identity().
make_identity({schema, Schema, Data}) ->
    Features = feat:read(Schema, Data),
    {identity, Features, Schema};
make_identity(Identity = {identity, _Features, _Schema}) ->
    Identity.

-spec get_internal_id(idempotent_key_params(), woody_context()) ->
    {ok, binary(), context_data()} | {error, internal_id_not_found}.
get_internal_id(IdempotentKeyParams, WoodyContext) ->
    IdempotentKey = make_idempotent_key(IdempotentKeyParams),
    case capi_woody_client:call_service(bender, 'GetInternalID', {IdempotentKey}, WoodyContext) of
        {ok, #bender_GetInternalIDResult{
            internal_id = InternalID,
            context = Context
        }} ->
            UnmarshaledCtx = capi_msgp_marshalling:unmarshal(Context),
            {ok, InternalID, get_context_data(UnmarshaledCtx)};
        {exception, #bender_InternalIDNotFound{}} ->
            {error, internal_id_not_found}
    end.

%%%
%%% Private
%%%

build_sequence_schema(SequenceID, SequenceParams) ->
    Minimum = maps:get(minimum, SequenceParams, undefined),
    {sequence, #bender_SequenceSchema{
        sequence_id = SequenceID,
        minimum = Minimum
    }}.

build_bender_ctx(Features, Ctx) ->
    #{
        <<"version">> => ?SCHEMA_VER3,
        <<"features">> => Features,
        <<"context_data">> => Ctx
    }.

get_external_id({_BenderPrefix, _PartyOrUserID, ExternalID}) ->
    ExternalID.

try_generate_id(BenderIdSchema, IdempotentKey, Identity, WoodyContext, CtxData) ->
    case generate_id(BenderIdSchema, IdempotentKey, Identity, WoodyContext, CtxData) of
        {ok, ID} ->
            ID;
        {error, {external_id_conflict, ID, undefined, Schema}} ->
            logger:warning("This externalID: ~p, used in another request.~n", [ID]),
            SourceID = get_external_id(IdempotentKey),
            throw({external_id_conflict, ID, SourceID, Schema});
        {error, {external_id_conflict, ID, Difference, Schema}} ->
            ReadableDiff = feat:list_diff_fields(Schema, Difference),
            logger:warning("This externalID: ~p, used in another request.~nDifference: ~p", [ID, ReadableDiff]),
            SourceID = get_external_id(IdempotentKey),
            throw({external_id_conflict, ID, SourceID, Schema})
    end.

generate_id(BenderIdSchema, IdempKeyParams, IdempIdentity, WoodyContext, CtxData) ->
    IdempKey = make_idempotent_key(IdempKeyParams),
    case IdempKey of
        undefined -> generator_generate_id(BenderIdSchema, WoodyContext);
        IdempKey -> bender_generate_id(BenderIdSchema, IdempKey, IdempIdentity, WoodyContext, CtxData)
    end.

-spec make_idempotent_key(idempotent_key_params()) -> idempotent_key() | undefined.
make_idempotent_key({Prefix, PartyID, ExternalID}) when is_atom(Prefix) ->
    make_idempotent_key({atom_to_binary(Prefix, utf8), PartyID, ExternalID});
make_idempotent_key({_Prefix, _PartyID, undefined}) ->
    %% If external ID is undefined, no reason to generate it: noone can really use it
    undefined;
make_idempotent_key({Prefix, PartyID, ExternalID}) ->
    <<"capi/", Prefix/binary, "/", PartyID/binary, "/", ExternalID/binary>>.

bender_generate_id(BenderIdSchema, IdempKey, IdempIdentity, WoodyContext, CtxData) ->
    {identity, Features, Schema} = make_identity(IdempIdentity),
    BenderCtx = build_bender_ctx(Features, CtxData),
    Args = {IdempKey, BenderIdSchema, capi_msgp_marshalling:marshal(BenderCtx)},
    Result =
        case capi_woody_client:call_service(bender, 'GenerateID', Args, WoodyContext) of
            {ok, #bender_GenerationResult{internal_id = InternalID, context = undefined}} ->
                {ok, InternalID};
            {ok, #bender_GenerationResult{internal_id = InternalID, context = Ctx}} ->
                {ok, InternalID, capi_msgp_marshalling:unmarshal(Ctx)}
        end,
    case Result of
        {ok, ID} ->
            {ok, ID};
        {ok, ID, #{<<"version">> := ?SCHEMA_VER2} = SavedBenderCtx} ->
            check_idempotent_conflict_deprecated_v2(ID, Features, SavedBenderCtx, Schema);
        {ok, ID, #{<<"version">> := ?SCHEMA_VER3} = SavedBenderCtx} ->
            check_idempotent_conflict(ID, Features, SavedBenderCtx, Schema)
    end.

generator_generate_id(BenderIDSchema, WoodyContext) ->
    case BenderIDSchema of
        {snowflake, #bender_SnowflakeSchema{}} ->
            {ok, {ID, _}} = bender_generator_client:gen_snowflake(WoodyContext),
            {ok, ID};
        {sequence, #bender_SequenceSchema{
            sequence_id = SequenceID,
            minimum = Minimum
        }} ->
            {ok, {ID, _}} = bender_generator_client:gen_sequence(SequenceID, WoodyContext, #{minimum => Minimum}),
            {ok, ID};
        {constant, #bender_ConstantSchema{internal_id = InternalID}} ->
            {ok, InternalID}
    end.

check_idempotent_conflict(ID, Features, SavedBenderCtx, Schema) ->
    #{
        <<"version">> := ?SCHEMA_VER3,
        <<"features">> := OtherFeatures
    } = SavedBenderCtx,
    case feat:compare(Features, OtherFeatures) of
        true ->
            {ok, ID};
        {false, Difference} ->
            {error, {external_id_conflict, ID, Difference, Schema}}
    end.

%% Deprecated idempotent context

check_idempotent_conflict_deprecated_v2(ID, Features, SavedBenderCtx, Schema) ->
    #{
        <<"version">> := ?SCHEMA_VER2,
        <<"features">> := OtherFeatures
    } = SavedBenderCtx,
    case capi_legacy_idemp_features:compare(Features, OtherFeatures) of
        true ->
            {ok, ID};
        {false, Difference} ->
            {error, {external_id_conflict, ID, Difference, Schema}}
    end.

-spec get_context_data(bender_context()) -> undefined | context_data().
get_context_data(Context) ->
    maps:get(<<"context_data">>, Context, #{}).
