-module(capi_bender_idemp).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type idempotant_key_prefix() :: binary() | atom().
-type external_id() :: binary().
-type issuer_id() :: dmsl_domain_thrift:'PartyID'() | dmsl_payment_processing_thrift:'UserID'().
-type idempotant_key() :: binary().
-type idempotent_key_params() :: idempotant_key() | {idempotant_key_prefix(), issuer_id(), external_id()}.
-type identity() :: {identity, identity_hash(), identity_features(), identity_schema()}.
-type identity_params() :: {schema, identity_schema(), capi_idemp_features:request()} | identity().
-type identity_hash() :: non_neg_integer().
-type identity_features() :: capi_idemp_features:features().
-type identity_schema() :: capi_idemp_features:schema().
-type woody_context() :: woody_context:ctx().
-type context_data() :: #{binary() => term()}.
%% -type sequence_params() :: #{minimum => integer()}.
%% -type difference() :: capi_idemp_features:difference() | undefined.

-export([try_gen_snowflake/3]).
-export([try_gen_snowflake/4]).
-export([build_idempotent_key/1]).
-export([build_identity/1]).

%% This module is WIP of changed capi_bender
%% Currently there's:
%% - code duplication due to generation of IdempotentKey for one-shot use
%% - code duplication due to explicit error handling via throw (no try_ alternative in API)
%% - not ideal parameter placement (no semantic grouping)
%%
%% Feedback is appreciated
%%
%% Envisioned usage:
%%
%% [try_]gen_snowflake(
%%         {<<"EncodeCustomerBindingParams">>, UserID, ExternalID},
%%         {schema, capi_feature_schemas:customer_binding_params(), Data},
%%         WoodyContext
%%         Context \\ #{}
%%        ),
%% [try_]gen_sequence(
%%         {<<"EncodeCustomerBindingParams">>, UserID, ExternalID},
%%         {schema, capi_feature_schemas:customer_binding_params(), Data},
%%         SequenceID,
%%         SequenceParams \\ #{},
%%         WoodyContext,
%%         Context \\ #{},
%%        ),
%% [try_]gen_constant(
%%         {<<"EncodeCustomerBindingParams">>, UserID, ExternalID},
%%         {schema, capi_feature_schemas:customer_binding_params(), Data},
%%         ConstantID,
%%         WoodyContext,
%%         Context \\ #{},
%%        ).

%% deprecated
-define(SCHEMA_VER1, 1).
-define(SCHEMA_VER2, 2).

-spec try_gen_snowflake(idempotent_key_params(), identity(), woody_context()) -> binary().
try_gen_snowflake(IdempotentKey, Identity, WoodyContext) ->
    try_gen_snowflake(IdempotentKey, Identity, WoodyContext, #{}).

-spec try_gen_snowflake(idempotent_key_params(), identity(), woody_context(), context_data()) -> binary().
try_gen_snowflake(IdempotentKey, Identity, WoodyContext, CtxData) ->
    IdSchema = {snowflake, #bender_SnowflakeSchema{}},
    try_generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, CtxData).

%%%
%%% Private
%%%
gen_external_id() ->
    genlib:unique().

-spec build_idempotent_key(idempotent_key_params()) -> idempotant_key().
build_idempotent_key(IdempotentKey) when is_binary(IdempotentKey) ->
    IdempotentKey;
build_idempotent_key({Prefix, PartyID, ExternalID}) when is_atom(Prefix) ->
    build_idempotent_key({atom_to_binary(Prefix, utf8), PartyID, ExternalID});
build_idempotent_key({Prefix, PartyID, undefined}) ->
    build_idempotent_key({Prefix, PartyID, gen_external_id()});
build_idempotent_key({Prefix, PartyID, ExternalID}) ->
    <<"capi/", Prefix/binary, "/", PartyID/binary, "/", ExternalID/binary>>.

-spec build_identity(identity_params()) -> identity().
build_identity({schema, Schema, Data}) ->
    Hash = erlang:phash2(Data),
    Features = capi_idemp_features:read(Schema, Data),
    {identity, Hash, Features, Schema};
build_identity(Identity = {identity, _Hash, _Features, _Schema}) ->
    Identity.

build_bender_ctx(Features, Ctx) ->
    #{
        <<"version">> => ?SCHEMA_VER2,
        <<"features">> => Features,
        <<"context_data">> => Ctx
    }.

get_external_id_or_idempotent_key({_BenderPrefix, _PartyOrUserID, ExternalID}) ->
    {external_id, ExternalID};
get_external_id_or_idempotent_key(IdempotentKey) when is_binary(IdempotentKey) ->
    {idempotent_key_params, IdempotentKey}.

try_generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, CtxData) ->
    case generate_id(IdSchema, IdempotentKey, Identity, WoodyContext, CtxData) of
        {ok, ID} ->
            ID;
        {error, {external_id_conflict, ID, undefined, Schema}} ->
            logger:warning("This externalID: ~p, used in another request.~n", [ID]),
            SourceID = get_external_id_or_idempotent_key(IdempotentKey),
            throw({external_id_conflict, ID, SourceID, Schema});
        {error, {external_id_conflict, ID, Difference, Schema}} ->
            ReadableDiff = capi_idemp_features:list_diff_fields(Schema, Difference),
            logger:warning("This externalID: ~p, used in another request.~nDifference: ~p", [ID, ReadableDiff]),
            SourceID = get_external_id_or_idempotent_key(IdempotentKey),
            throw({external_id_conflict, ID, SourceID, Schema})
    end.

generate_id(BenderIdSchema, IdempKeyParams, IdempIdentity, WoodyContext, CtxData) ->
    IdempKey = build_idempotent_key(IdempKeyParams),
    {identity, Hash, Features, Schema} = build_identity(IdempIdentity),
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
        {ok, ID, #{<<"version">> := ?SCHEMA_VER1} = DeprecatedCtx} ->
            check_idempotent_conflict_deprecated(ID, Hash, DeprecatedCtx, Schema);
        {ok, ID, #{<<"version">> := ?SCHEMA_VER2} = SavedBenderCtx} ->
            check_idempotent_conflict(ID, Features, SavedBenderCtx, Schema)
    end.

check_idempotent_conflict(ID, Features, SavedBenderCtx, Schema) ->
    #{
        <<"version">> := ?SCHEMA_VER2,
        <<"features">> := OtherFeatures
    } = SavedBenderCtx,
    case capi_idemp_features:compare(Features, OtherFeatures) of
        true ->
            {ok, ID};
        {false, Difference} ->
            {error, {external_id_conflict, ID, Difference, Schema}}
    end.

%% Deprecated idempotent context

check_idempotent_conflict_deprecated(ID, Hash, #{<<"params_hash">> := Hash}, _schema) ->
    {ok, ID};
check_idempotent_conflict_deprecated(ID, _Hash, #{<<"params_hash">> := _OtherHash}, Schema) ->
    {error, {external_id_conflict, ID, undefined, Schema}}.
