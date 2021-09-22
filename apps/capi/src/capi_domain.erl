-module(capi_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([get_payment_institution/2]).
-export([get_payment_institutions/1]).
-export([get/2]).
-export([get_objects_by_type/2]).
-export([map_to_dictionary_id/2]).
-export([map_to_dictionary_id/3]).
-export([encode_enum/2]).
-export([encode_enum/3]).
-export([fetch_type_info/1]).
-export([fetch_type_info/2]).
-export([extract_type/1]).

-type processing_context() :: capi_handler:processing_context().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type payment_institution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type payment_institution_object() :: dmsl_domain_thrift:'PaymentInstitutionObject'().
-type realm() :: dmsl_domain_thrift:'PaymentInstitutionRealm'().

-export_type([realm/0]).

-spec get_payment_institution(payment_institution_ref(), processing_context()) ->
    {ok, payment_institution()} | {error, not_found}.
get_payment_institution(Ref, Context) ->
    case get({payment_institution, Ref}, Context) of
        {ok, PiObject} ->
            {ok, PiObject#domain_PaymentInstitutionObject.data};
        Error ->
            Error
    end.

-spec get_payment_institutions(processing_context()) -> {ok, [payment_institution_object()]} | {error, not_found}.
get_payment_institutions(Context) ->
    try
        Opts = make_opts(Context),

        #'VersionedObject'{
            version = Version,
            object = {globals, #domain_GlobalsObject{data = Globals}}
        } = dmt_client:checkout_versioned_object(latest, globals(), Opts),

        PaymentInstitutionRefs =
            case Globals#domain_Globals.payment_institutions of
                undefined -> [];
                List -> List
            end,

        PaymentInstitutions =
            lists:map(
                fun(Ref) ->
                    {payment_institution, Object} = dmt_client:checkout_object(
                        Version, {payment_institution, Ref}, Opts
                    ),
                    Object
                end,
                PaymentInstitutionRefs
            ),

        {ok, PaymentInstitutions}
    catch
        throw:#'ObjectNotFound'{} ->
            {error, not_found}
    end.

-spec get(ref(), processing_context() | undefined) -> {ok, data()} | {error, not_found}.
get(Ref, Context) ->
    try
        Opts = make_opts(Context),
        {_Type, Object} = dmt_client:checkout_object(latest, Ref, Opts),
        {ok, Object}
    catch
        throw:#'ObjectNotFound'{} ->
            {error, not_found}
    end.

-spec map_to_dictionary_id(dmt_client:object_type(), ID :: undefined | term()) -> NewRef :: undefined | term().
map_to_dictionary_id(ObjectName, LegacyID) ->
    map_to_dictionary_id(ObjectName, LegacyID, undefined).

-spec map_to_dictionary_id(dmt_client:object_type(), ID :: undefined | term(), NewRef) -> NewRef when
    NewRef :: term() | no_return().
map_to_dictionary_id(ObjectName, LegacyID, NewRef) ->
    DictionaryID =
        case LegacyID of
            undefined ->
                NewRef;
            _ ->
                case fetch_dictionary_id(ObjectName, LegacyID) of
                    {ok, Ref} when Ref =:= NewRef; NewRef =:= undefined ->
                        Ref;
                    {ok, OtherRef} ->
                        logger:warning(
                            "Mismatch in mapped to dictionary value and passed ~p: ~p->~p and ~p."
                            "Using dictionary value (the latter)",
                            [ObjectName, LegacyID, OtherRef, NewRef]
                        ),
                        OtherRef;
                    {error, _} = Error ->
                        logger:warning(
                            "Failed to find mapping ~w with id ~w: "
                            "chance of domain misconifuration (Error: ~w). Using passed new ref",
                            [ObjectName, LegacyID, Error]
                        ),
                        NewRef
                end
        end,
    case DictionaryID of
        undefined ->
            error({no_mapping, {ObjectName, LegacyID, DictionaryID}});
        DictionaryID ->
            DictionaryID
    end.

fetch_dictionary_id(ObjectName, LegacyID) ->
    {dmsl_domain_thrift, RefRecordShortName} =
        extract_type(fetch_type_info([{variant, ObjectName}, {field, ref}])),
    RefRecordName = dmsl_domain_thrift:record_name(RefRecordShortName),
    case get({ObjectName, {RefRecordName, LegacyID}}, undefined) of
        {ok, Data} ->
            {DataFieldID, _Kind, _Info, data, _Default} =
                fetch_type_info([{variant, ObjectName}, {field, data}]),
            {ok, element(1 + DataFieldID, Data)};
        {error, _} = Error ->
            Error
    end.

-spec encode_enum(Type :: atom(), binary()) -> {ok, atom()} | {error, unknown_atom | unknown_variant}.
encode_enum(Type, Binary) ->
    encode_enum(dmsl_domain_thrift, Type, Binary).
-spec encode_enum(Module :: atom(), Type :: atom(), binary()) -> {ok, atom()} | {error, unknown_atom | unknown_variant}.
encode_enum(Module, Type, Binary) ->
    try erlang:binary_to_existing_atom(Binary, utf8) of
        Atom ->
            {enum, Variants} = Module:enum_info(Type),
            case lists:keyfind(Atom, 1, Variants) of
                false -> {error, unknown_variant};
                _ -> {ok, Atom}
            end
    catch
        error:badarg ->
            {error, unknown_atom}
    end.

-spec fetch_type_info([{variant, atom()} | {field, atom()}]) -> term().
fetch_type_info(Path) ->
    fetch_type_info('DomainObject', Path).

-spec fetch_type_info(TypeInfo :: term(), [{variant, atom()} | {field, atom()}]) -> term().
fetch_type_info(TypeInfo, []) ->
    TypeInfo;
fetch_type_info(TypeInfo, [{variant, Variant} | Rest]) ->
    fetch_type_info(fetch_struct_field(TypeInfo, union, Variant), Rest);
fetch_type_info(TypeInfo, [{field, Field} | Rest]) ->
    fetch_type_info(fetch_struct_field(TypeInfo, struct, Field), Rest);
fetch_type_info(_, _) ->
    error(badarg).

fetch_struct_field(TypeInfo, Kind, Field) ->
    {Module, Type} = extract_type(TypeInfo),
    {struct, Kind, Fields} = Module:struct_info(Type),
    lists:keyfind(Field, 4, Fields).

-type type() :: atom().
-type type_info() ::
    type() | {module(), type()} | dmsl_domain_thrift:struct_info().

-spec extract_type(type_info()) -> {module(), type()}.
extract_type(Name) when is_atom(Name) ->
    {dmsl_domain_thrift, Name};
extract_type({_ID, _Kind, TypeInfo, _Field, _Default}) ->
    extract_type(TypeInfo);
extract_type(Type = {Module, Name}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({struct, struct, Type = {Module, Name}}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({enum, Type = {Module, Name}}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type(_) ->
    error(badarg).

globals() ->
    {globals, #domain_GlobalsRef{}}.

-spec get_objects_by_type(Type :: atom(), processing_context()) -> {ok, [dmsl_domain_thrift:'DomainObject'()]}.
get_objects_by_type(Type, Context) ->
    Opts = make_opts(Context),
    Objects = dmt_client:checkout_objects_by_type(latest, Type, Opts),
    {ok, Objects}.

-spec make_opts(processing_context()) -> dmt_client:opts().
make_opts(#{woody_context := WoodyContext}) ->
    #{woody_context => WoodyContext};
make_opts(_) ->
    #{}.
