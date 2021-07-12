-module(capi_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([get_pi_realm/2]).
-export([get_payment_institutions/1]).
-export([get/2]).
-export([get_objects_by_type/2]).

-type processing_context() :: capi_handler:processing_context().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-type payment_institution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitutionObject'().
-type realm() :: dmsl_domain_thrift:'PaymentInstitutionRealm'().

-export_type([realm/0]).

-spec get_pi_realm(payment_institution_ref(), processing_context()) -> {ok, realm()}.
get_pi_realm(Ref, Context) ->
    {ok, PiObject} = get({payment_institution, Ref}, Context),
    #domain_PaymentInstitutionObject{data = Pi} = PiObject,
    #domain_PaymentInstitution{realm = Realm} = Pi,
    {ok, Realm}.

-spec get_payment_institutions(processing_context()) -> {ok, [payment_institution()]}.
get_payment_institutions(Context) ->
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
                {payment_institution, Object} = dmt_client:checkout_object(Version, {payment_institution, Ref}, Opts),
                Object
            end,
            PaymentInstitutionRefs
        ),

    {ok, PaymentInstitutions}.

-spec get(ref(), processing_context()) -> {ok, data()} | {error, not_found}.
get(Ref, Context) ->
    try
        Opts = make_opts(Context),
        {_Type, Object} = dmt_client:checkout_object(latest, Ref, Opts),
        {ok, Object}
    catch
        throw:#'ObjectNotFound'{} ->
            {error, not_found}
    end.

globals() ->
    {globals, #domain_GlobalsRef{}}.

-spec get_objects_by_type(Type :: atom(), processing_context()) -> {ok, [dmsl_domain_thrift:'DomainObject'()]}.
get_objects_by_type(Type, Context) ->
    Opts = make_opts(Context),
    Objects = dmt_client:checkout_objects_by_type(latest, Type, Opts),
    {ok, Objects}.

-spec make_opts(processing_context()) -> dmt_client:opts().
make_opts(#{woody_context := WoodyContext}) ->
    #{woody_context => WoodyContext}.
