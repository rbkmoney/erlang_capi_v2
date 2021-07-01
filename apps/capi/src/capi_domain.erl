-module(capi_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([get_payment_institutions/1]).
-export([get/2]).
-export([get_objects_by_type/2]).

-type context() :: woody_context:ctx().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-type payment_institution() :: #domain_PaymentInstitutionObject{}.

-spec get_payment_institutions(context()) -> {ok, [payment_institution()]}.
get_payment_institutions(Context) ->
    Opts = #{woody_context => Context},

    #'VersionedObject'{
        version = Version,
        object = {globals, Globals}
    } = dmt_client:checkout_versioned_object(globals(), Opts),

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

-spec get(ref(), context()) -> {ok, data()} | {error, not_found}.
get(Ref, Context) ->
    try
        {_Type, Object} = dmt_client:checkout_object(Ref, #{woody_context => Context}),
        {ok, Object}
    catch
        error:#'ObjectNotFound'{} ->
            {error, not_found}
    end.

globals() ->
    {globals, #domain_GlobalsRef{}}.

-spec get_objects_by_type(context(), Type :: atom()) -> {ok, [dmsl_domain_thrift:'DomainObject'()]}.
get_objects_by_type(Type, Context) ->
    Objects = dmt_client:checkout_objects_by_type(Type, #{woody_context => Context}),
    {ok, Objects}.
