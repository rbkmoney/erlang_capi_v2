-module(capi_domain).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

-export([get_categories/1]).
-export([get_payment_institutions/1]).
-export([get_default_payment_institution_ref/2]).
-export([get/2]).

-type context() :: woody_context:ctx().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-type category() :: dmsl_domain_thrift:'CategoryObject'().
-type payment_institution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type payment_institution_realm() :: dmsl_domain_thrift:'PaymentInstitutionRealm'().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitutionObject'().

-spec get_categories(context()) -> {ok, [category()]}.
get_categories(Context) ->
    {ok, Snapshot} = get_shapshot(Context),
    #'Snapshot'{
        domain = Domain
    } = Snapshot,

    Categories = maps:fold(
        fun
            ({'category', _}, {'category', CategoryObject}, Acc) ->
                [CategoryObject | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Domain
    ),
    {ok, Categories}.

-spec get_payment_institutions(context()) -> {ok, [payment_institution()]}.

get_payment_institutions(Context) ->
    % All this mess was done to reduce requests to dominant.
    % TODO rewrite this with dmt_client, cache, unicorns and rainbows.
    {ok, Snapshot} = get_shapshot(Context),
    #'Snapshot'{
        domain = Domain
    } = Snapshot,
    {globals, #domain_GlobalsObject{data = Globals}} = genlib_map:get({globals, #domain_GlobalsRef{}}, Domain),
    {ok, get_payment_institutions(Globals, Domain)}.

get_payment_institutions(#domain_Globals{payment_institutions = PaymentInstitutionRefs}, Domain)
    when PaymentInstitutionRefs /= undefined
->
    lists:map(
        fun(Ref) ->
            {_, Obj} = genlib_map:get({payment_institution, Ref}, Domain),
            Obj
        end,
        ordsets:to_list(PaymentInstitutionRefs)
    );
get_payment_institutions(#domain_Globals{payment_institutions = undefined}, _) ->
    [].

-spec get_default_payment_institution_ref(payment_institution_realm(), context()) -> payment_institution_ref().

get_default_payment_institution_ref(Realm, Context) ->
    #domain_Globals{
        contract_payment_institution_defaults = PaymentInstitutionDefaults
    } = checkout_globals(Context),
    case Realm of
        test ->
            PaymentInstitutionDefaults#domain_ContractPaymentInstitutionDefaults.test;
        live ->
            PaymentInstitutionDefaults#domain_ContractPaymentInstitutionDefaults.live
    end.

-spec get(ref(), context()) -> {ok, data()} | {error, not_found}.

get(Ref, Context) ->
    {ok, Snapshot} = get_shapshot(Context),
    #'Snapshot'{
        domain = Domain
    } = Snapshot,
    case genlib_map:get(Ref, Domain) of
        {_, C} ->
            {ok, C};
        undefined ->
            {error, not_found}
    end.

head() ->
    {'head', #'Head'{}}.

get_shapshot(Context) ->
    get_shapshot(head(), Context).

get_shapshot(Reference, Context) ->
    capi_woody_client:call_service(repository, 'Checkout', [Reference], Context).

checkout_object(Context, ObjectRef) ->
    checkout_object(head(), ObjectRef, Context).

checkout_object(Reference, ObjectRef, Context) ->
    capi_woody_client:call_service(repository_client, 'checkoutObject', [Reference, ObjectRef], Context).

checkout_globals(Context) ->
    {ok, #'VersionedObject'{
        object = {globals, #domain_GlobalsObject{data = Globals}}
    }} = checkout_object(Context, {globals, #domain_GlobalsRef{}}),
    Globals.
