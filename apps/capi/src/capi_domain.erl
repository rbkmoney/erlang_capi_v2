-module(capi_domain).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

-export([get_categories/1]).
-export([get_payment_institutions/1]).
-export([get/2]).

-type context() :: woody_context:ctx().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-type category() :: #domain_CategoryObject{}.
-type payment_institution() :: #domain_PaymentInstitutionObject{}.

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
    {ok, case Globals#domain_Globals.payment_institutions of
        PaymentInstitutionRefs when PaymentInstitutionRefs /= undefined ->
            lists:map(
                fun(Ref) ->
                    {_, Obj} = genlib_map:get({payment_institution, Ref}, Domain),
                    Obj
                end,
                ordsets:to_list(PaymentInstitutionRefs)
            );
        undefined ->
            []
    end}.

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
