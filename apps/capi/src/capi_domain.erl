-module(capi_domain).

-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").

-export([get_categories/1]).
-export([get/2]).

-type context() :: woody_client:context().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.

-type category() :: #domain_CategoryObject{}.

-spec get_categories(context()) -> {{ok, [category()]}, context()}.
get_categories(Context0) ->
    {{ok, Snapshot}, Context} = get_shapshot(Context0),
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
    {{ok, Categories}, Context}.

-spec get(ref(), context()) -> {{ok, data()}, context()} | {{error, not_found}, context()}.

get(Ref, Context0) ->
    {{ok, Snapshot}, Context} = get_shapshot(Context0),
    #'Snapshot'{
        domain = Domain
    } = Snapshot,
    case genlib_map:get(Ref, Domain) of
        undefined ->
            {{error, not_found}, Context};
        {_, C} ->
            {{ok, C}, Context}
    end.

head() ->
    {'head', #'Head'{}}.

get_shapshot(Context) ->
    get_shapshot(head(), Context).

get_shapshot(Reference, Context) ->
    service_call(
        repository,
        'Checkout',
        [Reference],
        Context
    ).

service_call(ServiceName, Function, Args, Context) ->
    cp_proto:call_service_safe(ServiceName, Function, Args, Context).

