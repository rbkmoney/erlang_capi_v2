-module(capi_domain).

-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").

-export([get_categories/1]).
-export([get_category_by_ref/2]).

-type category() :: #domain_CategoryObject{}.

-spec get_categories(woody_client:context()) -> {{ok, [category()]}, woody_client:context()}.
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

-spec get_category_by_ref(Ref :: integer(), woody_client:context()) ->
    {{ok, category()}, woody_client:context()} | {{error, not_found}, woody_client:context()}.

get_category_by_ref(CategoryRef, Context0) ->
    {{ok, Snapshot}, Context} = get_shapshot(Context0),
    #'Snapshot'{
        domain = Domain
    } = Snapshot,
    case genlib_map:get(
        {category, #domain_CategoryRef{id = CategoryRef}},
        Domain
    ) of
        {category, C = #domain_CategoryObject{}} ->
            {{ok, C}, Context};
        _ ->
            {{error, not_found}, Context}
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

