-module(capi_domain).

-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").

-export([get_categories/1]).
-export([get_category_by_ref/2]).

-type category() :: #domain_CategoryObject{}.

-spec get_categories(woody_client:context()) -> {{ok, [category()]}, woody_client:context()}.
get_categories(Context) ->
    get_all_categories(Context).

-spec get_category_by_ref(Ref :: integer(), woody_client:context()) ->
    {{ok, category()}, woody_client:context()} | {{error, not_found}, woody_client:context()}.

get_category_by_ref(CategoryRef, Context0) ->
    {{ok, Categories}, Context} = get_all_categories(Context0),
    E = lists:dropwhile(
        fun
            (#domain_CategoryObject{ref = #domain_CategoryRef{
                id = TmpRef
            }}) ->
                case TmpRef of
                    CategoryRef -> false;
                    _ -> true
                end;
            (_) -> true
        end,
        Categories
    ),
    case E of
        [#domain_CategoryObject{} = C | _] ->
            {{ok, C}, Context};
        _ ->
            {{error, not_found}, Context}
    end.

get_all_categories(Context0) ->
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

