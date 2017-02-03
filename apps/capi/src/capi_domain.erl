-module(capi_domain).

-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_config_thrift.hrl").

-export([get_categories/1]).
-export([get/2]).

-type context() :: woody_context:ctx().
-type ref() :: cp_domain_thrift:'Reference'().
-type data() :: _.

-type category() :: #domain_CategoryObject{}.

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
    cp_proto:call_service(
        repository,
        'Checkout',
        [Reference],
        Context,
        capi_woody_event_handler
    ).
