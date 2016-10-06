-module(capi_domain).

-include_lib("cp_proto/include/cp_domain_thrift.hrl").

-export([get_categories/0]).
-export([get_category_by_ref/1]).

-type category() :: #domain_CategoryObject{}.
-type category_ref() :: cp_domain_thrift:'CategoryRef'().

-spec get_categories() -> [category()].
get_categories() ->
    get_all_categories().

-spec get_category_by_ref(category_ref()) -> {ok, category()} | {error, not_found}.
get_category_by_ref(CategoryRef) ->
    E = lists:filter(
        fun
            (#domain_CategoryObject{ref = #domain_CategoryRef{id = TmpC}}) ->
                case TmpC of
                    CategoryRef -> true;
                    _ -> false
                end;
            (_) -> false
        end,
        get_all_categories()
    ),
    case E of
        [#domain_CategoryObject{} = C] -> {ok, C};
        _ -> {error, not_found}
    end.

get_all_categories() ->
    [
        #domain_CategoryObject{
            ref = #domain_CategoryRef{
                id = 1
            },
            data = #domain_Category{
                name = <<"entertainment">>
            }
        },
        #domain_CategoryObject{
            ref = #domain_CategoryRef{
                id = 2
            },
            data = #domain_Category{
                name = <<"commersion">>
            }
        }
    ].
