-module(capi_client_categories).

-export([get_categories/1]).
-export([get_category_by_ref/2]).

-type context() :: capi_client_lib:context().

-spec get_categories(context()) -> {ok, term()} | {error, term()}.
get_categories(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_categories_api:get_categories(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_category_by_ref(context(), term()) -> {ok, term()} | {error, term()}.
get_category_by_ref(Context, CategoryRef) ->
    Params = #{
        binding => #{
            <<"categoryID">> => genlib:to_list(CategoryRef)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_categories_api:get_category_by_ref(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
