-module(capi_handler_categories).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetCategories', _Req, #{woody_context := WoodyContext}, _) ->
    Categories = capi_utils:unwrap(capi_domain:get_categories(WoodyContext)),
    {ok, {200, [], [decode_category(C) || C <- Categories]}};

process_request('GetCategoryByRef', Req, Context, _) ->
    case get_category_by_id(genlib:to_int(maps:get(categoryID, Req)), Context) of
        {ok, Category} ->
            {ok, {200, [], decode_category(Category)}};
        {error, not_found} ->
            {404, [], capi_handler_utils:general_error(<<"Category not found">>)}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

get_category_by_id(CategoryID, #{woody_context := WoodyContext}) ->
    CategoryRef = {category, #domain_CategoryRef{id = CategoryID}},
    capi_domain:get(CategoryRef, WoodyContext).

decode_category(#domain_CategoryObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"name"       >> => Data#domain_Category.name,
        <<"categoryID" >> => Ref#domain_CategoryRef.id,
        <<"description">> => Data#domain_Category.description
    }).
