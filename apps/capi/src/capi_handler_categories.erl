-module(capi_handler_categories).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('GetCategories', _Req, #{woody_context := WoodyContext}) ->
    Categories = capi_utils:unwrap(capi_domain:get_categories(WoodyContext)),
    {ok, {200, [], [decode_category(C) || C <- Categories]}};

process_request('GetCategoryByRef', Req, Context) ->
    case get_category_by_id(genlib:to_int(maps:get(categoryID, Req)), Context) of
        {ok, Category} ->
            {ok, {200, [], decode_category(Category)}};
        {error, not_found} ->
            {404, [], capi_handler_utils:general_error(<<"Category not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

get_category_by_id(CategoryID, #{woody_context := WoodyContext}) ->
    CategoryRef = {category, #domain_CategoryRef{id = CategoryID}},
    capi_domain:get(CategoryRef, WoodyContext).

decode_category(#domain_CategoryObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"name"       >> => Data#domain_Category.name,
        <<"categoryID" >> => Ref#domain_CategoryRef.id,
        <<"description">> => Data#domain_Category.description
    }).
