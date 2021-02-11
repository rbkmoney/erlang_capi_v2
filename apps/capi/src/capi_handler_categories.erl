-module(capi_handler_categories).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare(OperationID, Req, Context) when
    OperationID =:= 'GetCategories' orelse
        OperationID =:= 'GetCategoryByRef'
->
    Authorize = fun() -> {ok, capi_auth:authorize_operation(OperationID, [], Context, Req)} end,
    Process = fun() -> process_request(OperationID, Context, Req) end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetCategories', #{woody_context := WoodyContext}, _Req) ->
    Categories = capi_utils:unwrap(capi_domain:get_categories(WoodyContext)),
    {ok, {200, #{}, [decode_category(C) || C <- Categories]}};
process_request('GetCategoryByRef', Context, Req) ->
    case get_category_by_id(genlib:to_int(maps:get(categoryID, Req)), Context) of
        {ok, Category} ->
            {ok, {200, #{}, decode_category(Category)}};
        {error, not_found} ->
            {ok, general_error(404, <<"Category not found">>)}
    end.

%%

get_category_by_id(CategoryID, #{woody_context := WoodyContext}) ->
    CategoryRef = {category, #domain_CategoryRef{id = CategoryID}},
    capi_domain:get(CategoryRef, WoodyContext).

decode_category(#domain_CategoryObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"name">> => Data#domain_Category.name,
        <<"categoryID">> => Ref#domain_CategoryRef.id,
        <<"description">> => Data#domain_Category.description
    }).
