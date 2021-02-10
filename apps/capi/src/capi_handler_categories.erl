-module(capi_handler_categories).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare_request/3]).
-export([process_request/3]).
-export([authorize_request/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare_request(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare_request(OperationID, _Req, _Context) when
    OperationID =:= 'GetCategories' orelse
        OperationID =:= 'GetCategoryByRef'
->
    {ok, #{}};
prepare_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec authorize_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
authorize_request(OperationID, Context, ReqState) when
    OperationID =:= 'GetCategories' orelse
        OperationID =:= 'GetCategoryByRef'
->
    Resolution = capi_auth:authorize_operation(OperationID, [], Context, ReqState),
    {ok, ReqState#{resolution => Resolution}};
authorize_request(_OperationID, _Context, _ReqState) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetCategories', #{woody_context := WoodyContext}, _ReqState) ->
    Categories = capi_utils:unwrap(capi_domain:get_categories(WoodyContext)),
    {ok, {200, #{}, [decode_category(C) || C <- Categories]}};
process_request('GetCategoryByRef', Context, #{data := Req}) ->
    case get_category_by_id(genlib:to_int(maps:get(categoryID, Req)), Context) of
        {ok, Category} ->
            {ok, {200, #{}, decode_category(Category)}};
        {error, not_found} ->
            {ok, general_error(404, <<"Category not found">>)}
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

get_category_by_id(CategoryID, #{woody_context := WoodyContext}) ->
    CategoryRef = {category, #domain_CategoryRef{id = CategoryID}},
    capi_domain:get(CategoryRef, WoodyContext).

decode_category(#domain_CategoryObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"name">> => Data#domain_Category.name,
        <<"categoryID">> => Ref#domain_CategoryRef.id,
        <<"description">> => Data#domain_Category.description
    }).
