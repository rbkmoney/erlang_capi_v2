-module(capi_handler_shops).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('ActivateShop', Req, Context, _) ->
    Call = {party_management, 'ActivateShop', [maps:get(shopID, Req)]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('SuspendShop', Req, Context, _) ->
    Call = {party_management, 'SuspendShop', [maps:get(shopID, Req)]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('GetShops', _Req, Context, _) ->
    Party = capi_utils:unwrap(get_my_party(Context)),
    {ok, {200, [], decode_shops_map(Party#domain_Party.shops)}};

process_request('GetShopByID', Req, Context, _) ->
    Call = {party_management, 'GetShop', [maps:get(shopID, Req)]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, Shop} ->
            {ok, {200, [], decode_shop(Shop)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {404, [], general_error(<<"Shop not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
