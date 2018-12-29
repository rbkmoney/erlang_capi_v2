-module(capi_handler_shops).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('SuspendShop', Req, Context, _) ->
    Call = {party_management, 'SuspendShop', [maps:get(shopID, Req)]},
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Shop not found">>)}};
                #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, [], undefined}}
            end
    end;

process_request('GetShops', _Req, Context, _) ->
    Party = capi_utils:unwrap(capi_handler_utils:get_my_party(Context)),
    {ok, {200, [], decode_shops_map(Party#domain_Party.shops)}};

process_request('GetShopByID', Req, Context, _) ->
    Call = {party_management, 'GetShop', [maps:get(shopID, Req)]},
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, Shop} ->
            {ok, {200, [], decode_shop(Shop)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Shop not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

%%

decode_shops_map(Shops) ->
    capi_handler:decode_map(Shops, fun decode_shop/1).

decode_shop(Shop) ->
    genlib_map:compact(#{
        <<"id"          >> => Shop#domain_Shop.id,
        <<"createdAt"   >> => Shop#domain_Shop.created_at,
        <<"isBlocked"   >> => capi_handler:is_blocked(Shop#domain_Shop.blocking),
        <<"isSuspended" >> => capi_handler:is_suspended(Shop#domain_Shop.suspension),
        <<"categoryID"  >> => capi_handler:decode_category_ref(Shop#domain_Shop.category),
        <<"details"     >> => decode_shop_details(Shop#domain_Shop.details),
        <<"location"    >> => decode_shop_location(Shop#domain_Shop.location),
        <<"contractID"  >> => Shop#domain_Shop.contract_id,
        <<"payoutToolID">> => Shop#domain_Shop.payout_tool_id,
        <<"scheduleID"  >> => capi_handler:decode_business_schedule_ref(Shop#domain_Shop.payout_schedule),
        <<"account"     >> => decode_shop_account(Shop#domain_Shop.account)
    }).

decode_shop_details(#domain_ShopDetails{name = Name, description = Description}) ->
    genlib_map:compact(#{
        <<"name">> => Name,
        <<"description">> => Description
    }).

decode_shop_location({url, Location}) ->
    #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => Location
    }.

decode_shop_account(undefined) ->
    undefined;
decode_shop_account(#domain_ShopAccount{currency = Currency, settlement = SettlementID, guarantee = GuaranteeID}) ->
    #{
        <<"guaranteeID" >> => GuaranteeID,
        <<"settlementID">> => SettlementID,
        <<"currency"    >> => capi_handler:decode_currency(Currency)
    }.

