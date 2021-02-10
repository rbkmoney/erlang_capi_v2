-module(capi_handler_shops).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

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
    OperationID =:= 'ActivateShop' orelse
        OperationID =:= 'SuspendShop' orelse
        OperationID =:= 'GetShops' orelse
        OperationID =:= 'GetShopByID' orelse
        OperationID =:= 'GetShopsForParty' orelse
        OperationID =:= 'GetShopByIDForParty' orelse
        OperationID =:= 'ActivateShopForParty' orelse
        OperationID =:= 'SuspendShopForParty'
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
    OperationID =:= 'ActivateShop' orelse
        OperationID =:= 'SuspendShop' orelse
        OperationID =:= 'GetShops' orelse
        OperationID =:= 'GetShopByID' orelse
        OperationID =:= 'GetShopsForParty' orelse
        OperationID =:= 'GetShopByIDForParty' orelse
        OperationID =:= 'ActivateShopForParty' orelse
        OperationID =:= 'SuspendShopForParty'
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
process_request('ActivateShop', Context, #{data := Req}) ->
    Call = {party_management, 'ActivateShop', {maps:get(shopID, Req)}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, general_error(404, <<"Shop not found">>)};
                #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;
process_request('SuspendShop', Context, #{data := Req}) ->
    Call = {party_management, 'SuspendShop', {maps:get(shopID, Req)}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, general_error(404, <<"Shop not found">>)};
                #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;
process_request('GetShops', Context, _ReqState) ->
    Party = capi_utils:unwrap(capi_handler_utils:get_party(Context)),
    {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}};
process_request('GetShopByID', Context, #{data := Req}) ->
    Call = {party_management, 'GetShop', {maps:get(shopID, Req)}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, Shop} ->
            {ok, {200, #{}, decode_shop(Shop)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, general_error(404, <<"Shop not found">>)}
    end;
process_request('GetShopsForParty', Context, #{data := Req}) ->
    PartyID = maps:get(partyID, Req),
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:get_party(PartyID, Context) of
        {ok, Party} ->
            {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_PartyNotFound{}} ->
            {ok, general_error(404, <<"Party not found">>)}
    end;
process_request('GetShopByIDForParty', Context, #{data := Req}) ->
    PartyID = maps:get(partyID, Req),
    ShopID = maps:get(shopID, Req),
    Call = {party_management, 'GetShop', {PartyID, ShopID}},
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, Shop} ->
            {ok, {200, #{}, decode_shop(Shop)}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_PartyNotFound{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, general_error(404, <<"Shop not found">>)}
    end;
process_request('ActivateShopForParty', Context, #{data := Req}) ->
    PartyID = maps:get(partyID, Req),
    ShopID = maps:get(shopID, Req),
    Call = {party_management, 'ActivateShop', {PartyID, ShopID}},
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Party not found">>)};
                #payproc_PartyNotFound{} ->
                    {ok, general_error(404, <<"Party not found">>)};
                #payproc_ShopNotFound{} ->
                    {ok, general_error(404, <<"Shop not found">>)};
                #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;
process_request('SuspendShopForParty', Context, #{data := Req}) ->
    PartyID = maps:get(partyID, Req),
    ShopID = maps:get(shopID, Req),
    Call = {party_management, 'SuspendShop', {PartyID, ShopID}},
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Party not found">>)};
                #payproc_PartyNotFound{} ->
                    {ok, general_error(404, <<"Party not found">>)};
                #payproc_ShopNotFound{} ->
                    {ok, general_error(404, <<"Shop not found">>)};
                #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

decode_shops_map(Shops) ->
    capi_handler_decoder_utils:decode_map(Shops, fun decode_shop/1).

decode_shop(Shop) ->
    genlib_map:compact(#{
        <<"id">> => Shop#domain_Shop.id,
        <<"createdAt">> => Shop#domain_Shop.created_at,
        <<"isBlocked">> => capi_handler_decoder_party:is_blocked(Shop#domain_Shop.blocking),
        <<"isSuspended">> => capi_handler_decoder_party:is_suspended(Shop#domain_Shop.suspension),
        <<"categoryID">> => capi_handler_decoder_utils:decode_category_ref(Shop#domain_Shop.category),
        <<"details">> => capi_handler_decoder_party:decode_shop_details(Shop#domain_Shop.details),
        <<"location">> => capi_handler_decoder_party:decode_shop_location(Shop#domain_Shop.location),
        <<"contractID">> => Shop#domain_Shop.contract_id,
        <<"payoutToolID">> => Shop#domain_Shop.payout_tool_id,
        <<"scheduleID">> => capi_handler_decoder_utils:decode_business_schedule_ref(Shop#domain_Shop.payout_schedule),
        <<"account">> => decode_shop_account(Shop#domain_Shop.account)
    }).

decode_shop_account(undefined) ->
    undefined;
decode_shop_account(#domain_ShopAccount{currency = Currency, settlement = SettlementID, guarantee = GuaranteeID}) ->
    #{
        <<"guaranteeID">> => GuaranteeID,
        <<"settlementID">> => SettlementID,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)
    }.
