-module(capi_handler_shops).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID = 'ActivateShop', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:activate_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, Exception} ->
                case Exception of
                    #payproc_ShopNotFound{} ->
                        {ok, general_error(404, <<"Shop not found">>)};
                    #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                        {ok, {204, #{}, undefined}}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'SuspendShop', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:suspend_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, Exception} ->
                case Exception of
                    #payproc_ShopNotFound{} ->
                        {ok, general_error(404, <<"Shop not found">>)};
                    #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                        {ok, {204, #{}, undefined}}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShops', _Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Party = capi_utils:unwrap(capi_party:get_party(PartyID, Context)),
        {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShopByID', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ShopID = maps:get(shopID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_shop(PartyID, ShopID, Context) of
            {ok, Shop} ->
                {ok, {200, #{}, decode_shop(Shop)}};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShopsForParty', Req, Context) ->
    PartyID = maps:get(partyID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}};
            {error, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShopByIDForParty', Req, Context) ->
    PartyID = maps:get(partyID, Req),
    ShopID = maps:get(shopID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_shop(PartyID, ShopID, Context) of
            {ok, Shop} ->
                {ok, {200, #{}, decode_shop(Shop)}};
            {error, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'ActivateShopForParty', Req, Context) ->
    PartyID = maps:get(partyID, Req),
    ShopID = maps:get(shopID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:activate_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, Exception} ->
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
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'SuspendShopForParty', Req, Context) ->
    PartyID = maps:get(partyID, Req),
    ShopID = maps:get(shopID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:suspend_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, Exception} ->
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
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
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
