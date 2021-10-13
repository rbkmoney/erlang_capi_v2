-module(capi_merchant_id).

-include_lib("damsel/include/dmsl_payment_tool_provider_thrift.hrl").

-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type realm() :: dmsl_domain_thrift:'PaymentInstitutionRealm'().
-type merchant_data() :: dmsl_payment_tool_provider_thrift:'MerchantID'().
-type merchant_id() :: binary().

-export([party_id/1]).
-export([shop_id/1]).
-export([realm/1]).
-export([encode/3]).
-export([encode/1]).
-export([decode/1]).

-define(THRIFT_TYPE, {struct, struct, {dmsl_payment_tool_provider_thrift, 'MerchantID'}}).

-spec party_id(merchant_data()) -> party_id().
party_id(#paytoolprv_MerchantID{party_id = PartyID}) ->
    PartyID.

-spec shop_id(merchant_data()) -> shop_id().
shop_id(#paytoolprv_MerchantID{shop_id = ShopID}) ->
    ShopID.

-spec realm(merchant_data()) -> realm() | undefined.
realm(#paytoolprv_MerchantID{realm = Realm}) ->
    Realm.

-spec encode(realm(), party_id(), shop_id()) -> merchant_id().
encode(Realm, PartyID, ShopID) ->
    encode(#paytoolprv_MerchantID{
        party_id = PartyID,
        shop_id = ShopID,
        realm = Realm
    }).

-spec encode(merchant_data()) -> merchant_id().
encode(MerchantData) ->
    Codec = thrift_strict_binary_codec:new(),
    Data =
        case thrift_strict_binary_codec:write(Codec, ?THRIFT_TYPE, MerchantData) of
            {ok, Codec1} ->
                thrift_strict_binary_codec:close(Codec1)
        end,
    jose_base64url:encode(Data).

-spec decode(merchant_id()) -> merchant_data() | undefined.
decode(MerchantID) ->
    case jose_base64url:decode(MerchantID) of
        {ok, Data} ->
            Codec = thrift_strict_binary_codec:new(Data),
            case thrift_strict_binary_codec:read(Codec, ?THRIFT_TYPE) of
                {ok, MerchantData, Codec1} ->
                    _ = thrift_strict_binary_codec:close(Codec1),
                    MerchantData;
                _Error ->
                    undefined
            end;
        _Error ->
            undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec merchant_id_test_() -> _.

merchant_id_test_() ->
    PartyID = <<"party-a4ef-4d03-b666-bdec4b26c5f7">>,
    ShopID = <<"shop-a4ef-4d03-b666-bdec4b26c5f7">>,
    Realm = live,
    MerchantID = encode(Realm, PartyID, ShopID),
    MerchantData = decode(MerchantID),
    [
        ?_assertEqual(undefined, decode(<<"*CwABAAAAIXBhcnR5LWE0ZW">>)),
        ?_assertEqual(undefined, decode(<<"CwABAAAAIXBhcnR5LWE0ZW">>)),
        ?_assertNotEqual(undefined, MerchantData),
        ?_assertEqual(PartyID, party_id(MerchantData)),
        ?_assertEqual(ShopID, shop_id(MerchantData)),
        ?_assertEqual(Realm, realm(MerchantData))
    ].

-endif.
