-module(capi_client_shops).

-export([get_shops/1]).
-export([get_party_shops/1]).
-export([get_shop_by_id/2]).
-export([suspend_shop/2]).
-export([activate_shop/2]).

-type context() :: capi_client_lib:context().

-spec get_shops(context()) -> {ok, term()} | {error, term()}.
get_shops(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_shops_api:get_shops(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_party_shops(context()) -> {ok, term()} | {error, term()}.
get_party_shops(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_shops_api:get_shops(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_shop_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_shop_by_id(Context, ShopID) ->
    Params = #{
        binding => #{
            <<"shopID">> => ShopID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:get_shop_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec suspend_shop(context(), binary()) -> ok | {error, term()}.
suspend_shop(Context, ShopID) ->
    Params = #{
        binding => #{
            <<"shopID">> => ShopID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:suspend_shop(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, undefined} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec activate_shop(context(), binary()) -> ok | {error, term()}.
activate_shop(Context, ShopID) ->
    Params = #{
        binding => #{
            <<"shopID">> => ShopID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:activate_shop(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, undefined} -> ok;
        {error, Error} -> {error, Error}
    end.
