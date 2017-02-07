-module(capi_auth).

-export([auth_api_key/2]).

-type context() :: #{binary() => any()}.

-spec auth_api_key(
    OperationID :: swagger_api:operation_id(),
    ApiKey :: binary()
) -> {true, Context :: context()} | false.

auth_api_key(OperationID, ApiKey) ->
    {ok, Type, Credentials} = parse_auth_token(ApiKey),
    case  process_auth(Type, Credentials, OperationID) of
        {ok, Context} ->
            {true, Context};
        {error, _Error} ->
            false
    end.

-spec parse_auth_token(ApiKey :: binary()) ->
    {ok, bearer, Credentials :: binary()} | {error, Reason :: atom()}.

parse_auth_token(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, bearer, Credentials};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec process_auth(Type :: atom(), AuthToken :: binary(), OperationID :: swagger_api:operation_id()) ->
    {ok, Context :: context()} | {error, Reason :: atom()}.

process_auth(bearer, AuthToken, OperationID) ->
    case verify_token(AuthToken) of
        {ok, Claims} ->
            authorize(Claims, OperationID);
        Error ->
            Error
    end.

verify_token(AuthToken) ->
    case verify_alg(AuthToken) of
        {ok, Payload} ->
            Claims = jsx:decode(Payload, [return_maps]), %% @FIXME deal with non json token
            case is_valid_exp(Claims) of
                true ->
                    {ok, Claims};
                false ->
                    {error, expired}
            end;
        Error ->
            Error
    end.

-include_lib("jose/include/jose_jwk.hrl").

verify_alg(AuthToken) ->
    PemFilePath = genlib_app:env(capi, api_secret_path),
    RSAPublicJWK = #jose_jwk{} = jose_jwk:from_pem_file(PemFilePath),
    case jose_jwk:verify(AuthToken, RSAPublicJWK) of
        {true, Claims, _} ->
            {ok, Claims};
        _ ->
            {error, invalid_token}
    end.

authorize(
    #{
        <<"resource_access">> := #{
            <<"common-api">> := #{
                <<"roles">> := Roles
            }
        }
    } = Claims,
    OperationID
) ->
    case genlib_map:get(OperationID, get_actions()) of
        undefined ->
            {error, unauthorized};
        RequiredRoles ->
            case RequiredRoles -- Roles of
                [] ->
                    {ok, Claims};
                _ ->
                    {error, unauthorized}
            end
    end;

authorize(_Claims, _OperationID) ->
    {error, unauthorized}.

is_valid_exp(Claims) ->
    case genlib_map:get(<<"exp">>, Claims) of
        undefined ->
            false;
        I when is_integer(I) ->
            genlib_time:unow() =< I
    end.

get_actions() ->
    #{
        'CreateInvoice' => [<<"payments:create">>],
        'CreatePayment' => [<<"payments:create">>],
        'CreatePaymentToolToken' => [<<"payment_tool_tokens:create">>],
        'GetInvoiceByID' => [<<"payments:create">>],
        'FulfillInvoice' => [<<"payments:create">>],
        'RescindInvoice' => [<<"payments:create">>],
        'GetInvoiceEvents' => [<<"payments:create">>],
        'GetPaymentByID' => [<<"payments:create">>],
        'GetInvoices' => [<<"payments:create">>],
        'GetPaymentConversionStats' => [<<"party:create">>],
        'GetPaymentRevenueStats' => [<<"party:create">>],
        'GetPaymentGeoStats' => [<<"party:create">>],
        'GetPaymentRateStats' => [<<"party:create">>],
        'GetPaymentMethodStats' => [<<"party:create">>],
        'GetMyParty' => [<<"party:create">>],
        'ActivateShop' => [<<"party:create">>],
        'CreateShop' => [<<"party:create">>],
        'SuspendShop' => [<<"party:create">>],
        'UpdateShop' => [<<"party:create">>],
        'SuspendMyParty' => [<<"party:create">>],
        'ActivateMyParty' => [<<"party:create">>],
        'GetClaimByID' => [<<"party:create">>],
        'GetClaimsByStatus' => [<<"party:create">>],
        'RevokeClaimByID' => [<<"party:create">>],
        'GetCategories' => [<<"categories:get">>],
        'GetCategoryByRef' => [<<"categories:get">>],
        'GetAccountByID' => [<<"party:create">>],
        'GetShopByID' => [<<"party:create">>],
        'GetShops' => [<<"party:create">>],
        'GetPayoutTools' => [<<"party:create">>],
        'CreatePayoutTool' => [<<"party:create">>],
        'GetContracts' => [<<"party:create">>],
        'CreateContract' => [<<"party:create">>],
        'GetContractByID' => [<<"party:create">>],
        'GetLocationsNames' => [<<"party:create">>]
    }.
