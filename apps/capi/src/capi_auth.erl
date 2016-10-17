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
        'CreateInvoice' => [<<"invoices:create">>],
        'CreatePayment' => [<<"payments:create">>],
        'CreatePaymentToolToken' => [<<"payment_tool_tokens:create">>],
        'GetInvoiceByID' => [<<"invoices:get">>],
        'FulfillInvoice' => [<<"invoices:fulfill">>],
        'RescindInvoice' => [<<"invoices:rescind">>],
        'GetInvoiceEvents' => [<<"invoices.events:get">>],
        'GetPaymentByID' => [<<"payments:get">>],
        'GetInvoices' => [<<"invoices_stats:get">>],
        'GetPaymentConversionStats' => [<<"payments_conversion_stats:get">>],
        'GetPaymentRevenueStats' => [<<"payments_revenue_stats:get">>],
        'GetPaymentGeoStats' => [<<"payments_geo_stats:get">>],
        'GetPaymentRateStats' => [<<"payments_rate_stats:get">>],
        'GetPaymentInstrumentStats' => [<<"payments_instrument_stats:get">>],
        'GetMyParty' => [<<"party:get">>, <<"party:create">>],
        'ActivateShop' => [<<"shops:activate">>, <<"party:create">>],
        'CreateShop' => [<<"shop:create">>, <<"party:create">>],
        'SuspendShop' => [<<"shops:suspend">>, <<"party:create">>],
        'UpdateShop' => [<<"shops:update">>, <<"party:create">>],
        'SuspendMyParty' => [<<"party:suspend">>, <<"party:create">>],
        'ActivateMyParty' => [<<"party:activate">>, <<"party:create">>],
        'GetClaimByID' => [<<"claims:get">>, <<"party:create">>],
        'GetClaimByStatus' => [<<"claims:get">>, <<"party:create">>],
        'RevokeClaimByID' => [<<"claims:revoke">>, <<"party:create">>],
        'GetCategories' => [<<"categories:get">>],
        'GetCategoryByRef' => [<<"categories:get">>],
        'GetShopAccounts' => [<<"accounts:get">>],
        'GetAccountByID' => [<<"accounts:get">>]
    }.
