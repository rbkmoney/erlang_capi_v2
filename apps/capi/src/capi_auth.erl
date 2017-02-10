-module(capi_auth).

-export([init/0]).
-export([auth_api_key/2]).

-define(KEYNAME, api_auth_pubkey).

-spec init() -> ok | no_return().

init() ->
    ok = capi_keystore:init(),
    PubkeyPath = genlib_app:env(capi, api_auth_pubkey_path),
    case capi_keystore:store(?KEYNAME, PubkeyPath) of
        ok ->
            ok;
        {error, Reason} ->
            _ = lager:error("Missing api auth pubkey, stopping the app: ~p", [Reason]),
            exit({api_auth_pubkey_error, Reason})
    end.

-type context() :: #{binary() => any()}.

-spec auth_api_key(
    OperationID :: swagger_api:operation_id(),
    ApiKey :: binary()
) -> {true, Context :: context()} | false.

auth_api_key(OperationID, ApiKey) ->
    case parse_auth_token(ApiKey) of
        {ok, {Type, Credentials}} ->
            case  process_auth(Type, Credentials, OperationID) of
                {ok, Context} ->
                    {true, Context};
                {error, Error} ->
                    _ = log_auth_error(OperationID, Error),
                    false
            end;
        {error, Error} ->
            _ = log_auth_error(OperationID, Error),
            false
    end.

-spec parse_auth_token(ApiKey :: binary()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_auth_token(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec process_auth(Type :: atom(), AuthToken :: binary(), OperationID :: swagger_api:operation_id()) ->
    {ok, Context :: context()} | {error, Reason :: atom()}.

process_auth(bearer, AuthToken, OperationID) ->
    case verify_token(AuthToken, OperationID) of
        {ok, Claims} ->
            authorize(Claims, OperationID);
        Error ->
            Error
    end.

verify_token(AuthToken, OperationID) ->
    case verify_alg(AuthToken) of
        {ok, Payload} ->
            Claims = jsx:decode(Payload, [return_maps]), %% @FIXME deal with non json token
            case validate_claims(Claims, OperationID) of
                true ->
                    {ok, Claims};
                false ->
                    {error, expired}
            end;
        Error ->
            Error
    end.

verify_alg(AuthToken) ->
    {ok, PublicJWK} = capi_keystore:get(?KEYNAME),
    try
        case jose_jwk:verify(AuthToken, PublicJWK) of
            {true, Claims, _} ->
                {ok, Claims};
            _ ->
                {error, invalid_token}
        end
    catch
        error:{badarg, _} ->
            _ = lager:info(
                "Unparsable auth token ~s",
                [genlib_format:format_stacktrace(erlang:get_stacktrace(), [newlines])]
            ),
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

validate_claims(Claims, OperationID) ->
    Validator = get_validator(OperationID),
    Validator(Claims).

get_actions() ->
    #{
        'CreateInvoice' => [<<"payments:write">>],
        'CreatePayment' => [<<"payments:write">>],
        'CreatePaymentToolToken' => [<<"payment_tool_tokens:write">>],
        'GetInvoiceByID' => [<<"payments:read">>],
        'FulfillInvoice' => [<<"payments:write">>],
        'RescindInvoice' => [<<"payments:write">>],
        'GetInvoiceEvents' => [<<"payments:read">>],
        'GetPaymentByID' => [<<"payments:read">>],
        'GetInvoices' => [<<"payments:read">>],
        'GetPaymentConversionStats' => [<<"party:read">>],
        'GetPaymentRevenueStats' => [<<"party:read">>],
        'GetPaymentGeoStats' => [<<"party:read">>],
        'GetPaymentRateStats' => [<<"party:read">>],
        'GetPaymentMethodStats' => [<<"party:read">>],
        'GetMyParty' => [<<"party:read">>],
        'ActivateShop' => [<<"party:write">>],
        'CreateShop' => [<<"party:write">>],
        'SuspendShop' => [<<"party:write">>],
        'UpdateShop' => [<<"party:write">>],
        'SuspendMyParty' => [<<"party:write">>],
        'ActivateMyParty' => [<<"party:write">>],
        'GetClaimByID' => [<<"party:read">>],
        'GetClaimsByStatus' => [<<"party:read">>],
        'RevokeClaimByID' => [<<"party:write">>],
        'GetCategories' => [],
        'GetCategoryByRef' => [],
        'GetAccountByID' => [<<"party:read">>],
        'GetShopByID' => [<<"party:read">>],
        'GetShops' => [<<"party:read">>],
        'GetPayoutTools' => [<<"party:read">>],
        'CreatePayoutTool' => [<<"party:write">>],
        'GetContracts' => [<<"party:read">>],
        'CreateContract' => [<<"party:write">>],
        'GetContractByID' => [<<"party:read">>],
        'GetLocationsNames' => []
    }.

get_validator('CreatePaymentToolToken') ->
    fun (_Claims) -> true end;
get_validator(_Any) ->
    fun check_expiration/1.

check_expiration(Claims) ->
    case genlib_map:get(<<"exp">>, Claims) of
        undefined ->
            false;
        I when is_integer(I) ->
            genlib_time:unow() =< I
    end.


log_auth_error(OperationID, Error) ->
    lager:info("Auth for operation ~p failed due to ~p", [OperationID, Error]).

