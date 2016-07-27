-module(capi_auth).

-export([auth_api_key/2]).

-type context() :: #{binary() => any()}.

-spec auth_api_key(ApiKey :: binary(), OperationID :: atom()) -> {true, Context :: context()} | false.
auth_api_key(ApiKey, OperationID) ->
    {ok, Type, Credentials} = parse_auth_token(ApiKey),
    {ok, Context} = process_auth(Type, Credentials, OperationID),
    {true, Context}.

-spec parse_auth_token(ApiKey :: binary()) -> {ok, bearer, Credentials :: binary()} | {error, Reason :: atom()}.
parse_auth_token(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, bearer, Credentials};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec process_auth(Type :: atom(), AuthToken :: binary(), OperationID :: atom()) -> {ok, Context :: context()} | {error, Reason :: atom()}.
process_auth(bearer, _AuthToken, _OperationID) ->
    %% @TODO find a jwt library :(
    {ok, #{}}.
