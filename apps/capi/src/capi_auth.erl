-module(capi_auth).

-export([auth_api_key/2]).

auth_api_key(ApiKey, OperationID) ->
    {ok, Type, Credentials} = parse_auth_token(ApiKey),
    {ok, Context} = process_auth(Type, Credentials, OperationID),
    {true, Context}.

parse_auth_token(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, bearer, Credentials};
        _ ->
            {error, unsupported_auth_scheme}
    end.


process_auth(bearer, _AuthToken, _OperationID) ->
    %% @TODO find a jwt library :(
    {ok, #{}}.
