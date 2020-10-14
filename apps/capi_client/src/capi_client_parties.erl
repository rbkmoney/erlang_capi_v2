-module(capi_client_parties).

-export([get_my_party/1]).
-export([suspend_my_party/1]).
-export([activate_my_party/1]).

-type context() :: capi_client_lib:context().

-spec get_my_party(context()) -> {ok, term()} | {error, term()}.
get_my_party(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_parties_api:get_my_party(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec suspend_my_party(context()) -> ok | {error, term()}.
suspend_my_party(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_parties_api:suspend_my_party(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, undefined} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec activate_my_party(context()) -> ok | {error, term()}.
activate_my_party(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_parties_api:activate_my_party(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, undefined} -> ok;
        {error, Error} -> {error, Error}
    end.
