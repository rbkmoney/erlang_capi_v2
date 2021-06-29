-module(capi_client_trade_blocs).

-export([get_trade_blocs/1]).
-export([get_trade_bloc_by_id/2]).

-type context() :: capi_client_lib:context().

-spec get_trade_blocs(context()) -> {ok, term()} | {error, term()}.
get_trade_blocs(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_trade_blocs_api:get_trade_blocs(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_trade_bloc_by_id(context(), Id :: binary()) -> {ok, term()} | {error, term()}.
get_trade_bloc_by_id(Context, Id) ->
    Params = #{
        binding => #{
            <<"tradeBlocID">> => Id
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_trade_blocs_api:get_trade_bloc_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
