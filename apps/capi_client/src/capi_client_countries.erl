-module(capi_client_countries).

-export([get_countries/1]).
-export([get_country_by_id/2]).

-type context() :: capi_client_lib:context().

-spec get_countries(context()) -> {ok, term()} | {error, term()}.
get_countries(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_countries_api:get_countries(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_country_by_id(context(), Id :: binary()) -> {ok, term()} | {error, term()}.
get_country_by_id(Context, Id) ->
    Params = #{
        binding => #{
            <<"countryID">> => Id
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_countries_api:get_country_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
