-module(capi_client_geo).

-export([get_location_names/2]).

-type context() :: capi_client_lib:context().

-spec get_location_names(context(), map()) -> {ok, term()} | {error, term()}.
get_location_names(Context, Query) ->
    Params = #{
        qs_val => Query
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_geo_api:get_locations_names(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
