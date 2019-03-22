-module(capi_client_analytics).

-export([get_payment_conversion_stats/2]).
-export([get_payment_revenue_stats/2]).
-export([get_payment_geo_stats/2]).
-export([get_payment_rate_stats/2]).
-export([get_payment_method_stats/2]).

-type context() :: capi_client_lib:context().
-type search_query() :: capi_client_lib:search_query().

-spec get_payment_conversion_stats(context(), search_query()) ->
    {ok, term()} | {error, term()}.
get_payment_conversion_stats(Context, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_conversion_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_revenue_stats(context(), search_query()) -> {ok, term()} | {error, term()}.
get_payment_revenue_stats(Context, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_revenue_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_geo_stats(context(), search_query()) -> {ok, term()} | {error, term()}.
get_payment_geo_stats(Context, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_geo_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_rate_stats(context(), search_query()) -> {ok, term()} | {error, term()}.
get_payment_rate_stats(Context, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_rate_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_method_stats(context(), search_query()) -> {ok, term()} | {error, term()}.
get_payment_method_stats(Context, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_method_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
