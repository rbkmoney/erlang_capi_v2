-module(capi_client_analytics).

-export([get_payment_conversion_stats/3]).
-export([get_payment_revenue_stats/3]).
-export([get_payment_geo_stats/3]).
-export([get_payment_rate_stats/3]).
-export([get_payment_method_stats/3]).

-type context() :: capi_client_lib:context().
-type analytics_search_query() :: capi_client_lib:analytics_search_query().

-spec get_payment_conversion_stats(context(), integer(), analytics_search_query()) ->
    {ok, term()} | {error, term()}.
get_payment_conversion_stats(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_analytics_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_conversion_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_revenue_stats(context(), integer(), analytics_search_query()) -> {ok, term()} | {error, term()}.
get_payment_revenue_stats(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_analytics_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_revenue_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_geo_stats(context(), integer(), analytics_search_query()) -> {ok, term()} | {error, term()}.
get_payment_geo_stats(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_analytics_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_geo_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_rate_stats(context(), integer(), analytics_search_query()) -> {ok, term()} | {error, term()}.
get_payment_rate_stats(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_analytics_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_rate_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_method_stats(context(), integer(), analytics_search_query()) -> {ok, term()} | {error, term()}.
get_payment_method_stats(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_analytics_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payment_method_stats(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
