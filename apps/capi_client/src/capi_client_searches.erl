-module(capi_client_searches).

-export([search_invoices/3]).
-export([search_payments/3]).
-export([search_refunds/3]).
-export([search_payouts/3]).

-type context() :: capi_client_lib:context().
-type search_query() :: capi_client_lib:search_query().

-spec search_invoices(context(), binary(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_invoices(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_invoices(Url, PreparedParams, Opts),
    ct:print("Response ~p", [Response]),
    case capi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Invoices}} ->
            {ok, TotalCount, Invoices};
        {error, Error} ->
            {error, Error}
    end.

-spec search_payments(context(), binary(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payments(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payments(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} ->
            {error, Error}
    end.

-spec search_refunds(context(), binary(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_refunds(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_refunds(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} ->
            {error, Error}
    end.

-spec search_payouts(context(), binary(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payouts(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payouts(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} ->
            {error, Error}
    end.
