-module(capi_client_customers).

-export([create_customer/2]).
-export([get_customer_by_id/2]).
-export([delete_customer/2]).
-export([create_customer_access_token/2]).
-export([create_binding/3]).
-export([get_bindings/2]).
-export([get_binding/3]).
-export([get_customer_events/3]).
-export([get_customer_events/4]).

-type context() :: capi_client_lib:context().

-spec create_customer(context(), map()) -> {ok, term()} | {error, term()}.
create_customer(Context, Request) ->
    Params = #{
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:create_customer(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_customer_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_customer_by_id(Context, CustomerID) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_customer_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec delete_customer(context(), binary()) -> {ok, term()} | {error, term()}.
delete_customer(Context, CustomerID) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:delete_customer(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_customer_access_token(context(), binary()) -> {ok, term()} | {error, term()}.
create_customer_access_token(Context, CustomerID) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:create_customer_access_token(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_binding(context(), binary(), map()) -> {ok, term()} | {error, term()}.
create_binding(Context, CustomerID, Request) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID
        },
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:create_binding(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_bindings(context(), binary()) -> {ok, term()} | {error, term()}.
get_bindings(Context, CustomerID) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_bindings(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_binding(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_binding(Context, CustomerID, BindingID) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID,
            <<"customerBindingID">> => BindingID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_binding(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_customer_events(context(), binary(), integer()) -> {ok, term()} | {error, term()}.
get_customer_events(Context, CustomerID, Limit) ->
    Qs = #{
        <<"limit">> => genlib:to_binary(Limit)
    },
    get_customer_events_with(Context, CustomerID, Qs).

-spec get_customer_events(context(), binary(), integer(), integer()) -> {ok, term()} | {error, term()}.
get_customer_events(Context, CustomerID, EventID, Limit) ->
    Qs = #{
        <<"eventID">> => genlib:to_binary(EventID),
        <<"limit">> => genlib:to_binary(Limit)
    },
    get_customer_events_with(Context, CustomerID, Qs).

get_customer_events_with(Context, CustomerID, Qs) ->
    Params = #{
        binding => #{
            <<"customerID">> => CustomerID
        },
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_customer_events(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
