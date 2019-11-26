-module(capi_client_invoices).

-export([create_invoice/2]).
-export([create_invoice_access_token/2]).
-export([get_invoice_events/3]).
-export([get_invoice_events/4]).
-export([get_invoice_by_id/2]).
-export([get_invoice_by_external_id/2]).
-export([fulfill_invoice/3]).
-export([rescind_invoice/3]).
-export([get_invoice_payment_methods/2]).

-type context() :: capi_client_lib:context().

-spec create_invoice(context(), map()) -> {ok, term()} | {error, term()}.
create_invoice(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:create_invoice(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_invoice_access_token(context(), binary()) -> {ok, term()} | {error, term()}.
create_invoice_access_token(Context, InvoiceID) ->
    Params = #{binding => #{<<"invoiceID">> => InvoiceID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:create_invoice_access_token(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_invoice_events(context(), binary(), integer()) -> {ok, term()} | {error, term()}.
get_invoice_events(Context, InvoiceID, Limit) ->
    Qs = #{
        <<"limit">> => genlib:to_binary(Limit)
    },
    get_invoice_events_with(Context, InvoiceID, Qs).

-spec get_invoice_events(context(), binary(), integer(), integer()) -> {ok, term()} | {error, term()}.
get_invoice_events(Context, InvoiceID, EventID, Limit) ->
    Qs = #{
        <<"eventID">> => genlib:to_binary(EventID),
        <<"limit">> => genlib:to_binary(Limit)
    },
    get_invoice_events_with(Context, InvoiceID, Qs).

get_invoice_events_with(Context, InvoiceID, Qs) ->
    Params = #{
        binding => #{<<"invoiceID">> => InvoiceID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:get_invoice_events(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_invoice_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_invoice_by_id(Context, InvoiceID) ->
    Params = #{binding => #{<<"invoiceID">> => InvoiceID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:get_invoice_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_invoice_by_external_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_invoice_by_external_id(Context, ExternalID) ->
    Params = #{qs_val => #{<<"externalID">> => ExternalID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:get_invoice_by_external_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec fulfill_invoice(context(), binary(), string()) -> ok | {error, term()}.
fulfill_invoice(Context, InvoiceID, Reason) ->
    Params = #{
        binding => #{<<"invoiceID">> => InvoiceID},
        body => #{<<"reason">> => genlib:to_binary(Reason)}
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:fulfill_invoice(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, _Body} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec rescind_invoice(context(), binary(), string()) -> ok | {error, term()}.
rescind_invoice(Context, InvoiceID, Reason) ->
    Params = #{
        binding => #{<<"invoiceID">> => InvoiceID},
        body => #{<<"reason">> => genlib:to_binary(Reason)}
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:rescind_invoice(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, _Body} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec get_invoice_payment_methods(context(), binary()) -> {ok, term()} | {error, term()}.
get_invoice_payment_methods(Context, InvoiceID) ->
    Params = #{binding => #{<<"invoiceID">> => InvoiceID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoices_api:get_invoice_payment_methods(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
