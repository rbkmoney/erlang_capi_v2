-module(capi_client_payments).

-export([get_payment_by_id/3]).
-export([get_payments/2]).
-export([create_payment/3]).
-export([cancel_payment/4]).
-export([capture_payment/4]).
-export([get_refunds/3]).
-export([get_refund_by_id/4]).
-export([create_refund/4]).

-type context() :: capi_client_lib:context().

-spec get_payments(context(), binary()) -> {ok, term()} | {error, term()}.
get_payments(Context, InvoiceID) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID
        }
     },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:get_payments(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_by_id(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_payment_by_id(Context, InvoiceID, PaymentID) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID,
            <<"paymentID">> => PaymentID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:get_payment_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_payment(context(), map(), binary()) -> {ok, term()} | {error, term()}.
create_payment(Context, Request, InvoiceID) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID
        },
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:create_payment(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec cancel_payment(context(), integer(), integer(), binary()) -> ok | {error, term()}.
cancel_payment(Context, InvoiceID, PaymentID, Reason) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID,
            <<"paymentID">> => PaymentID
        },
        body => #{<<"reason">> => genlib:to_binary(Reason)}
    },
    {Host, Port, PreparedParams} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:cancel_payment(Host, Port, PreparedParams),
    case capi_client_lib:handle_response(Response) of
        {ok, _Body} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec capture_payment(context(), integer(), integer(), binary()) -> ok | {error, term()}.
capture_payment(Context, InvoiceID, PaymentID, Reason) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID,
            <<"paymentID">> => PaymentID
        },
        body => #{<<"reason">> => genlib:to_binary(Reason)}
    },
    {Host, Port, PreparedParams} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:capture_payment(Host, Port, PreparedParams),
    case capi_client_lib:handle_response(Response) of
        {ok, _Body} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec get_refunds(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_refunds(Context, InvoiceID, PaymentID) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID,
            <<"paymentID">> => PaymentID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:get_refunds(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_refund_by_id(context(), binary(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_refund_by_id(Context, InvoiceID, PaymentID, RefundID) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID,
            <<"paymentID">> => PaymentID,
            <<"refundID">>  => RefundID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:get_refund_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_refund(context(), binary(), binary(), binary()) -> {ok, term()} | {error, term()}.
create_refund(Context, InvoiceID, PaymentID, Reason) ->
    Params = #{
        binding => #{
            <<"invoiceID">> => InvoiceID,
            <<"paymentID">> => PaymentID
        },
        body => #{<<"reason">> => genlib:to_binary(Reason)}
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payments_api:create_refund(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

