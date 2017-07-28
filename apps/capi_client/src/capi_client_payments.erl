-module(capi_client_payments).

-export([get_payment_by_id/3]).
-export([get_payments/2]).
-export([create_payment/3]).

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
