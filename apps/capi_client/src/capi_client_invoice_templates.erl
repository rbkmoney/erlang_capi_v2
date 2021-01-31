-module(capi_client_invoice_templates).

-export([create/2]).
-export([get_template_by_id/2]).
-export([update/3]).
-export([delete/2]).
-export([create_invoice/3]).
-export([get_invoice_payment_methods/2]).

-type context() :: capi_client_lib:context().

-spec create(context(), map()) -> {ok, term()} | {error, term()}.
create(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoice_templates_api:create_invoice_template(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_template_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_template_by_id(Context, InvoiceTplID) ->
    Params = #{binding => #{<<"invoiceTemplateID">> => InvoiceTplID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoice_templates_api:get_invoice_template_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec update(context(), binary(), map()) -> {ok, term()} | {error, term()}.
update(Context, InvoiceTplID, Request) ->
    Params = #{
        binding => #{<<"invoiceTemplateID">> => InvoiceTplID},
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoice_templates_api:update_invoice_template(Url, PreparedParams, Opts),

    capi_client_lib:handle_response(Response).

-spec delete(context(), binary()) -> ok | {error, term()}.
delete(Context, InvoiceTplID) ->
    Params = #{
        binding => #{<<"invoiceTemplateID">> => InvoiceTplID}
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoice_templates_api:delete_invoice_template(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, _Body} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec create_invoice(context(), binary(), map()) -> {ok, term()} | {error, term()}.
create_invoice(Context, InvoiceTplID, Request) ->
    Params = #{
        binding => #{<<"invoiceTemplateID">> => InvoiceTplID},
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoice_templates_api:create_invoice_with_template(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_invoice_payment_methods(context(), binary()) -> {ok, term()} | {error, term()}.
get_invoice_payment_methods(Context, InvoiceTplID) ->
    Params = #{binding => #{<<"invoiceTemplateID">> => InvoiceTplID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_invoice_templates_api:get_invoice_payment_methods_by_template_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
