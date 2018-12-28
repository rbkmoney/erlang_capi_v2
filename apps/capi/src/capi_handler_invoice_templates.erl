-module(capi_handler_invoice_templates).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('CreateInvoiceTemplate', Req, Context, _) ->
    PartyID = get_party_id(Context),
    try
        CallArgs = [encode_invoice_tpl_create_params(PartyID, maps:get('InvoiceTemplateCreateParams', Req))],
        service_call_with([user_info, party_creation], {invoice_templating, 'Create', CallArgs}, Context)
    of
        {ok, InvoiceTpl} ->
            {ok, {201, [], make_invoice_tpl_and_token(InvoiceTpl, PartyID)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
            end
    catch
        throw:invoice_cart_empty ->
            {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        throw:zero_invoice_lifetime ->
            {ok, {400, [], logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}}
    end;

process_request('GetInvoiceTemplateByID', Req, Context, _) ->
    Call = {invoice_templating, 'Get', [maps:get('invoiceTemplateID', Req)]},
    case service_call_with([user_info, party_creation], Call, Context) of
        {ok, InvoiceTpl} ->
            {ok, {200, [], decode_invoice_tpl(InvoiceTpl)}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, {404, [], general_error(<<"Invoice template not found">>)}}
    end;

process_request('UpdateInvoiceTemplate', Req, Context, _) ->
    try
        Params = encode_invoice_tpl_update_params(maps:get('InvoiceTemplateUpdateParams', Req)),
        Call = {invoice_templating, 'Update', [maps:get('invoiceTemplateID', Req), Params]},
        service_call_with([user_info, party_creation], Call, Context)
    of
        {ok, InvoiceTpl} ->
            {ok, {200, [], decode_invoice_tpl(InvoiceTpl)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    catch
        throw:#payproc_InvalidUser{} ->
            {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
        throw:#payproc_InvoiceTemplateNotFound{} ->
            {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
        throw:#payproc_InvoiceTemplateRemoved{} ->
            {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
        throw:invoice_cart_empty ->
            {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        throw:zero_invoice_lifetime ->
            {ok, {400, [], logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}}
    end;

process_request('DeleteInvoiceTemplate', Req, Context, _) ->
    Call = {invoice_templating, 'Delete', [maps:get('invoiceTemplateID', Req)]},
    case service_call_with([user_info, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    end;

process_request('CreateInvoiceWithTemplate', Req, Context, _) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    InvoiceParams = maps:get('InvoiceParamsWithTemplate', Req),
    try
        Call = {invoicing, 'CreateWithTemplate', [encode_invoice_params_with_tpl(InvoiceTplID, InvoiceParams)]},
        service_call_with([user_info, party_creation], Call, Context)
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], make_invoice_and_token(Invoice, get_party_id(Context))}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, {404, [], general_error(<<"Invoice Template not found">>)}}
            end
    catch
        throw:{bad_invoice_params, currency_no_amount} ->
            {ok, {400, [], logic_error(invalidRequest, <<"Amount is required for the currency">>)}};
        throw:{bad_invoice_params, amount_no_currency} ->
            {ok, {400, [], logic_error(invalidRequest, <<"Currency is required for the amount">>)}}
    end;

process_request('GetInvoicePaymentMethodsByTemplateID', Req, Context, _) ->
    Result =
        construct_payment_methods(
            invoice_templating,
            [maps:get('invoiceTemplateID', Req), capi_utils:unwrap(rfc3339:format(erlang:system_time()))],
            Context
        ),
    case Result of
        {ok, PaymentMethods} when is_list(PaymentMethods) ->
            {ok, {200, [], PaymentMethods}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, {404, [], general_error(<<"Invoice template not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
