-module(capi_handler_invoices).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('CreateInvoice', Req, Context, _) ->
    PartyID = get_party_id(Context),
    try
        Call = {invoicing, 'Create', [encode_invoice_params(PartyID, maps:get('InvoiceParams', Req))]},
        service_call_with([user_info, party_creation], Call, Context)
    of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, [], make_invoice_and_token(Invoice, PartyID)}};
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
        invoice_cart_empty ->
            {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
        invalid_invoice_cost ->
            {ok, {400, [], logic_error(invalidInvoiceCost, <<"Invalid invoice amount">>)}}
    end;

process_request('CreateInvoiceAccessToken', Req, Context, _) ->
    InvoiceID = maps:get(invoiceID, Req),
    case get_invoice_by_id(InvoiceID, Context) of
        {ok, #'payproc_Invoice'{}} ->
            {ok, {201, [], issue_access_token(get_party_id(Context), {invoice, InvoiceID})}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetInvoiceByID', Req, Context, _) ->
    case get_invoice_by_id(maps:get(invoiceID, Req), Context) of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {200, [], decode_invoice(Invoice)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('FulfillInvoice', Req, Context, _) ->
    Call = {invoicing, 'Fulfill', [maps:get(invoiceID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))]},
    case service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('RescindInvoice', Req, Context, _) ->
    Call = {invoicing, 'Rescind', [maps:get(invoiceID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))]},
    case service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
                #payproc_InvoicePaymentPending{} ->
                    {ok, {400, [], logic_error(invoicePaymentPending, <<"Invoice payment pending">>)}};
                #payproc_InvalidPartyStatus{} ->
                    {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
                #payproc_InvalidShopStatus{} ->
                    {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

process_request('GetInvoiceEvents', Req, Context, _) ->
    Result =
        collect_events(
            maps:get(limit, Req),
            genlib_map:get(eventID, Req),
            fun(Range) ->
                service_call_with([user_info], {invoicing, 'GetEvents', [maps:get(invoiceID, Req), Range]}, Context)
            end,
            fun decode_invoice_event/2,
            Context
        ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, [], Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_EventNotFound{} ->
                    {ok, {404, [], general_error(<<"Event not found">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetInvoicePaymentMethods', Req, Context, _) ->
    case construct_payment_methods(invoicing, [maps:get(invoiceID, Req)], Context) of
        {ok, PaymentMethods} when is_list(PaymentMethods) ->
            {ok, {200, [], PaymentMethods}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}};
                #payproc_InvoiceNotFound{} ->
                    {ok, {404, [], general_error(<<"Invoice not found">>)}}
            end
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
