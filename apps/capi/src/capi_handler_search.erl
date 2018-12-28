-module(capi_handler_search).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('SearchInvoices', Req, Context, _) ->
    Query = #{
        <<"merchant_id"              >> => get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => get_time('fromTime', Req),
        <<"to_time"                  >> => get_time('toTime', Req),
        <<"invoice_status"           >> => genlib_map:get('invoiceStatus', Req),
        <<"payment_status"           >> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow"             >> => genlib_map:get('paymentFlow', Req),
        <<"payment_method"           >> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id"      >> => genlib_map:get('customerID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"payment_email"            >> => genlib_map:get('payerEmail', Req),
        <<"payment_ip"               >> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint"      >> => genlib_map:get('payerFingerprint', Req),
        <<"payment_last_digits"      >> => genlib_map:get('lastDigits', Req),
        <<"payment_amount"           >> => genlib_map:get('paymentAmount', Req),
        <<"invoice_amount"           >> => genlib_map:get('invoiceAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_bin"              >> => genlib_map:get('bin', Req)
    },
    Opts = #{
        thrift_fun => 'GetInvoices',
        decode_fun => fun decode_stat_invoice/2
    },
    process_search_request(invoices, Query, Req, Context, Opts);

process_request('SearchPayments', Req, Context, _) ->
    Query = #{
        <<"merchant_id"              >> => get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => get_time('fromTime', Req),
        <<"to_time"                  >> => get_time('toTime', Req),
        <<"payment_status"           >> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow"             >> => genlib_map:get('paymentFlow', Req),
        <<"payment_method"           >> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id"      >> => genlib_map:get('customerID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"payment_email"            >> => genlib_map:get('payerEmail', Req),
        <<"payment_ip"               >> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint"      >> => genlib_map:get('payerFingerprint', Req),
        <<"payment_last_digits"      >> => genlib_map:get('lastDigits', Req),
        <<"payment_amount"           >> => genlib_map:get('paymentAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_bin"              >> => genlib_map:get('bin', Req)
    },
    Opts = #{
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_payment/2
    },
    process_search_request(payments, Query, Req, Context, Opts);

process_request('SearchPayouts', Req, Context, _) ->
    Query = #{
        <<"merchant_id"    >> => get_party_id(Context),
        <<"shop_id"        >> => genlib_map:get('shopID', Req),
        <<"from_time"      >> => get_time('fromTime', Req),
        <<"to_time"        >> => get_time('toTime', Req),
        <<"payout_statuses">> => [<<"confirmed">>, <<"paid">>],
        <<"payout_id"      >> => genlib_map:get('payoutID', Req),
        <<"payout_type"    >> => encode_payout_type(genlib_map:get('payoutToolType', Req))
    },
    Opts = #{
        thrift_fun => 'GetPayouts',
        decode_fun => fun decode_stat_payout/2
    },
    process_search_request(payouts, Query, Req, Context, Opts);

process_request('SearchRefunds', Req, Context, _) ->
    Query = #{
        <<"merchant_id"              >> => get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"refund_id"                >> => genlib_map:get('refundID', Req),
        <<"from_time"                >> => get_time('fromTime', Req),
        <<"to_time"                  >> => get_time('toTime', Req),
        <<"refund_status"            >> => genlib_map:get('refundStatus', Req)
    },
    Opts = #{
        %% TODO no special fun for refunds so we can use any
        %% should be fixed in new magista
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_refund/2
    },
    process_search_request(refunds, Query, Req, Context, Opts);

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
