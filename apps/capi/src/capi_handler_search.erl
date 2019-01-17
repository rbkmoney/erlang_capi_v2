-module(capi_handler_search).

-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('SearchInvoices', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => capi_handler_utils:get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => capi_handler_utils:get_time('fromTime', Req),
        <<"to_time"                  >> => capi_handler_utils:get_time('toTime', Req),
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

process_request('SearchPayments', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => capi_handler_utils:get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => capi_handler_utils:get_time('fromTime', Req),
        <<"to_time"                  >> => capi_handler_utils:get_time('toTime', Req),
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

process_request('SearchPayouts', Req, Context) ->
    Query = #{
        <<"merchant_id"    >> => capi_handler_utils:get_party_id(Context),
        <<"shop_id"        >> => genlib_map:get('shopID', Req),
        <<"from_time"      >> => capi_handler_utils:get_time('fromTime', Req),
        <<"to_time"        >> => capi_handler_utils:get_time('toTime', Req),
        <<"payout_statuses">> => [<<"confirmed">>, <<"paid">>],
        <<"payout_id"      >> => genlib_map:get('payoutID', Req),
        <<"payout_type"    >> => encode_payout_type(genlib_map:get('payoutToolType', Req))
    },
    Opts = #{
        thrift_fun => 'GetPayouts',
        decode_fun => fun decode_stat_payout/2
    },
    process_search_request(payouts, Query, Req, Context, Opts);

process_request('SearchRefunds', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => capi_handler_utils:get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"refund_id"                >> => genlib_map:get('refundID', Req),
        <<"from_time"                >> => capi_handler_utils:get_time('fromTime', Req),
        <<"to_time"                  >> => capi_handler_utils:get_time('toTime', Req),
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

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

process_search_request(QueryType, Query, Req, Context, Opts = #{thrift_fun := ThriftFun}) ->
    QueryParams = #{
        <<"size">> => genlib_map:get('limit', Req),
        <<"from">> => genlib_map:get('offset', Req)
    },
    ContinuationToken = genlib_map:get('continuationToken', Req),
    Call = {
        merchant_stat,
        ThriftFun,
        [
            capi_handler_encoder:encode_stat_request(
                capi_handler_utils:create_dsl(QueryType, Query, QueryParams),
                ContinuationToken
            )
        ]
    },
    process_search_request_result(QueryType, capi_handler_utils:service_call(Call, Context), Context, Opts).

process_search_request_result(QueryType, Result, Context, #{decode_fun := DecodeFun}) ->
    case Result of
        {ok, #merchstat_StatResponse{
            data = {QueryType, Data},
            total_count = TotalCount,
            continuation_token = ContinuationToken
        }} ->
            DecodedData = [DecodeFun(D, Context) || D <- Data],
            Resp = genlib_map:compact(#{
                <<"result">> => DecodedData,
                <<"totalCount">> => TotalCount,
                <<"continuationToken">> => ContinuationToken
            }),
            {ok, {200, [], Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
            {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
        {exception, #merchstat_BadToken{}} ->
            {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Invalid token">>)}}
    end.

merchstat_to_domain({bank_card, BankCard = #merchstat_BankCard{}}) ->
    {bank_card, #domain_BankCard{
        token          = BankCard#merchstat_BankCard.token,
        payment_system = BankCard#merchstat_BankCard.payment_system,
        bin            = BankCard#merchstat_BankCard.bin,
        masked_pan     = BankCard#merchstat_BankCard.masked_pan,
        token_provider = BankCard#merchstat_BankCard.token_provider
    }};
merchstat_to_domain({payment_terminal, #merchstat_PaymentTerminal{terminal_type = Type}}) ->
    {payment_terminal, #domain_PaymentTerminal{terminal_type = Type}};
merchstat_to_domain({digital_wallet, #merchstat_DigitalWallet{provider = Provider, id = ID}}) ->
    {digital_wallet, #domain_DigitalWallet{provider = Provider, id = ID}};
merchstat_to_domain({bank_card, #merchstat_PayoutCard{card = BankCard}}) ->
    merchstat_to_domain({bank_card, BankCard});
merchstat_to_domain({bank_account, {russian_payout_account, PayoutAccount}}) ->
    #merchstat_RussianPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {russian_bank_account, #domain_RussianBankAccount{
        account           = BankAccount#merchstat_RussianBankAccount.account,
        bank_name         = BankAccount#merchstat_RussianBankAccount.bank_name,
        bank_post_account = BankAccount#merchstat_RussianBankAccount.bank_post_account,
        bank_bik          = BankAccount#merchstat_RussianBankAccount.bank_bik
    }};
merchstat_to_domain({bank_account, {international_payout_account, PayoutAccount}}) ->
    #merchstat_InternationalPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {international_bank_account, merchstat_to_domain({international_bank_account, BankAccount})};
merchstat_to_domain({wallet, #merchstat_Wallet{wallet_id = WalletID}}) ->
    {wallet_info, #domain_WalletInfo{wallet_id = WalletID}};

merchstat_to_domain({international_bank_account, undefined}) ->
    undefined;
merchstat_to_domain({international_bank_account, BankAccount = #merchstat_InternationalBankAccount{}}) ->
    #domain_InternationalBankAccount{
        number         = BankAccount#merchstat_InternationalBankAccount.number,
        iban           = BankAccount#merchstat_InternationalBankAccount.iban,
        account_holder = BankAccount#merchstat_InternationalBankAccount.account_holder,
        bank           = merchstat_to_domain(
            {international_bank_details, BankAccount#merchstat_InternationalBankAccount.bank}
        ),
        correspondent_account = merchstat_to_domain(
            {international_bank_account, BankAccount#merchstat_InternationalBankAccount.correspondent_account}
        )
    };
merchstat_to_domain({international_bank_details, undefined}) ->
    undefined;
merchstat_to_domain({international_bank_details, Bank = #merchstat_InternationalBankDetails{}}) ->
    #domain_InternationalBankDetails{
            bic     = Bank#merchstat_InternationalBankDetails.bic,
            name    = Bank#merchstat_InternationalBankDetails.name,
            address = Bank#merchstat_InternationalBankDetails.address,
            country = Bank#merchstat_InternationalBankDetails.country,
            aba_rtn = Bank#merchstat_InternationalBankDetails.aba_rtn
    };

merchstat_to_domain({Status, #merchstat_InvoicePaymentPending{}}) ->
    {Status, #domain_InvoicePaymentPending{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentProcessed{}}) ->
    {Status, #domain_InvoicePaymentProcessed{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentCaptured{}}) ->
    {Status, #domain_InvoicePaymentCaptured{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentCancelled{}}) ->
    {Status, #domain_InvoicePaymentCancelled{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentRefunded{}}) ->
    {Status, #domain_InvoicePaymentRefunded{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentFailed{failure = Failure}}) ->
    NewFailure =
        case Failure of
            {failure, _} ->
                Failure;
            {operation_timeout, #merchstat_OperationTimeout{}} ->
                {operation_timeout, #domain_OperationTimeout{}}
        end,
    {Status, #domain_InvoicePaymentFailed{failure = NewFailure}};

merchstat_to_domain({Status, #merchstat_InvoiceUnpaid{}}) ->
    {Status, #domain_InvoiceUnpaid{}};
merchstat_to_domain({Status, #merchstat_InvoicePaid{}}) ->
    {Status, #domain_InvoicePaid{}};
merchstat_to_domain({Status, #merchstat_InvoiceCancelled{details = Details}}) ->
    {Status, #domain_InvoiceCancelled{details = Details}};
merchstat_to_domain({Status, #merchstat_InvoiceFulfilled{details = Details}}) ->
    {Status, #domain_InvoiceFulfilled{details = Details}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentRefundPending{}}) ->
    {Status, #domain_InvoicePaymentRefundPending{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentRefundSucceeded{}}) ->
    {Status, #domain_InvoicePaymentRefundSucceeded{}};
merchstat_to_domain({Status, #merchstat_InvoicePaymentRefundFailed{failure = Failure}}) ->
    NewFailure =
        case Failure of
            {failure, _} ->
                Failure;
            {operation_timeout, #merchstat_OperationTimeout{}} ->
                {operation_timeout, #domain_OperationTimeout{}}
        end,
    {Status, #domain_InvoicePaymentRefundFailed{failure = NewFailure}};

merchstat_to_domain({instant, #merchstat_InvoicePaymentFlowInstant{}}) ->
    {instant, #domain_InvoicePaymentFlowInstant{}};
merchstat_to_domain({hold, Hold}) ->
    {hold, #domain_InvoicePaymentFlowHold{
        on_hold_expiration = Hold#merchstat_InvoicePaymentFlowHold.on_hold_expiration,
        held_until         = Hold#merchstat_InvoicePaymentFlowHold.held_until
    }}.

%%

encode_payment_method('bankCard'       ) -> <<"bank_card">>;
encode_payment_method('paymentTerminal') -> <<"payment_terminal">>;
encode_payment_method(undefined        ) -> undefined.

encode_payout_type('PayoutAccount') -> <<"bank_account">>;
encode_payout_type('Wallet'       ) -> <<"wallet">>;
encode_payout_type(undefined      ) -> undefined.

%%

decode_stat_invoice(Invoice, _Context) ->
    capi_handler_utils:merge_and_compact(#{
        <<"id"         >> => Invoice#merchstat_StatInvoice.id,
        <<"shopID"     >> => Invoice#merchstat_StatInvoice.shop_id,
        <<"createdAt"  >> => Invoice#merchstat_StatInvoice.created_at,
        <<"dueDate"    >> => Invoice#merchstat_StatInvoice.due,
        <<"amount"     >> => Invoice#merchstat_StatInvoice.amount,
        <<"currency"   >> => Invoice#merchstat_StatInvoice.currency_symbolic_code,
        <<"metadata"   >> => capi_handler_decoder_utils:decode_context(Invoice#merchstat_StatInvoice.context),
        <<"product"    >> => Invoice#merchstat_StatInvoice.product,
        <<"description">> => Invoice#merchstat_StatInvoice.description,
        <<"cart"       >> => capi_handler_decoder_invoicing:decode_invoice_cart(Invoice#merchstat_StatInvoice.cart)
    }, decode_stat_invoice_status(Invoice#merchstat_StatInvoice.status)).

decode_stat_invoice_status(Status) ->
    capi_handler_decoder_invoicing:decode_invoice_status(merchstat_to_domain(Status)).

decode_stat_payment(Stat, Context) ->
    capi_handler_utils:merge_and_compact(#{
        <<"id"             >> => Stat#merchstat_StatPayment.id,
        <<"shortID"        >> => Stat#merchstat_StatPayment.short_id,
        <<"invoiceID"      >> => Stat#merchstat_StatPayment.invoice_id,
        <<"shopID"         >> => Stat#merchstat_StatPayment.shop_id,
        <<"createdAt"      >> => Stat#merchstat_StatPayment.created_at,
        <<"amount"         >> => Stat#merchstat_StatPayment.amount,
        <<"flow"           >> => decode_stat_payment_flow(Stat#merchstat_StatPayment.flow),
        <<"fee"            >> => Stat#merchstat_StatPayment.fee,
        <<"currency"       >> => Stat#merchstat_StatPayment.currency_symbolic_code,
        <<"payer"          >> => decode_stat_payer(Stat#merchstat_StatPayment.payer),
        <<"geoLocationInfo">> => decode_geo_location_info(Stat#merchstat_StatPayment.location_info),
        <<"metadata"       >> => capi_handler_decoder_utils:decode_context(Stat#merchstat_StatPayment.context),
        <<"makeRecurrent"  >> => capi_handler_decoder_invoicing:decode_make_recurrent(
            Stat#merchstat_StatPayment.make_recurrent
        ),
        <<"statusChangedAt">> => decode_status_changed_at(Stat#merchstat_StatPayment.status)
    }, decode_stat_payment_status(Stat#merchstat_StatPayment.status, Context)).

decode_stat_payer({customer, #merchstat_CustomerPayer{customer_id = ID}}) ->
    #{
        <<"payerType" >> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_stat_payer({recurrent, RecurrentPayer}) ->
    #merchstat_RecurrentPayer{
        recurrent_parent = RecurrentParent,
        phone_number = PhoneNumber,
        email = Email
    } = RecurrentPayer,
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email"      >> => Email
        }),
        <<"recurrentParentPayment">> => capi_handler_decoder_invoicing:decode_recurrent_parent(RecurrentParent)
    };
decode_stat_payer({payment_resource, PaymentResource}) ->
    #merchstat_PaymentResourcePayer{
        payment_tool = PaymentTool,
        session_id = PaymentSession,
        fingerprint = Fingerprint,
        ip_address = IP,
        phone_number = PhoneNumber,
        email = Email
    } = PaymentResource,
    genlib_map:compact(#{
        <<"payerType"         >> => <<"PaymentResourcePayer">>,
        <<"paymentToolToken"  >> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentSession"    >> => PaymentSession,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"clientInfo"        >> => genlib_map:compact(#{
            <<"ip"         >> => IP,
            <<"fingerprint">> => Fingerprint
        }),
        <<"contactInfo"       >> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email"      >> => Email
        })
    }).

decode_stat_payment_flow(Flow) ->
    capi_handler_decoder_invoicing:decode_flow(merchstat_to_domain(Flow)).

decode_stat_payment_status(PaymentStatus, Context) ->
    capi_handler_decoder_invoicing:decode_payment_status(merchstat_to_domain(PaymentStatus), Context).

decode_stat_payment_tool_token(PaymentTool) ->
    capi_handler_decoder_party:decode_payment_tool_token(merchstat_to_domain(PaymentTool)).

decode_stat_payment_tool_details(PaymentTool) ->
    capi_handler_decoder_party:decode_payment_tool_details(merchstat_to_domain(PaymentTool)).

decode_geo_location_info(#geo_ip_LocationInfo{city_geo_id = CityID, country_geo_id = CountryID}) ->
    #{
        <<"cityGeoID">> => CityID,
        <<"countryGeoID">> => CountryID
    };
decode_geo_location_info(undefined) ->
    undefined.

decode_status_changed_at({_, #merchstat_InvoicePaymentPending{}}) ->
    undefined;
decode_status_changed_at({_, #merchstat_InvoicePaymentProcessed{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentCaptured{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentCancelled{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentRefunded{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentFailed{at = ChangedAt}}) ->
    ChangedAt.

decode_stat_payout(Payout, _Context) ->
    capi_handler_utils:merge_and_compact(#{
        <<"id"               >> => Payout#merchstat_StatPayout.id,
        <<"shopID"           >> => Payout#merchstat_StatPayout.shop_id,
        <<"createdAt"        >> => Payout#merchstat_StatPayout.created_at,
        <<"amount"           >> => Payout#merchstat_StatPayout.amount,
        <<"fee"              >> => Payout#merchstat_StatPayout.fee,
        <<"currency"         >> => Payout#merchstat_StatPayout.currency_symbolic_code,
        <<"payoutToolDetails">> => decode_stat_payout_tool_details(Payout#merchstat_StatPayout.type),
        <<"payoutSummary"    >> => decode_stat_payout_summary(Payout#merchstat_StatPayout.summary)
    }, decode_stat_payout_status(Payout#merchstat_StatPayout.status)).

decode_stat_payout_status({cancelled, #merchstat_PayoutCancelled{details = Details}}) ->
    #{
        <<"status"             >> => <<"cancelled">>,
        <<"cancellationDetails">> => genlib:to_binary(Details)
    };
decode_stat_payout_status({Status, _}) ->
    #{
        <<"status">> => genlib:to_binary(Status)
    }.

decode_stat_payout_tool_details(PayoutType) ->
    capi_handler_decoder_party:decode_payout_tool_details(merchstat_to_domain(PayoutType)).

decode_stat_payout_summary(PayoutSummary) when is_list(PayoutSummary) ->
    [decode_stat_payout_summary_item(PayoutSummaryItem) || PayoutSummaryItem <- PayoutSummary];
decode_stat_payout_summary(undefined) ->
    undefined.

decode_stat_payout_summary_item(PayoutSummary) ->
    genlib_map:compact(#{
        <<"amount"  >> => PayoutSummary#merchstat_PayoutSummaryItem.amount,
        <<"fee"     >> => PayoutSummary#merchstat_PayoutSummaryItem.fee,
        <<"currency">> => PayoutSummary#merchstat_PayoutSummaryItem.currency_symbolic_code,
        <<"count"   >> => PayoutSummary#merchstat_PayoutSummaryItem.count,
        <<"fromTime">> => PayoutSummary#merchstat_PayoutSummaryItem.from_time,
        <<"toTime"  >> => PayoutSummary#merchstat_PayoutSummaryItem.to_time,
        <<"type"    >> => genlib:to_binary(PayoutSummary#merchstat_PayoutSummaryItem.operation_type)
    }).

decode_stat_refund(Refund, Context) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"invoiceID">> => Refund#merchstat_StatRefund.invoice_id,
            <<"paymentID">> => Refund#merchstat_StatRefund.payment_id,
            <<"id">>        => Refund#merchstat_StatRefund.id,
            <<"createdAt">> => Refund#merchstat_StatRefund.created_at,
            <<"amount">>    => Refund#merchstat_StatRefund.amount,
            <<"currency">>  => Refund#merchstat_StatRefund.currency_symbolic_code,
            <<"reason">>    => Refund#merchstat_StatRefund.reason
        },
        decode_stat_refund_status(Refund#merchstat_StatRefund.status, Context)
    ).

decode_stat_refund_status(RefundStatus, Context) ->
    capi_handler_decoder_invoicing:decode_refund_status(merchstat_to_domain(RefundStatus), Context).

