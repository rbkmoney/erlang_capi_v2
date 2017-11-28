-define(STRING, <<"TEST">>).
-define(RUB, <<"RUB">>).
-define(JSON, <<"{}">>).
-define(INTEGER, 10000).
-define(INTEGER_BINARY, <<"10000">>).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).

-define(DETAILS, #domain_InvoiceDetails{
        product = ?STRING,
        description = ?STRING
    }).

-define(CASH, #domain_Cash{
    amount = ?INTEGER,
    currency = #domain_CurrencyRef{
        symbolic_code = ?RUB
    }
}).

-define(CONTENT, #'Content'{
    type = <<"application/json">>,
    data = ?JSON
}).

-define(LIFETIME_INTERVAL, #domain_LifetimeInterval{
    years = ?INTEGER,
    months =?INTEGER,
    days = ?INTEGER
}).

-define(TPL_CASH, {fixed, ?CASH}).

-define(
    INVOICE_STATUS(Status),
    case Status of
        unpaid ->
            {unpaid, #domain_InvoiceUnpaid{}};
        paid ->
            {paid, #domain_InvoicePaid{}};
        cancelled ->
            {cancelled, #domain_InvoiceCancelled{details = ?STRING}};
        fulfilled ->
            {fulfilled, #domain_InvoiceFulfilled{details = ?STRING}}
    end
).

-define(INVOICE, #domain_Invoice{
    id          = ?STRING,
    created_at  = ?TIMESTAMP,
    status      = ?INVOICE_STATUS(unpaid),
    due         = ?TIMESTAMP,
    details     = ?DETAILS,
    cost        = ?CASH,
    context     = ?CONTENT,
    shop_id     = ?STRING,
    owner_id    = ?STRING,
    template_id = ?STRING
}).

-define(PAYPROC_INVOICE, #payproc_Invoice{
    invoice = ?INVOICE,
    payments = []
}).

-define(INVOICE_LINE, #domain_InvoiceLine{
    product = ?STRING,
    quantity = ?INTEGER,
    price = ?CASH,
    metadata = #{?STRING => {obj, #{}}}
}).

-define(INVOICE_TPL, #domain_InvoiceTemplate{
    id          = ?STRING,
    details     = {product, #domain_InvoiceTemplateProduct{
        product = ?STRING,
        price = ?TPL_CASH,
        metadata = #{?STRING => {obj, #{}}}
    }},
    product     = ?STRING,
    context     = ?CONTENT,
    shop_id     = ?STRING,
    owner_id    = ?STRING,
    invoice_lifetime = ?LIFETIME_INTERVAL
}).

-define(BANK_CARD, #domain_BankCard{
    token = ?STRING,
    payment_system = visa,
    bin = ?STRING,
    masked_pan = <<"TEST1234">>
}).

-define(CONTACT_INFO, #domain_ContactInfo{
        phone_number = ?STRING,
        email = <<"test@test.ru">>
    }).

-define(DISP_PAYMENT_RESOURCE, #domain_DisposablePaymentResource{
    payment_tool = {bank_card, ?BANK_CARD},
    payment_session_id = ?STRING,
    client_info = #domain_ClientInfo{
        fingerprint = ?STRING,
        ip_address = ?STRING
    }
}).

-define(PAYMENT_RESOURCE_PAYER, #domain_PaymentResourcePayer{
    resource = ?DISP_PAYMENT_RESOURCE,
    contact_info = ?CONTACT_INFO
}).

-define(PAYER, {payment_resource, ?PAYMENT_RESOURCE_PAYER}).

-define(PAYMENT, #domain_InvoicePayment{
    id              = ?STRING,
    created_at      = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    status          = {pending, #domain_InvoicePaymentPending{}},
    payer           = ?PAYER,
    cost            = ?CASH,
    flow            = {instant, #domain_InvoicePaymentFlowInstant{}},
    context         = ?CONTENT
}).

-define(PAYPROC_PAYMENT, #payproc_InvoicePayment{
    payment = ?PAYMENT,
    refunds = [?REFUND],
    adjustments = [?ADJUSTMENT]
}).

-define(ACCOUNT_STATE, #payproc_AccountState{
    account_id = ?INTEGER,
    own_amount = ?INTEGER,
    available_amount = ?INTEGER,
    currency = #domain_Currency{
        name = ?STRING,
        symbolic_code = ?RUB,
        numeric_code = ?INTEGER,
        exponent = ?INTEGER
    }
}).

-define(REFUND, #domain_InvoicePaymentRefund{
    id = ?STRING,
    status = {pending, #domain_InvoicePaymentRefundPending{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?STRING
}).

-define(CONTRACT, #domain_Contract{
    id = ?STRING,
    contractor = {registered_user, #domain_RegisteredUser{email = ?STRING}},
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    status = {active, #domain_ContractActive{}},
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER},
    adjustments = [?CONTRACT_ADJUSTMENT],
    payout_tools = [?PAYOUT_TOOL]
}).

-define(BLOCKING, {unblocked, #domain_Unblocked{
        reason = ?STRING,
        since = ?TIMESTAMP
}}).

-define(SUSPENTION, {active, #domain_Active{since = ?TIMESTAMP}}).

-define(SHOP, #domain_Shop{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    details = #domain_ShopDetails{name = ?STRING},
    location = {url, ?STRING},
    category = #domain_CategoryRef{id = ?INTEGER},
    contract_id = ?STRING
}).

-define(PARTY, #domain_Party{
    id = ?STRING,
    contact_info = #domain_PartyContactInfo{email = ?STRING},
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    contracts = #{?STRING => ?CONTRACT},
    shops = #{?STRING => ?SHOP}
}).

-define(CLAIM, #payproc_Claim{
    id = ?INTEGER,
    revision = ?INTEGER,
    created_at = ?TIMESTAMP,
    updated_at = ?TIMESTAMP,
    status = {pending, #payproc_ClaimPending{}},
    changeset = []
}).

-define(ADJUSTMENT, #domain_InvoicePaymentAdjustment{
    id = ?STRING,
    status = {pending, #domain_InvoicePaymentAdjustmentPending{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?STRING,
    new_cash_flow = [],
    old_cash_flow_inverse = []
}).

-define(CONTRACT_ADJUSTMENT, #domain_ContractAdjustment{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER}
}).

-define(PAYOUT_TOOL, #domain_PayoutTool{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    currency = #domain_CurrencyRef{symbolic_code = ?RUB},
    payout_tool_info = {bank_account, #domain_BankAccount{
        account = <<"12345678901234567890">>,
        bank_name = ?STRING,
        bank_post_account = <<"12345678901234567890">>,
        bank_bik = <<"123456789">>
    }}
}).

-define(WEBHOOK, #webhooker_Webhook{
    id = ?INTEGER,
    party_id = ?STRING,
    event_filter = {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ?STRING,
        types = [{created, #webhooker_InvoiceCreated{}}]
    }},
    url = ?STRING,
    pub_key = ?STRING,
    enabled = true
}).

-define (STAT_RESPONSE(Data), #merchstat_StatResponse{
    data = Data,
    total_count = ?INTEGER}
).

-define(STAT_RESPONSE_INVOICES, ?STAT_RESPONSE({invoices, [?STAT_INVOICE]})).

-define(STAT_RESPONSE_PAYMENTS, ?STAT_RESPONSE({payments, [?STAT_PAYMENT]})).
-define(STAT_RESPONSE_PAYOUTS,  ?STAT_RESPONSE({payouts,  [?STAT_PAYOUT]})).

-define(STAT_RESPONSE_RECORDS, ?STAT_RESPONSE({records, [?STAT_RECORD]})).

-define(STAT_INVOICE, #merchstat_StatInvoice{
    id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {unpaid, #merchstat_InvoiceUnpaid{}},
    product = ?STRING,
    description = ?STRING,
    due  = ?TIMESTAMP,
    amount = ?INTEGER,
    currency_symbolic_code = ?RUB,
    context = ?CONTENT
}).

-define(STAT_PAYMENT, #merchstat_StatPayment{
    id = ?STRING,
    invoice_id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {pending, #merchstat_InvoicePaymentPending{}},
    amount = ?INTEGER,
    flow = {instant, #merchstat_InvoicePaymentFlowInstant{}},
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    payer = {payment_resource, #merchstat_PaymentResourcePayer{
            payment_tool = {bank_card, #merchstat_BankCard{
            token = ?STRING,
            payment_system = visa,
            bin = ?STRING,
            masked_pan = <<"TEST1234">>
        }},
        ip_address = ?STRING,
        fingerprint = ?STRING,
        phone_number = ?STRING,
        email = <<"test@test.ru">>,
        session_id = ?STRING
    }},
    context = ?CONTENT
}).

-define(STAT_PAYOUT, #merchstat_StatPayout{
    id = ?STRING,
    party_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {paid, #merchstat_PayoutPaid{}},
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    type = {bank_account, #merchstat_PayoutAccount{
        account = #merchstat_BankAccount{
            account = <<"12345678901234567890">>,
            bank_name = ?STRING,
            bank_post_account = <<"12345678901234567890">>,
            bank_bik = <<"123456789">>
        },
        inn = <<"12345678901234567890">>,
        purpose = ?STRING
    }}
}).

-define(STAT_RECORD, #{
    <<"offset">> => ?INTEGER_BINARY,
    <<"successful_count">> => ?INTEGER_BINARY,
    <<"total_count">> => ?INTEGER_BINARY,
    <<"conversion">> => ?INTEGER_BINARY,
    <<"city_id">> => ?INTEGER_BINARY,
    <<"currency_symbolic_code">> => ?RUB,
    <<"amount_with_fee">> => ?INTEGER_BINARY,
    <<"amount_without_fee">> => ?INTEGER_BINARY,
    <<"unic_count">> => ?INTEGER_BINARY,
    <<"total_count">> => ?INTEGER_BINARY,
    <<"payment_system">> => <<"visa">>
}).

-define(REPORT, #reports_Report{
    report_id = ?INTEGER,
    time_range = #reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = provision_of_service,
    status = created,
    files = [
        #reports_FileMeta{
            file_id = ?STRING,
            filename = ?STRING,
            signature = #reports_Signature{
                md5 = ?STRING,
                sha256 = ?STRING
            }
        }
    ]
}).

-define(SNAPSHOT, #'Snapshot'{
    version = ?INTEGER,
    domain = #{
        {category, #domain_CategoryRef{id = ?INTEGER}} =>
        {category, #domain_CategoryObject{
            ref = #domain_CategoryRef{id = ?INTEGER},
            data = #domain_Category{
                name = ?STRING,
                description = ?STRING
            }
        }}
    }
}).

-define(INVOICE_EVENT(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    payload =  {invoice_changes,
        [
            {invoice_created, #payproc_InvoiceCreated{invoice = ?INVOICE}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(unpaid)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(paid)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(cancelled)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(fulfilled)}}
        ]
    },
    source =  {invoice_id, ?STRING}
}).

-define(INVOICE_EVENT_PRIVATE(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    payload =  {invoice_changes,
        [
            {invoice_payment_change, #payproc_InvoicePaymentChange{
                id = <<"1">>,
                payload = {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
                    target = {processed, #domain_InvoicePaymentProcessed{}},
                    payload = {session_started, #payproc_SessionStarted{}}}
                }}
            }
        ]
    },
    source =  {invoice_id, ?STRING}
}).

-define(TERM_SET, #domain_TermSet{}).

-define(CUSTOMER, #payproc_Customer{
    id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    status = {ready, #payproc_CustomerReady{}},
    created_at = ?TIMESTAMP,
    bindings = [?CUSTOMER_BINDING],
    contact_info = ?CONTACT_INFO,
    metadata = {obj, #{}}
}).

-define(CUSTOMER_BINDING, #payproc_CustomerBinding{
    id = ?STRING,
    rec_payment_tool_id = ?STRING,
    payment_resource = ?DISP_PAYMENT_RESOURCE,
    status = {succeeded, #payproc_CustomerBindingSucceeded{}}
}).

-define(PUT_CARD_DATA_RESULT, #'PutCardDataResult'{
    bank_card = ?BANK_CARD,
    session_id = ?STRING
}).
