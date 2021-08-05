-ifndef(capi_bouncer_data_included__).
-define(capi_bouncer_data_included__, ok).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-define(JUDGEMENT(Resolution), #bdcs_Judgement{resolution = Resolution}).
-define(ALLOWED, {allowed, #bdcs_ResolutionAllowed{}}).
-define(FORBIDDEN, {forbidden, #bdcs_ResolutionForbidden{}}).

-define(CTX_ENTITY(ID), #bctx_v1_Entity{id = ID}).

-define(CTX_CAPI(Op), #bctx_v1_ContextCommonAPI{op = Op}).

-define(CTX_CAPI_OP(ID), #bctx_v1_CommonAPIOperation{id = ID}).

-define(CTX_PARTY_OP(ID, PartyID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_SHOP_OP(ID, PartyID, ShopID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_CONTRACT_OP(ID, PartyID, ContractID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    contract = ?CTX_ENTITY(ContractID)
}).

-define(CTX_INVOICE_OP(ID, InvoiceID), #bctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID)
}).

-define(CTX_PAYMENT_OP(ID, InvoiceID, PaymentID), #bctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID)
}).

-define(CTX_PAYMENT_OP(ID, InvoiceID), #bctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID)
}).

-define(CTX_REFUND_OP(ID, InvoiceID, PaymentID, RefundID), #bctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID),
    refund = ?CTX_ENTITY(RefundID)
}).

-define(CTX_INVOICE_TPL_OP(ID, InvoiceTemplateID), #bctx_v1_CommonAPIOperation{
    id = ID,
    invoice_template = ?CTX_ENTITY(InvoiceTemplateID)
}).

-define(CTX_CUSTOMER_OP(ID, CustomerID), #bctx_v1_CommonAPIOperation{
    id = ID,
    customer = ?CTX_ENTITY(CustomerID)
}).

-define(CTX_BINDING_OP(ID, CustomerID, BindingID), #bctx_v1_CommonAPIOperation{
    id = ID,
    customer = ?CTX_ENTITY(CustomerID),
    binding = ?CTX_ENTITY(BindingID)
}).

-define(CTX_WEBHOOK_OP(ID, WebhookID), #bctx_v1_CommonAPIOperation{
    id = ID,
    webhook = ?CTX_ENTITY(WebhookID)
}).

-define(CTX_PAYOUT_OP(ID, PayoutID, PartyID), #bctx_v1_CommonAPIOperation{
    id = ID,
    payout = ?CTX_ENTITY(PayoutID),
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_SEARCH_INVOICE_OP(ID, PartyID, ShopID, InvoiceID, PaymentID, CustomerID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID),
    customer = ?CTX_ENTITY(CustomerID)
}).

-define(CTX_SEARCH_PAYMENT_OP(ID, PartyID, ShopID, InvoiceID, PaymentID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID)
}).

-define(CTX_SEARCH_PAYOUT_OP(ID, PartyID, ShopID, PayoutID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    payout = ?CTX_ENTITY(PayoutID)
}).

-define(CTX_REPORT_OP(ID, ReportID), #bctx_v1_CommonAPIOperation{
    id = ID,
    report = ?CTX_ENTITY(ReportID)
}).

-define(CTX_REPORT_OP(ID, PartyID, ShopID, ReportID, FileID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    report = ?CTX_ENTITY(ReportID),
    file = ?CTX_ENTITY(FileID)
}).

-define(CTX_FILE_OP(ID, ReportID, FileID), #bctx_v1_CommonAPIOperation{
    id = ID,
    report = ?CTX_ENTITY(ReportID),
    file = ?CTX_ENTITY(FileID)
}).

-define(CTX_SEARCH_REFUND_OP(ID, PartyID, ShopID, InvoiceID, PaymentID, RefundID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID),
    refund = ?CTX_ENTITY(RefundID)
}).

-define(CTX_SEARCH_OP(
    ID,
    PartyID,
    ShopID,
    InvoiceID,
    PaymentID,
    CustomerID,
    PayoutID,
    RefundID
),
    #bctx_v1_CommonAPIOperation{
        id = ID,
        party = ?CTX_ENTITY(PartyID),
        shop = ?CTX_ENTITY(ShopID),
        invoice = ?CTX_ENTITY(InvoiceID),
        payment = ?CTX_ENTITY(PaymentID),
        customer = ?CTX_ENTITY(CustomerID),
        payout = ?CTX_ENTITY(PayoutID),
        refund = ?CTX_ENTITY(RefundID)
    }
).

-define(CTX_CLAIM_OP(ID, PartyID, ClaimID), #bctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    claim = ?CTX_ENTITY(ClaimID)
}).

-define(CTX_INVOICE(ID, PartyID, ShopID), #bctx_v1_Invoice{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_INVOICE(ID, PartyID, ShopID, Payments), #bctx_v1_Invoice{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    payments = Payments
}).

-define(CTX_PAYMENT(ID), #bctx_v1_Payment{id = ID}).

-define(CTX_INVOICE_TPL(ID, PartyID, ShopID), #bctx_v1_InvoiceTemplate{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_CUSTOMER(ID, PartyID, ShopID), #bctx_v1_Customer{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_WEBHOOK(ID, PartyID), #bctx_v1_Webhook{
    id = ID,
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_PAYOUT(ID, PartyID, ContractID, ShopID), #bctx_v1_Payout{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    contract = ?CTX_ENTITY(ContractID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_REPORT(ID, PartyID, ShopID, Files), #bctx_v1_Report{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    files = Files
}).

-define(CTX_CONTEXT_REPORTS(Report), #bctx_v1_ContextReports{
    report = Report
}).

-define(assertContextMatches(Expect), fun(Context) ->
    try
        ?assertMatch(Expect, Context),
        {ok, ?JUDGEMENT(?ALLOWED)}
    catch
        error:AssertMatchError:Stacktrace ->
            logger:error("failed ~p at ~p", [AssertMatchError, Stacktrace]),
            {throwing, #bdcs_InvalidContext{}}
    end
end).

-endif.
