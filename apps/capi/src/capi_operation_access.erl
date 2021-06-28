-module(capi_operation_access).

%%

-export([get_operation_access/2]).

%%

-spec get_operation_access(swag_server:operation_id(), swag_server:object()) ->
    [{uac_acl:scope(), uac_acl:permission()}].
get_operation_access('CreateInvoice', _) ->
    [{[invoices], write}];
get_operation_access('GetInvoiceByID', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('GetInvoiceByExternalID', _) ->
    [{[invoices], read}];
get_operation_access('GetInvoiceEvents', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('GetInvoicePaymentMethods', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('FulfillInvoice', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], write}];
get_operation_access('RescindInvoice', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], write}];
get_operation_access('CreateInvoiceAccessToken', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], write}];
get_operation_access('CreatePayment', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}, payments], write}];
get_operation_access('GetPayments', #{'invoiceID' := ID}) ->
    [{[{invoices, ID}, payments], read}];
get_operation_access('GetPaymentByID', #{'invoiceID' := ID1, paymentID := ID2}) ->
    [{[{invoices, ID1}, {payments, ID2}], read}];
get_operation_access('GetPaymentByExternalID', _) ->
    [{[invoices, payments], read}];
get_operation_access('CancelPayment', #{'invoiceID' := ID1, paymentID := ID2}) ->
    [{[{invoices, ID1}, {payments, ID2}], write}];
get_operation_access('CapturePayment', #{'invoiceID' := ID1, paymentID := ID2}) ->
    [{[{invoices, ID1}, {payments, ID2}], write}];
get_operation_access('CreateRefund', _) ->
    [{[invoices, payments], write}];
get_operation_access('GetRefunds', _) ->
    [{[invoices, payments], read}];
get_operation_access('GetRefundByID', _) ->
    [{[invoices, payments], read}];
get_operation_access('GetRefundByExternalID', _) ->
    [{[invoices, payments], read}];
get_operation_access('GetChargebacks', _) ->
    [{[invoices, payments], read}];
get_operation_access('GetChargebackByID', _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchInvoices', _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments', _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchRefunds', _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchPayouts', _) ->
    [{[party], read}];
get_operation_access('CreatePaymentResource', _) ->
    [{[payment_resources], write}];
get_operation_access('GetPaymentConversionStats', _) ->
    [{[party], read}];
get_operation_access('GetPaymentRevenueStats', _) ->
    [{[party], read}];
get_operation_access('GetPaymentGeoStats', _) ->
    [{[party], read}];
get_operation_access('GetPaymentRateStats', _) ->
    [{[party], read}];
get_operation_access('GetPaymentMethodStats', _) ->
    [{[party], read}];
get_operation_access('ActivateShop', _) ->
    [{[party], write}];
get_operation_access('SuspendShop', _) ->
    [{[party], write}];
get_operation_access('GetMyParty', _) ->
    [{[party], read}];
get_operation_access('SuspendMyParty', _) ->
    [{[party], write}];
get_operation_access('ActivateMyParty', _) ->
    [{[party], write}];
get_operation_access('GetPartyByID', _) ->
    [{[party], read}];
get_operation_access('SuspendPartyByID', _) ->
    [{[party], write}];
get_operation_access('ActivatePartyByID', _) ->
    [{[party], write}];
get_operation_access('CreateClaim', _) ->
    [{[party], write}];
get_operation_access('GetClaims', _) ->
    [{[party], read}];
get_operation_access('GetClaimByID', _) ->
    [{[party], read}];
get_operation_access('GetClaimsByStatus', _) ->
    [{[party], read}];
get_operation_access('RevokeClaimByID', _) ->
    [{[party], write}];
get_operation_access('GetAccountByID', _) ->
    [{[party], read}];
get_operation_access('GetShopByID', _) ->
    [{[party], read}];
get_operation_access('GetShopsForParty', _) ->
    [{[party], read}];
get_operation_access('GetShopByIDForParty', _) ->
    [{[party], read}];
get_operation_access('ActivateShopForParty', _) ->
    [{[party], write}];
get_operation_access('SuspendShopForParty', _) ->
    [{[party], write}];
get_operation_access('GetShops', _) ->
    [{[party], read}];
get_operation_access('GetPayoutTools', _) ->
    [{[party], read}];
get_operation_access('GetPayoutToolByID', _) ->
    [{[party], read}];
get_operation_access('GetPayoutToolsForParty', _) ->
    [{[party], read}];
get_operation_access('GetPayoutToolByIDForParty', _) ->
    [{[party], read}];
get_operation_access('GetContracts', _) ->
    [{[party], read}];
get_operation_access('GetContractByID', _) ->
    [{[party], read}];
get_operation_access('GetContractAdjustments', _) ->
    [{[party], read}];
get_operation_access('GetContractAdjustmentByID', _) ->
    [{[party], read}];
get_operation_access('GetContractsForParty', _) ->
    [{[party], read}];
get_operation_access('GetContractByIDForParty', _) ->
    [{[party], read}];
get_operation_access('GetContractAdjustmentsForParty', _) ->
    [{[party], read}];
get_operation_access('GetContractAdjustmentByIDForParty', _) ->
    [{[party], read}];
get_operation_access('GetReports', _) ->
    [{[party], read}];
get_operation_access('GetReport', _) ->
    [{[party], read}];
get_operation_access('CreateReport', _) ->
    [{[party], write}];
get_operation_access('DownloadFile', _) ->
    [{[party], read}];
get_operation_access('GetReportsForParty', _) ->
    [{[party], read}];
get_operation_access('GetReportForParty', _) ->
    [{[party], read}];
get_operation_access('CreateReportForParty', _) ->
    [{[party], write}];
get_operation_access('DownloadFileForParty', _) ->
    [{[party], read}];
get_operation_access('GetWebhooks', _) ->
    [{[party], read}];
get_operation_access('GetWebhookByID', _) ->
    [{[party], read}];
get_operation_access('CreateWebhook', _) ->
    [{[party], write}];
get_operation_access('DeleteWebhookByID', _) ->
    [{[party], write}];
get_operation_access('CreateInvoiceTemplate', _) ->
    [{[party], write}];
get_operation_access('GetInvoiceTemplateByID', #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], read}];
get_operation_access('UpdateInvoiceTemplate', #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], write}];
get_operation_access('DeleteInvoiceTemplate', #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], write}];
get_operation_access('CreateInvoiceWithTemplate', #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}, invoice_template_invoices], write}];
get_operation_access('GetInvoicePaymentMethodsByTemplateID', #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], read}];
get_operation_access('CreateCustomer', _) ->
    [{[customers], write}];
get_operation_access('GetCustomerById', #{'customerID' := ID}) ->
    [{[{customers, ID}], read}];
get_operation_access('DeleteCustomer', #{'customerID' := ID}) ->
    [{[{customers, ID}], write}];
get_operation_access('CreateCustomerAccessToken', #{'customerID' := ID}) ->
    [{[{customers, ID}], write}];
get_operation_access('CreateBinding', #{'customerID' := ID}) ->
    [{[{customers, ID}, bindings], write}];
get_operation_access('GetBindings', #{'customerID' := ID}) ->
    [{[{customers, ID}, bindings], read}];
get_operation_access('GetBinding', #{'customerID' := ID1, 'customerBindingID' := ID2}) ->
    [{[{customers, ID1}, {bindings, ID2}], read}];
get_operation_access('GetCustomerEvents', #{'customerID' := ID}) ->
    [{[{customers, ID}], read}];
get_operation_access('GetCategories', _) ->
    [];
get_operation_access('GetCategoryByRef', _) ->
    [];
get_operation_access('GetScheduleByRef', _) ->
    [];
get_operation_access('GetPaymentInstitutions', _) ->
    [];
get_operation_access('GetPaymentInstitutionByRef', _) ->
    [];
get_operation_access('GetPaymentInstitutionPaymentTerms', _) ->
    [{[party], read}];
get_operation_access('GetPaymentInstitutionPayoutMethods', _) ->
    [{[party], read}];
get_operation_access('GetPaymentInstitutionPayoutSchedules', _) ->
    [{[party], read}];
get_operation_access(OperationID, _) when OperationID =:= 'GetCountries'; OperationID =:= 'GetCountryByID' ->
    [];
get_operation_access(OperationID, _) when OperationID =:= 'GetTradeBlocs'; OperationID =:= 'GetTradeBlocByID' ->
    [];
get_operation_access('GetLocationsNames', _) ->
    [];
get_operation_access('CreatePayout', _) ->
    [{[payouts], write}];
get_operation_access('GetPayout', _) ->
    [{[payouts], read}].
