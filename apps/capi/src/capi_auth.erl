-module(capi_auth).

-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_consumer/1]).

-export([get_operation_access/2]).

-export([get_access_config/0]).

-type context () :: uac:context().
-type claims  () :: uac:claims().
-type consumer() :: client | merchant | provider.

-export_type([context /0]).
-export_type([claims  /0]).
-export_type([consumer/0]).

-define(SIGNEE, capi).

%%

% TODO
% We need shared type here, exported somewhere in swagger app
-type request_data() :: #{atom() | binary() => term()}.

%%

%% TODO
%% Hardcode for now, should pass it here probably as an argument
-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-type token_spec() ::
      {invoice    , InvoiceID    :: binary()}
    | {invoice_tpl, InvoiceTplID :: binary()}
    | {customer   , CustomerID   :: binary()}
.

-spec issue_access_token(PartyID :: binary(), token_spec()) ->
    uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, #{}).

-spec issue_access_token(PartyID :: binary(), token_spec(), map()) ->
    uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, ExtraProperties) ->
    {Claims0, DomainRoles, LifeTime} = resolve_token_spec(TokenSpec),
    Claims = maps:merge(ExtraProperties, Claims0),
    capi_utils:unwrap(uac_authorizer_jwt:issue(
        capi_utils:get_unique_id(),
        LifeTime,
        PartyID,
        DomainRoles,
        Claims,
        ?SIGNEE
    )).

-spec resolve_token_spec(token_spec()) ->
    {claims(), uac_authorizer_jwt:domains(), uac_authorizer_jwt:expiration()}.
resolve_token_spec({invoice, InvoiceID}) ->
    Claims =
        #{
            <<"cons">> => <<"client">> % token consumer
        },
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[{invoices, InvoiceID}]           , read },
            {[{invoices, InvoiceID}, payments] , read },
            {[{invoices, InvoiceID}, payments] , write},
            {[payment_resources              ] , write}
        ])
    },
    Expiration = {lifetime, ?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME},
    {Claims, DomainRoles, Expiration};
resolve_token_spec({invoice_tpl, InvoiceTplID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[party, {invoice_templates, InvoiceTplID}                           ], read },
            {[party, {invoice_templates, InvoiceTplID}, invoice_template_invoices], write}
        ])
    },
    {#{}, DomainRoles, unlimited};
resolve_token_spec({customer, CustomerID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[{customers, CustomerID}], read},
            {[{customers, CustomerID}, bindings], read },
            {[{customers, CustomerID}, bindings], write},
            {[payment_resources], write}
        ])
    },
    Expiration = {lifetime, ?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME},
    {#{}, DomainRoles, Expiration}.

%%

-spec get_operation_access(swag_server:operation_id(), request_data()) ->
    [{uac_acl:scope(), uac_acl:permission()}].

get_operation_access('CreateInvoice'             , _) ->
    [{[invoices], write}];
get_operation_access('GetInvoiceByID'            , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('GetInvoiceByExternalID'    , _) ->
    [{[invoices], read}];
get_operation_access('GetInvoiceEvents'          , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('GetInvoicePaymentMethods'  , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('FulfillInvoice'            , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], write}];
get_operation_access('RescindInvoice'            , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], write}];
get_operation_access('CreateInvoiceAccessToken'  , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], write}];
get_operation_access('CreatePayment'             , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}, payments], write}];
get_operation_access('GetPayments'               , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}, payments], read}];
get_operation_access('GetPaymentByID'            , #{'invoiceID' := ID1, paymentID := ID2}) ->
    [{[{invoices, ID1}, {payments, ID2}], read}];
get_operation_access('GetPaymentByExternalID'    , _) ->
    [{[invoices, payments], read}];
get_operation_access('CancelPayment'             , #{'invoiceID' := ID1, paymentID := ID2}) ->
    [{[{invoices, ID1}, {payments, ID2}], write}];
get_operation_access('CapturePayment'            , #{'invoiceID' := ID1, paymentID := ID2}) ->
    [{[{invoices, ID1}, {payments, ID2}], write}];
get_operation_access('CreateRefund'              , _) ->
    [{[invoices, payments], write}];
get_operation_access('GetRefunds'                , _) ->
    [{[invoices, payments], read}];
get_operation_access('GetRefundByID'             , _) ->
    [{[invoices, payments], read}];
get_operation_access('GetRefundByExternalID'     , _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchInvoices'            , _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments'            , _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchRefunds'             , _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchPayouts'             , _) ->
    [{[party], read}];
get_operation_access('CreatePaymentResource'     , _) ->
    [{[payment_resources], write}];
get_operation_access('GetPaymentConversionStats' , _) ->
    [{[party], read}];
get_operation_access('GetPaymentRevenueStats'    , _) ->
    [{[party], read}];
get_operation_access('GetPaymentGeoStats'        , _) ->
    [{[party], read}];
get_operation_access('GetPaymentRateStats'       , _) ->
    [{[party], read}];
get_operation_access('GetPaymentMethodStats'     , _) ->
    [{[party], read}];
get_operation_access('GetMyParty'                , _) ->
    [{[party], read}];
get_operation_access('ActivateShop'              , _) ->
    [{[party], write}];
get_operation_access('SuspendShop'               , _) ->
    [{[party], write}];
get_operation_access('SuspendMyParty'            , _) ->
    [{[party], write}];
get_operation_access('ActivateMyParty'           , _) ->
    [{[party], write}];
get_operation_access('CreateClaim'               , _) ->
    [{[party], write}];
get_operation_access('GetClaims'                 , _) ->
    [{[party], read}];
get_operation_access('GetClaimByID'              , _) ->
    [{[party], read}];
get_operation_access('GetClaimsByStatus'         , _) ->
    [{[party], read}];
get_operation_access('RevokeClaimByID'           , _) ->
    [{[party], write}];
get_operation_access('GetAccountByID'            , _) ->
    [{[party], read}];
get_operation_access('GetShopByID'               , _) ->
    [{[party], read}];
get_operation_access('GetShops'                  , _) ->
    [{[party], read}];
get_operation_access('GetPayoutTools'            , _) ->
    [{[party], read}];
get_operation_access('GetPayoutToolByID'         , _) ->
    [{[party], read}];
get_operation_access('GetContracts'              , _) ->
    [{[party], read}];
get_operation_access('GetContractByID'           , _) ->
    [{[party], read}];
get_operation_access('GetContractAdjustments'    , _) ->
    [{[party], read}];
get_operation_access('GetContractAdjustmentByID' , _) ->
    [{[party], read}];
get_operation_access('GetReports'                , _) ->
    [{[party], read}];
get_operation_access('GetReport'                 , _) ->
    [{[party], read}];
get_operation_access('CreateReport'              , _) ->
    [{[party], write}];
get_operation_access('DownloadFile'              , _) ->
    [{[party], read}];
get_operation_access('GetWebhooks'               , _) ->
    [{[party], read}];
get_operation_access('GetWebhookByID'            , _) ->
    [{[party], read}];
get_operation_access('CreateWebhook'             , _) ->
    [{[party], write}];
get_operation_access('DeleteWebhookByID'         , _) ->
    [{[party], write}];
get_operation_access('CreateInvoiceTemplate'     , _) ->
    [{[party], write}];
get_operation_access('GetInvoiceTemplateByID'    , #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], read}];
get_operation_access('UpdateInvoiceTemplate'     , #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], write}];
get_operation_access('DeleteInvoiceTemplate'     , #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], write}];
get_operation_access('CreateInvoiceWithTemplate' , #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}, invoice_template_invoices], write}];
get_operation_access('GetInvoicePaymentMethodsByTemplateID', #{'invoiceTemplateID' := ID}) ->
    [{[party, {invoice_templates, ID}], read}];
get_operation_access('CreateCustomer'            , _) ->
    [{[customers], write}];
get_operation_access('GetCustomerById'           , #{'customerID' := ID}) ->
    [{[{customers, ID}], read}];
get_operation_access('DeleteCustomer'            , #{'customerID' := ID}) ->
    [{[{customers, ID}], write}];
get_operation_access('CreateCustomerAccessToken' , #{'customerID' := ID}) ->
    [{[{customers, ID}], write}];
get_operation_access('CreateBinding'             , #{'customerID' := ID}) ->
    [{[{customers, ID}, bindings], write}];
get_operation_access('GetBindings'               , #{'customerID' := ID}) ->
    [{[{customers, ID}, bindings], read}];
get_operation_access('GetBinding'                , #{'customerID' := ID1, 'customerBindingID' := ID2}) ->
    [{[{customers, ID1}, {bindings, ID2}], read}];
get_operation_access('GetCustomerEvents'         , #{'customerID' := ID}) ->
    [{[{customers, ID}], read}];
get_operation_access('GetCategories'             , _) ->
    [];
get_operation_access('GetCategoryByRef'          , _) ->
    [];
get_operation_access('GetScheduleByRef'          , _) ->
    [];
get_operation_access('GetPaymentInstitutions'    , _) ->
    [];
get_operation_access('GetPaymentInstitutionByRef', _) ->
    [];
get_operation_access('GetPaymentInstitutionPaymentTerms', _) ->
    [{[party], read}];
get_operation_access('GetPaymentInstitutionPayoutMethods', _) ->
    [{[party], read}];
get_operation_access('GetPaymentInstitutionPayoutSchedules', _) ->
    [{[party], read}];
get_operation_access('GetLocationsNames'         , _) ->
    [];
get_operation_access('CreatePayout'              , _) ->
    [{[payouts], write}];
get_operation_access('GetPayout'                 , _) ->
    [{[payouts], read}].

-spec get_access_config() -> map().

get_access_config() ->
    #{
        domain_name => <<"common-api">>,
        resource_hierarchy => get_resource_hierarchy()
    }.

-spec get_resource_hierarchy() -> #{atom() => map()}.

get_resource_hierarchy() ->
    #{
        party               => #{invoice_templates => #{invoice_template_invoices => #{}}},
        customers           => #{bindings => #{}},
        invoices            => #{payments => #{}},
        payment_resources   => #{},
        payouts             => #{},
        card_bins           => #{}
    }.

-spec get_consumer(claims()) ->
    consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client"  >> -> client;
        <<"provider">> -> provider
    end.
