-module(capi_auth).

-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_consumer/1]).

-export([get_operation_access/2]).

-export([get_access_config/0]).

-export([get_extra_properties/0]).
-export([authorize_operation/4]).

-type context() :: uac:context().
-type claims() :: uac:claims().
-type consumer() :: client | merchant | provider.
-type realm() :: binary().
-type auth_method() ::
    user_session_token.

-type metadata() :: #{
    auth_method => auth_method(),
    user_realm => realm()
}.

-type resolution() ::
    allowed
    | forbidden
    | {forbidden, _Reason}.

-export_type([context/0]).
-export_type([claims/0]).
-export_type([consumer/0]).
-export_type([auth_method/0]).
-export_type([metadata/0]).
-export_type([resolution/0]).

-define(SIGNEE, capi).

%%

%% TODO
%% Hardcode for now, should pass it here probably as an argument
-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type token_spec() ::
    {invoice, InvoiceID :: binary()}
    | {invoice_tpl, InvoiceTplID :: binary()}
    | {customer, CustomerID :: binary()}.

-spec issue_access_token(PartyID :: binary(), token_spec()) -> uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, #{}).

-spec issue_access_token(PartyID :: binary(), token_spec(), map()) -> uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, ExtraProperties) ->
    TokenID = capi_utils:get_unique_id(),
    AuthContext = resolve_bouncer_ctx(TokenSpec, PartyID),
    ContextFragment = bouncer_context_helpers:make_auth_fragment(
        AuthContext#{
            token => #{id => TokenID}
        }
    ),
    Claims1 = maps:merge(
        ExtraProperties,
        resolve_token_spec(TokenSpec)
    ),
    Claims2 = capi_bouncer:set_claim(
        bouncer_client:bake_context_fragment(ContextFragment),
        Claims1
    ),
    capi_utils:unwrap(
        uac_authorizer_jwt:issue(TokenID, PartyID, Claims2, ?SIGNEE)
    ).

-spec resolve_token_spec(token_spec()) -> claims().
resolve_token_spec({invoice, InvoiceID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[{invoices, InvoiceID}], read},
            {[{invoices, InvoiceID}, payments], read},
            {[{invoices, InvoiceID}, payments], write},
            {[payment_resources], write}
        ])
    },
    Expiration = lifetime_to_expiration(?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME),
    #{
        <<"exp">> => Expiration,
        <<"resource_access">> => DomainRoles,
        % token consumer
        <<"cons">> => <<"client">>
    };
resolve_token_spec({invoice_tpl, InvoiceTplID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[party, {invoice_templates, InvoiceTplID}], read},
            {[party, {invoice_templates, InvoiceTplID}, invoice_template_invoices], write}
        ])
    },
    #{
        <<"exp">> => unlimited,
        <<"resource_access">> => DomainRoles
    };
resolve_token_spec({customer, CustomerID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[{customers, CustomerID}], read},
            {[{customers, CustomerID}, bindings], read},
            {[{customers, CustomerID}, bindings], write},
            {[payment_resources], write}
        ])
    },
    Expiration = lifetime_to_expiration(?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME),
    #{
        <<"exp">> => Expiration,
        <<"resource_access">> => DomainRoles
    }.

-spec resolve_bouncer_ctx(token_spec(), _PartyID :: binary()) -> bouncer_context_helpers:auth_params().
resolve_bouncer_ctx({invoice, InvoiceID}, PartyID) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICEACCESSTOKEN,
        expiration => make_auth_expiration(lifetime_to_expiration(?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME)),
        scope => [
            #{
                party => #{id => PartyID},
                invoice => #{id => InvoiceID}
            }
        ]
    };
resolve_bouncer_ctx({invoice_tpl, InvoiceTemplateID}, PartyID) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICETEMPLATEACCESSTOKEN,
        expiration => make_auth_expiration(unlimited),
        scope => [
            #{
                party => #{id => PartyID},
                invoice_template => #{id => InvoiceTemplateID}
            }
        ]
    };
resolve_bouncer_ctx({customer, CustomerID}, PartyID) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_CUSTOMERACCESSTOKEN,
        expiration => make_auth_expiration(lifetime_to_expiration(?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME)),
        scope => [
            #{
                party => #{id => PartyID},
                customer => #{id => CustomerID}
            }
        ]
    }.

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
get_operation_access('GetLocationsNames', _) ->
    [];
get_operation_access('CreatePayout', _) ->
    [{[payouts], write}];
get_operation_access('GetPayout', _) ->
    [{[payouts], read}].

-spec get_access_config() -> uac_conf:options().
get_access_config() ->
    #{
        domain_name => <<"common-api">>,
        resource_hierarchy => get_resource_hierarchy()
    }.

-spec get_resource_hierarchy() -> uac_conf:resource_hierarchy().
get_resource_hierarchy() ->
    #{
        party => #{invoice_templates => #{invoice_template_invoices => #{}}},
        customers => #{bindings => #{}},
        invoices => #{payments => #{}},
        payment_resources => #{},
        payouts => #{},
        card_bins => #{}
    }.

-spec get_extra_properties() -> [binary()].
% Which claims are gonna make it to InvoiceAccessTokens
get_extra_properties() ->
    [
        <<"ip_replacement_allowed">>
    ].

-spec get_consumer(claims()) -> consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider
    end.

lifetime_to_expiration(Lt) when is_integer(Lt) ->
    genlib_time:unow() + Lt.

make_auth_expiration(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
make_auth_expiration(unlimited) ->
    undefined.

-spec authorize_operation(
    OperationID :: capi_handler:operation_id(),
    Prototypes :: capi_bouncer_context:prototypes(),
    Context :: capi_handler:processing_context(),
    Req :: capi_handler:request_data()
) -> resolution() | no_return().
authorize_operation(
    OperationID,
    Prototypes,
    Ctx = #{swagger_context := #{auth_context := AuthContext}},
    Req
) ->
    OperationACL = get_operation_access(OperationID, Req),
    OldAuthResult =
        case uac:authorize_operation(OperationACL, AuthContext) of
            ok ->
                allowed;
            {error, Reason} ->
                {forbidden, Reason}
        end,
    AuthResult = authorize_operation(Prototypes, Ctx),
    handle_auth_result(OldAuthResult, AuthResult).

handle_auth_result(allowed, allowed) ->
    allowed;
handle_auth_result(Res = {forbidden, _Reason}, forbidden) ->
    Res;
handle_auth_result(Res, undefined) ->
    Res;
handle_auth_result(OldRes, NewRes) ->
    _ = logger:warning("New auth ~p differ from old ~p", [NewRes, OldRes]),
    OldRes.

%% TODO: Remove this clause after all handlers will be implemented
authorize_operation([], _) ->
    undefined;
authorize_operation(Prototypes, #{swagger_context := ReqCtx, woody_context := WoodyCtx}) ->
    case capi_bouncer:extract_context_fragments(ReqCtx, WoodyCtx) of
        Fragments when Fragments /= undefined ->
            Fragments1 = capi_bouncer_context:build(Prototypes, Fragments, WoodyCtx),
            try
                capi_bouncer:judge(Fragments1, WoodyCtx)
            catch
                error:{woody_error, _Error} ->
                    % TODO
                    % This is temporary safeguard around bouncer integration put here so that
                    % external requests would remain undisturbed by bouncer intermittent failures.
                    % We need to remove it as soon as these two points come true:
                    % * bouncer proves to be stable enough,
                    % * capi starts depending on bouncer exclusively for authz decisions.
                    undefined
            end;
        undefined ->
            undefined
    end.
