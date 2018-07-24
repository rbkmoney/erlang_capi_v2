-module(capi_auth).

-export([authorize_api_key/2]).
-export([authorize_operation/3]).
-export([issue_access_token/2]).

-export([get_subject_id/1]).
-export([get_claims/1]).
-export([get_claim/2]).
-export([get_claim/3]).
-export([get_consumer/1]).

-export([get_resource_hierarchy/0]).

-type context () :: capi_authorizer_jwt:t().
-type claims  () :: capi_authorizer_jwt:claims().
-type consumer() :: client | merchant | provider.

-export_type([context /0]).
-export_type([claims  /0]).
-export_type([consumer/0]).

-spec authorize_api_key(
    OperationID :: swag_server:operation_id(),
    ApiKey      :: swag_server:api_key()
) -> {true, Context :: context()} | false.

authorize_api_key(OperationID, ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, {Type, Credentials}} ->
            case authorize_api_key(OperationID, Type, Credentials) of
                {ok, Context} ->
                    check_blacklist(Context, ApiKey);
                {error, Error} ->
                    _ = log_auth_error(OperationID, Error),
                    false
            end;
        {error, Error} ->
            _ = log_auth_error(OperationID, Error),
            false
    end.

log_auth_error(OperationID, Error) ->
    lager:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]).

-spec parse_api_key(ApiKey :: swag_server:api_key()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_api_key(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec authorize_api_key(
    OperationID :: swag_server:operation_id(),
    Type :: atom(),
    Credentials :: binary()
) ->
    {ok, Context :: context()} | {error, Reason :: atom()}.

authorize_api_key(_OperationID, bearer, Token) ->
    % NOTE
    % We are knowingly delegating actual request authorization to the logic handler
    % so we could gather more data to perform fine-grained access control.
    capi_authorizer_jwt:verify(Token).

%%

% TODO
% We need shared type here, exported somewhere in swagger app
-type request_data() :: #{atom() | binary() => term()}.

-spec authorize_operation(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Auth :: capi_authorizer_jwt:t()
) ->
    ok | {error, unauthorized}.

authorize_operation(OperationID, Req, {{_SubjectID, ACL}, _}) ->
    Access = get_operation_access(OperationID, Req),
    case lists:all(
        fun ({Scope, Permission}) ->
            lists:member(Permission, capi_acl:match(Scope, ACL))
        end,
        Access
    ) of
        true ->
            ok;
        false ->
            {error, unauthorized}
    end.

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
    capi_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    {Claims, ACL, Expiration} = resolve_token_spec(TokenSpec),
    capi_utils:unwrap(capi_authorizer_jwt:issue({{PartyID, capi_acl:from_list(ACL)}, Claims}, Expiration)).

-type acl() :: [{capi_acl:scope(), capi_acl:permission()}].

-spec resolve_token_spec(token_spec()) ->
    {claims(), acl(), capi_authorizer_jwt:expiration()}.
resolve_token_spec({invoice, InvoiceID}) ->
    Claims =
        #{
            <<"cons">> => <<"client">> % token consumer
        },
    ACL = [
        {[{invoices, InvoiceID}]           , read },
        {[{invoices, InvoiceID}, payments] , read },
        {[{invoices, InvoiceID}, payments] , write},
        {[payment_resources              ] , write}
    ],
    Expiration = {lifetime, ?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME},
    {Claims, ACL, Expiration};
resolve_token_spec({invoice_tpl, InvoiceTplID}) ->
    ACL = [
        {[party, {invoice_templates, InvoiceTplID}                           ], read },
        {[party, {invoice_templates, InvoiceTplID}, invoice_template_invoices], write}
    ],
    {#{}, ACL, unlimited};
resolve_token_spec({customer, CustomerID}) ->
    ACL = [
        {[{customers, CustomerID}], read},
        {[{customers, CustomerID}, bindings], read },
        {[{customers, CustomerID}, bindings], write},
        {[payment_resources], write}
    ],
    Expiration = {lifetime, ?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME},
    {#{}, ACL, Expiration}.

-spec get_subject_id(context()) -> binary().

get_subject_id({{SubjectID, _ACL}, _}) ->
    SubjectID.

-spec get_claims(context()) -> claims().

get_claims({_Subject, Claims}) ->
    Claims.

-spec get_claim(binary(), context()) -> term().

get_claim(ClaimName, {_Subject, Claims}) ->
    maps:get(ClaimName, Claims).

-spec get_claim(binary(), context(), term()) -> term().

get_claim(ClaimName, {_Subject, Claims}, Default) ->
    maps:get(ClaimName, Claims, Default).

%%

-spec get_operation_access(swag_server:operation_id(), request_data()) ->
    [{capi_acl:scope(), capi_acl:permission()}].

get_operation_access('CreateInvoice'             , _) ->
    [{[invoices], write}];
get_operation_access('GetInvoiceByID'            , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
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
get_operation_access('SearchInvoices'            , _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments'            , _) ->
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
    [].

-spec get_resource_hierarchy() -> #{atom() => map()}.

get_resource_hierarchy() ->
    #{
        party               => #{invoice_templates => #{invoice_template_invoices => #{}}},
        customers           => #{bindings => #{}},
        invoices            => #{payments => #{}},
        payment_resources => #{}
    }.

-spec get_consumer(claims()) ->
    consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client"  >> -> client;
        <<"provider">> -> provider
    end.

check_blacklist(Context, ApiKey) ->
    SubjectID = get_subject_id(Context),
    case capi_api_key_blacklist:check(SubjectID, ApiKey) of
        true ->
            _ = lager:warning("Blacklisted API Key usage detected for subject_id: ~p", [SubjectID]),
            false;
        false ->
            {true, Context}
    end.
