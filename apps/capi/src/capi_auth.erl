-module(capi_auth).

-export([authorize_api_key/2]).
-export([authorize_operation/3]).
-export([issue_invoice_access_token/2]).

-type context() :: capi_authorizer_jwt:t().

-export_type([context/0]).

-spec authorize_api_key(
    OperationID :: swagger:operation_id(),
    ApiKey      :: swagger:api_key()
) -> {true, Context :: context()} | false.

authorize_api_key(OperationID, ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, {Type, Credentials}} ->
            case authorize_api_key(OperationID, Type, Credentials) of
                {ok, Context} ->
                    {true, Context};
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

-spec parse_api_key(ApiKey :: swagger:api_key()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_api_key(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec authorize_api_key(
    OperationID :: swagger:operation_id(),
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
    OperationID :: swagger:operation_id(),
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
-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 60 * 60). % 1 hour

-spec issue_invoice_access_token(PartyID :: binary(), InvoiceID :: binary()) ->
    {ok, capi_authorizer_jwt:token()} | {error, _}.

issue_invoice_access_token(PartyID, InvoiceID) ->
    ACL = capi_acl:from_list([
        {[{invoices, InvoiceID}]           , read},
        {[{invoices, InvoiceID}, payments] , read},
        {[{invoices, InvoiceID}, payments] , write},
        {[payment_tool_tokens]             , write}
    ]),
    capi_authorizer_jwt:issue(
        {{PartyID, ACL}, #{}},
        {lifetime, ?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME}
    ).

%%

-spec get_operation_access(swagger:operation_id(), request_data()) ->
    [{capi_acl:scope(), capi_acl:permission()}].

get_operation_access('CreateInvoice'             , _) ->
    [{[invoices], write}];
get_operation_access('GetInvoiceByID'            , #{'invoiceID' := ID}) ->
    [{[{invoices, ID}], read}];
get_operation_access('GetInvoiceEvents'          , #{'invoiceID' := ID}) ->
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
get_operation_access('SearchInvoices'            , _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments'            , _) ->
    [{[invoices, payments], read}];
get_operation_access('CreatePaymentToolToken'    , _) ->
    [{[payment_tool_tokens] , write}];
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
get_operation_access('CreateShop'                , _) ->
    [{[party], write}];
get_operation_access('SuspendShop'               , _) ->
    [{[party], write}];
get_operation_access('UpdateShop'                , _) ->
    [{[party], write}];
get_operation_access('SuspendMyParty'            , _) ->
    [{[party], write}];
get_operation_access('ActivateMyParty'           , _) ->
    [{[party], write}];
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
get_operation_access('CreatePayoutTool'          , _) ->
    [{[party], write}];
get_operation_access('GetContracts'              , _) ->
    [{[party], read}];
get_operation_access('CreateContract'            , _) ->
    [{[party], write}];
get_operation_access('GetContractByID'           , _) ->
    [{[party], read}];
get_operation_access('GetWebhooks'               , _) ->
    [{[party], read}];
get_operation_access('GetWebhookByID'            , _) ->
    [{[party], read}];
get_operation_access('CreateWebhook'             , _) ->
    [{[party], write}];
get_operation_access('DeleteWebhookByID'         , _) ->
    [{[party], write}];
get_operation_access('GetCategories'             , _) ->
    [];
get_operation_access('GetCategoryByRef'          , _) ->
    [];
get_operation_access('GetLocationsNames'         , _) ->
    [].
