-module(capi_handler).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-behaviour(swag_server_logic_handler).

-export([encode_contact_info/1]).
-export([encode_client_info/1]).
-export([encode_payment_tool_token/1]).
-export([encode_cash/1]).
-export([encode_cash/2]).
-export([encode_currency/1]).
-export([encode_invoice_cart/1]).
-export([encode_stat_request/1]).
-export([encode_invoice_context/1]).
-export([encode_invoice_line_meta/1]).
-export([encode_residence/1]).

-export([decode_map/2]).
-export([decode_currency/1]).
-export([decode_shop_location/1]).
-export([decode_shop_details/1]).
-export([decode_business_schedule_ref/1]).
-export([decode_contact_info/1]).
-export([decode_party/1]).
-export([decode_contractor/1]).
-export([decode_legal_agreement/1]).
-export([decode_reporting_preferences/1]).
-export([decode_payment_institution_ref/1]).
-export([decode_payment_tool_token/1]).
-export([decode_payment_tool_details/1]).
-export([decode_last_digits/1]).
-export([decode_masked_pan/2]).
-export([decode_operation_failure/2]).
-export([decode_user_interaction_form/1]).
-export([decode_payment/3]).
-export([decode_user_interaction/1]).
-export([decode_refund/2]).
-export([decode_residence/1]).
-export([decode_category_ref/1]).
-export([decode_payout_tool_params/2]).
-export([decode_payout_tool_details/1]).
-export([decode_disposable_payment_resource/1]).
-export([decode_invoice_cart/1]).
-export([decode_invoice_line_tax_mode/1]).
-export([decode_invoice/1]).
-export([decode_payment_status/2]).
-export([decode_refund_status/2]).
-export([decode_context/1]).
-export([decode_invoice_status/1]).
-export([decode_recurrent_parent/1]).
-export([decode_flow/1]).
-export([decode_make_recurrent/1]).
-export([encode_content/2]).
-export([encode_stat_request/2]).
-export([is_blocked/1]).
-export([is_suspended/1]).
-export([decode_optional/2]).

-export([construct_payment_methods/3]).
-export([make_invoice_and_token/2]).

%% API callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% Handler behaviour
-export([process_request/4]).
-export_type([operation_id/0]).
-export_type([request_data/0]).
-export_type([request_context/0]).
-export_type([response/0]).
-export_type([processing_context/0]).

-callback process_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    Context     :: processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key()) ->
    Result :: false | {true, capi_auth:context()}.

authorize_api_key(OperationID, ApiKey) ->
    _ = capi_utils:logtag_process(operation_id, OperationID),
    capi_auth:authorize_api_key(OperationID, ApiKey).

-type request_data()        :: #{atom() | binary() => term()}.
-type operation_id()        :: swag_server:operation_id().
-type request_context()     :: swag_server:request_context().
-type response()            :: swag_server_logic_handler:response().
-type processing_context()  :: #{
    swagger_context := swag_server:request_context(),
    woody_context   := woody_context:ctx()
}.

get_handlers() ->
    [
        capi_handler_accounts,
        capi_handler_analytics,
        capi_handler_categories,
        capi_handler_claims,
        capi_handler_contracts,
        capi_handler_customers,
        capi_handler_geo,
        capi_handler_invoice_templates,
        capi_handler_invoices,
        capi_handler_parties,
        capi_handler_payment_institutions,
        capi_handler_payments,
        capi_handler_payouts,
        capi_handler_reports,
        capi_handler_search,
        capi_handler_shops,
        capi_handler_tokens,
        capi_handler_webhooks
    ].

-spec handle_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    SwagContext :: request_context()
) ->
    {ok | error,   response()}.

handle_request(OperationID, Req, SwagContext = #{auth_context := AuthContext}) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    try
        case capi_auth:authorize_operation(OperationID, Req, AuthContext) of
            ok ->
                WoodyContext = attach_deadline(Req, create_woody_context(Req, AuthContext)),
                Context = create_processing_context(SwagContext, WoodyContext),
                process_request(OperationID, Req, Context, get_handlers());
            {error, _} = Error ->
                _ = lager:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {ok, {401, [], undefined}}
        end
    catch
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        throw:{bad_deadline, Deadline} ->
            _ = lager:warning("Operation ~p failed due to invalid deadline ~p", [OperationID, Deadline]),
            ErrorMsg = capi_handler_utils:logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>),
            {ok, {400, [], ErrorMsg}}
    end.

-spec process_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    Context     :: processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request(OperationID, _Req, _Context, []) ->
    erlang:throw({handler_function_clause, OperationID});
process_request(OperationID, Req, Context, [Handler | Rest]) ->
    Handler:process_request(OperationID, Req, Context, Rest).

%%

create_processing_context(SwaggerContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwaggerContext
    }.

create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    _ = lager:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => capi_auth:get_subject_id(AuthContext),
        realm    => ?REALM,
        email    => capi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => capi_auth:get_claim(<<"name">> , AuthContext, undefined)
    }).

attach_deadline(#{'X-Request-Deadline' := undefined}, Context) ->
    Context;
attach_deadline(#{'X-Request-Deadline' := Header}, Context) ->
    case capi_utils:parse_deadline(Header) of
        {ok, Deadline} when Deadline /= undefined ->
            woody_context:set_deadline(Deadline, Context);
        _ ->
            throw({bad_deadline, Header})
    end.

process_woody_error(_Source, result_unexpected   , _Details) ->
    {error, capi_handler_utils:reply_5xx(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, capi_handler_utils:reply_5xx(503)};
process_woody_error(_Source, result_unknown      , _Details) ->
    {error, capi_handler_utils:reply_5xx(504)}.

%%

-spec encode_contact_info(map()) ->
    tuple().

encode_contact_info(ContactInfo) ->
    #domain_ContactInfo{
        phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
        email        = genlib_map:get(<<"email">>, ContactInfo)
    }.

-spec encode_client_info(map()) ->
    tuple().
encode_client_info(ClientInfo) ->
    #domain_ClientInfo{
        fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
        ip_address  = maps:get(<<"ip"         >>, ClientInfo)
    }.

-spec encode_payment_tool_token(_) ->
    tuple().

encode_payment_tool_token(Token) ->
    try capi_utils:base64url_to_map(Token) of
        #{<<"type">> := <<"bank_card">>} = Encoded ->
            encode_bank_card(Encoded);
        #{<<"type">> := <<"payment_terminal">>} = Encoded ->
            encode_payment_terminal(Encoded);
        #{<<"type">> := <<"digital_wallet">>} = Encoded ->
            encode_digital_wallet(Encoded)
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end.

encode_bank_card(BankCard) ->
    {bank_card, #domain_BankCard{
        token          = maps:get(<<"token">>, BankCard),
        payment_system = encode_payment_system(maps:get(<<"payment_system">>, BankCard)),
        bin            = maps:get(<<"bin">>, BankCard),
        masked_pan     = maps:get(<<"masked_pan">>, BankCard),
        token_provider = encode_token_provider(genlib_map:get(<<"token_provider">>, BankCard)),
        issuer_country = encode_residence(genlib_map:get(<<"issuer_country">>, BankCard)),
        bank_name      = genlib_map:get(<<"bank_name">>, BankCard),
        metadata       = encode_bank_card_metadata(genlib_map:get(<<"metadata">>, BankCard))
    }}.

encode_payment_system(PaymentSystem) ->
    binary_to_existing_atom(PaymentSystem, utf8).

encode_bank_card_metadata(undefined) ->
    undefined;
encode_bank_card_metadata(Meta) ->
    maps:map(fun(_, Data) -> capi_msgp_marshalling:marshal(Data) end, Meta).

encode_token_provider(TokenProvider) when TokenProvider /= undefined ->
    binary_to_existing_atom(TokenProvider, utf8);
encode_token_provider(undefined) ->
    undefined.

-spec encode_residence(_) ->
    _.

encode_residence(undefined) ->
    undefined;
encode_residence(Residence) when is_binary(Residence) ->
    try
        list_to_existing_atom(string:to_lower(binary_to_list(Residence)))
    catch
        error:badarg ->
            throw({encode_residence, invalid_residence})
    end.

encode_payment_terminal(#{<<"terminal_type">> := Type}) ->
    {payment_terminal, #domain_PaymentTerminal{
        terminal_type = binary_to_existing_atom(Type, utf8)
    }}.

encode_digital_wallet(#{<<"provider">> := Provider, <<"id">> := ID}) ->
    {digital_wallet, #domain_DigitalWallet{
        provider = binary_to_existing_atom(Provider, utf8),
        id       = ID
    }}.

-spec encode_cash(_) ->
    _.

encode_cash(Params) ->
    Amount   = genlib_map:get(<<"amount"  >>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_cash(Amount, Currency).

-spec encode_cash(_, _) ->
    _.

encode_cash(Amount, Currency) ->
    #domain_Cash{
        amount   = Amount,
        currency = encode_currency(Currency)
    }.

-spec encode_currency(binary()) ->
    tuple().

encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{symbolic_code = SymbolicCode}.

-spec encode_invoice_cart(_) ->
    _.

encode_invoice_cart(Params) ->
    Cart     = genlib_map:get(<<"cart"    >>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_invoice_cart(Cart, Currency).

encode_invoice_cart(Cart, Currency) when Cart =/= undefined, Cart =/= [] ->
    #domain_InvoiceCart{
        lines = [encode_invoice_line(Line, Currency) || Line <- Cart]
    };
encode_invoice_cart([], _) ->
    throw(invoice_cart_empty);
encode_invoice_cart(undefined, _) ->
    undefined.

encode_invoice_line(Line, Currency) ->
    Metadata = encode_invoice_line_meta(Line),
    Price = encode_cash(genlib_map:get(<<"price">>, Line), Currency),
    #domain_InvoiceLine{
        product  = genlib_map:get(<<"product" >>, Line),
        quantity = genlib_map:get(<<"quantity">>, Line),
        price    = Price,
        metadata = Metadata
    }.

-spec encode_invoice_line_meta(_) ->
    _.

-define(DEFAULT_INVOICE_LINE_META, #{}).

encode_invoice_line_meta(Line) ->
    case genlib_map:get(<<"taxMode">>, Line) of
        TaxMode when TaxMode =/= undefined ->
            TM = encode_invoice_line_tax_mode(TaxMode),
            #{<<"TaxMode">> => {str, TM}};
        undefined ->
            ?DEFAULT_INVOICE_LINE_META
    end.

encode_invoice_line_tax_mode(#{<<"type">> := <<"InvoiceLineTaxVAT">>} = TaxMode)  ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    genlib_map:get(<<"rate">>, TaxMode).

-spec encode_invoice_context(_) ->
    _.

-define(DEFAULT_INVOICE_META, #{}).

encode_invoice_context(Params) ->
    encode_invoice_context(Params, ?DEFAULT_INVOICE_META).

encode_invoice_context(Params, DefaultMeta) ->
    Context = genlib_map:get(<<"metadata">>, Params, DefaultMeta),
    encode_content(json, Context).

-spec encode_content(_, _) ->
    _.

encode_content(json, Data) ->
    #'Content'{
        type = <<"application/json">>,
        data = jsx:encode(Data)
    }.

-spec encode_stat_request(_) ->
    _.

encode_stat_request(Dsl) ->
    encode_stat_request(Dsl, undefined).

-spec encode_stat_request(_, _) ->
    _.

encode_stat_request(Dsl, ContinuationToken) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl), ContinuationToken);

encode_stat_request(Dsl, ContinuationToken) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl,
        continuation_token = ContinuationToken
    }.

%%

-spec decode_map(_, _) ->
    _.

decode_map(Items, Fun) ->
    lists:map(Fun, maps:values(Items)).

-spec decode_currency(tuple()) ->
    binary().

decode_currency(#domain_Currency   {symbolic_code = SymbolicCode}) -> SymbolicCode;
decode_currency(#domain_CurrencyRef{symbolic_code = SymbolicCode}) -> SymbolicCode.

-spec decode_shop_location(tuple()) ->
    map().

decode_shop_location({url, Location}) ->
    #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => Location
    }.

-spec decode_shop_details(tuple()) ->
    map().

decode_shop_details(#domain_ShopDetails{name = Name, description = Description}) ->
    genlib_map:compact(#{
        <<"name">> => Name,
        <<"description">> => Description
    }).

-spec decode_business_schedule_ref(tuple()) ->
    binary() | undefined.

decode_business_schedule_ref(#domain_BusinessScheduleRef{id = ID}) when ID /= undefined ->
    ID;
decode_business_schedule_ref(undefined) ->
    undefined.

-spec decode_contact_info(tuple()) ->
    map().

decode_contact_info(#domain_ContactInfo{phone_number = PhoneNumber, email = Email}) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => PhoneNumber,
        <<"email"      >> => Email
    }).

-spec decode_party(tuple()) ->
    map().

decode_party(#domain_Party{id = PartyID, blocking = Blocking, suspension = Suspension}) ->
    #{
        <<"id">> => PartyID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension)
    }.

-spec is_blocked(_) ->
    true | false.

is_blocked({blocked  , _}) -> true;
is_blocked({unblocked, _}) -> false.

-spec is_suspended(_) ->
    true | false.

is_suspended({suspended, _}) -> true;
is_suspended({active   , _}) ->false.

-spec decode_contractor(tuple()) ->
    map().

decode_contractor({legal_entity, LegalEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"LegalEntity">>}, decode_legal_entity(LegalEntity));

decode_contractor({private_entity, PrivateEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"PrivateEntity">>}, decode_private_entity(PrivateEntity));

decode_contractor({registered_user, RegisteredUser}) ->
    maps:merge(#{<<"contractorType">> => <<"RegisteredUser">>}, decode_registered_user(RegisteredUser)).

decode_legal_entity({international_legal_entity, LegalEntity}) ->
    genlib_map:compact(#{
        <<"entityType">> => <<"InternationalLegalEntity">>,
        <<"legalName"               >> => LegalEntity#domain_InternationalLegalEntity.legal_name,
        <<"tradingName"             >> => LegalEntity#domain_InternationalLegalEntity.trading_name,
        <<"registeredOffice"        >> => LegalEntity#domain_InternationalLegalEntity.registered_address,
        <<"principalPlaceOfBusiness">> => LegalEntity#domain_InternationalLegalEntity.actual_address,
        <<"registeredNumber"        >> => LegalEntity#domain_InternationalLegalEntity.registered_number
    }).

decode_private_entity({russian_private_entity, PrivateEntity}) ->
    #{
        <<"entityType">>    => <<"RussianPrivateEntity">>,
        <<"firstName">>     => PrivateEntity#domain_RussianPrivateEntity.first_name,
        <<"secondName">>    => PrivateEntity#domain_RussianPrivateEntity.second_name,
        <<"middleName">>    => PrivateEntity#domain_RussianPrivateEntity.middle_name,
        <<"contactInfo">>   => decode_contact_info(PrivateEntity#domain_RussianPrivateEntity.contact_info)
    }.

decode_registered_user(#domain_RegisteredUser{email = Email}) ->
    #{<<"email">> => Email}.

-spec decode_legal_agreement(tuple()) ->
    map().

decode_legal_agreement(
    #domain_LegalAgreement{
        signed_at = SignedAt,
        legal_agreement_id = ID,
        valid_until = ValidUntil
    }
) ->
    genlib_map:compact(#{
        <<"id"      >> => ID,
        <<"signedAt">> => SignedAt,
        <<"validUntil">> => ValidUntil
    }).

-spec decode_reporting_preferences(tuple()) ->
    map().

decode_reporting_preferences(#domain_ReportPreferences{
    service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
        schedule = ScheduleRef,
        signer = Signer
    }
}) ->
    #{
        <<"serviceAcceptanceActPreferences">> => #{
            <<"scheduleID">> => decode_business_schedule_ref(ScheduleRef),
            <<"signer">> => decode_representative(Signer)
        }
    };
decode_reporting_preferences(#domain_ReportPreferences{service_acceptance_act_preferences = undefined}) ->
    #{}.

decode_representative(#domain_Representative{
    position  = Position,
    full_name = Name,
    document  = Document
}) ->
    #{
        <<"position">> => Position,
        <<"fullName">> => Name,
        <<"document">> => decode_representative_document(Document)
    }.

decode_representative_document({articles_of_association, #domain_ArticlesOfAssociation{}}) ->
    #{
        <<"representativeDocumentType">> => <<"ArticlesOfAssociation">>
    };
decode_representative_document({power_of_attorney, LegalAgreement}) ->
    maps:merge(
        #{<<"representativeDocumentType">> => <<"PowerOfAttorney">>},
        decode_legal_agreement(LegalAgreement)
    ).

-spec decode_payment_institution_ref(tuple()) ->
    integer().

decode_payment_institution_ref(#domain_PaymentInstitutionRef{id = Ref}) ->
    Ref.

-spec decode_payment_tool_token(tuple()) ->
    _.

decode_payment_tool_token({bank_card, BankCard}) ->
    decode_bank_card(BankCard);
decode_payment_tool_token({payment_terminal, PaymentTerminal}) ->
    decode_payment_terminal(PaymentTerminal);
decode_payment_tool_token({digital_wallet, DigitalWallet}) ->
    decode_digital_wallet(DigitalWallet).

decode_bank_card(#domain_BankCard{
    'token'          = Token,
    'payment_system' = PaymentSystem,
    'bin'            = Bin,
    'masked_pan'     = MaskedPan,
    'token_provider' = TokenProvider,
    'issuer_country' = IssuerCountry,
    'bank_name'      = BankName,
    'metadata'       = Metadata
}) ->
    capi_utils:map_to_base64url(genlib_map:compact(#{
        <<"type"          >> => <<"bank_card">>,
        <<"token"         >> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin"           >> => Bin,
        <<"masked_pan"    >> => MaskedPan,
        <<"token_provider">> => TokenProvider,
        <<"issuer_country">> => IssuerCountry,
        <<"bank_name"     >> => BankName,
        <<"metadata"      >> => decode_bank_card_metadata(Metadata)
    })).

decode_bank_card_metadata(undefined) ->
    undefined;
decode_bank_card_metadata(Meta) ->
    maps:map(fun(_, Data) -> capi_msgp_marshalling:unmarshal(Data) end, Meta).

decode_payment_terminal(#domain_PaymentTerminal{
    terminal_type = Type
}) ->
    capi_utils:map_to_base64url(#{
        <<"type"         >> => <<"payment_terminal">>,
        <<"terminal_type">> => Type
    }).

decode_digital_wallet(#domain_DigitalWallet{
    provider = Provider,
    id = ID
}) ->
    capi_utils:map_to_base64url(#{
        <<"type"    >> => <<"digital_wallet">>,
        <<"provider">> => atom_to_binary(Provider, utf8),
        <<"id"      >> => ID
    }).

-spec decode_payment_tool_details(_) ->
    _.

decode_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>}).

decode_bank_card_details(BankCard, V) ->
    LastDigits = decode_last_digits(BankCard#domain_BankCard.masked_pan),
    Bin = BankCard#domain_BankCard.bin,
    capi_handler_utils:merge_and_compact(V, #{
        <<"lastDigits">>     => LastDigits,
        <<"bin">>            => Bin,
        <<"cardNumberMask">> => decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem" >> => genlib:to_binary(BankCard#domain_BankCard.payment_system),
        <<"tokenProvider" >> => decode_token_provider(BankCard#domain_BankCard.token_provider)
    }).

decode_token_provider(Provider) when Provider /= undefined ->
    genlib:to_binary(Provider);
decode_token_provider(undefined) ->
    undefined.

decode_payment_terminal_details(#domain_PaymentTerminal{terminal_type = Type}, V) ->
    V#{
        <<"provider">> => genlib:to_binary(Type)
    }.

decode_digital_wallet_details(#domain_DigitalWallet{provider = qiwi, id = ID}, V) ->
    V#{
        <<"digitalWalletDetailsType">> => <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask"         >> => mask_phone_number(ID)
    }.

-define(PAN_LENGTH, 16).

-spec decode_masked_pan(_, _) ->
    _.

decode_masked_pan(Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, ?PAN_LENGTH - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec decode_last_digits(_) ->
    _.

decode_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_last_digits(MaskedPan) ->
    MaskedPan.

mask_phone_number(PhoneNumber) ->
    capi_utils:redact(PhoneNumber, <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>).

-spec decode_operation_failure(_, _) ->
    _.

decode_operation_failure({operation_timeout, _}, _) ->
    capi_handler_utils:logic_error(timeout, <<"timeout">>);
decode_operation_failure({failure, #domain_Failure{code = Code, reason = Reason}}, _) ->
    capi_handler_utils:logic_error(Code, Reason).

-spec decode_payment(_, _, _) ->
    _.

decode_payment(InvoiceID, Payment, Context) ->
    #domain_Cash{
        amount   = Amount,
        currency = Currency
    } = Payment#domain_InvoicePayment.cost,
    capi_handler_utils:merge_and_compact(#{
        <<"id"                    >> => Payment#domain_InvoicePayment.id,
        <<"invoiceID"             >> => InvoiceID,
        <<"createdAt"             >> => Payment#domain_InvoicePayment.created_at,
        % TODO whoops, nothing to get it from yet
        <<"flow"                  >> => decode_flow(Payment#domain_InvoicePayment.flow),
        <<"amount"                >> => Amount,
        <<"currency"              >> => decode_currency(Currency),
        <<"payer"                 >> => decode_payer(Payment#domain_InvoicePayment.payer),
        <<"makeRecurrent"         >> => decode_make_recurrent(Payment#domain_InvoicePayment.make_recurrent)
    }, decode_payment_status(Payment#domain_InvoicePayment.status, Context)).

decode_payer({customer, #domain_CustomerPayer{customer_id = ID}}) ->
    #{
        <<"payerType" >> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_payer({recurrent, #domain_RecurrentPayer{recurrent_parent = RecurrentParent, contact_info = ContactInfo}}) ->
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"contactInfo">> => decode_contact_info(ContactInfo),
        <<"recurrentParentPayment">> => decode_recurrent_parent(RecurrentParent)
    };
decode_payer({payment_resource, #domain_PaymentResourcePayer{resource = Resource, contact_info = ContactInfo}}) ->
    maps:merge(
        #{
            <<"payerType"  >> => <<"PaymentResourcePayer">>,
            <<"contactInfo">> => decode_contact_info(ContactInfo)
        },
        decode_disposable_payment_resource(Resource)
    ).

-spec decode_payment_status(_, _) ->
    _.

decode_payment_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentFailed{failure = OperationFailure} ->
                decode_payment_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

decode_payment_operation_failure({operation_timeout, _}, _) ->
    payment_error(<<"timeout">>);
decode_payment_operation_failure({failure, Failure}, Context) ->
    case capi_auth:get_consumer(capi_auth:get_claims(capi_handler_utils:get_auth_context(Context))) of
        client ->
            payment_error(payproc_errors:match('PaymentFailure', Failure, fun payment_error_client_maping/1));
        merchant ->
            % чтобы не городить ещё один обход дерева как в payproc_errors проще отформатировать в текст,
            % а потом уже в json
            decode_payment_operation_failure_(
                binary:split(erlang:list_to_binary(payproc_errors:format_raw(Failure)), <<":">>, [global])
            )
    end.

decode_payment_operation_failure_([H|T]) ->
    R = payment_error(H),
    case T of
        [] -> R;
        _  -> R#{<<"subError">> => decode_payment_operation_failure_(T)}
    end.

-spec decode_flow(_) ->
    _.

decode_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};

decode_flow({hold, #domain_InvoicePaymentFlowHold{on_hold_expiration = OnHoldExpiration, held_until = HeldUntil}}) ->
    #{
        <<"type"            >> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil"       >> => HeldUntil
    }.

-spec decode_make_recurrent(_) ->
    _.

decode_make_recurrent(undefined) ->
    false;
decode_make_recurrent(Value) when is_boolean(Value) ->
    Value.

-spec decode_recurrent_parent(_) ->
    _.

decode_recurrent_parent(#domain_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    };
decode_recurrent_parent(#merchstat_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    }.

payment_error(Code) ->
    #{<<"code">> => Code}.

%% client error mapping
%% @see https://github.com/petrkozorezov/swag/blob/master/spec/definitions/PaymentError.yaml
-spec payment_error_client_maping(dmsl_payment_processing_errors_thrift:'PaymentFailure'()) ->
    binary().
payment_error_client_maping({preauthorization_failed, _})->
    <<"PreauthorizationFailed">>;
payment_error_client_maping({authorization_failed, {account_blocked, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {rejected_by_issuer, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {payment_tool_rejected, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_not_found, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_limit_exceeded, _}}) ->
    <<"AccountLimitsExceeded">>;
payment_error_client_maping({authorization_failed, {insufficient_funds, _}}) ->
    <<"InsufficientFunds">>;
payment_error_client_maping(_) ->
    <<"PaymentRejected">>.

-spec decode_disposable_payment_resource(_) ->
    _.

decode_disposable_payment_resource(Resource) ->
    #domain_DisposablePaymentResource{payment_tool = PaymentTool, payment_session_id = SessionID} = Resource,
    ClientInfo = decode_client_info(Resource#domain_DisposablePaymentResource.client_info),
    #{
        <<"paymentToolToken"  >> => decode_payment_tool_token(PaymentTool),
        <<"paymentSession"    >> => capi_handler_utils:wrap_payment_session(ClientInfo, SessionID),
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"clientInfo"        >> => ClientInfo
    }.

decode_client_info(undefined) ->
    undefined;
decode_client_info(ClientInfo) ->
    #{
        <<"fingerprint">> => ClientInfo#domain_ClientInfo.fingerprint,
        <<"ip"         >> => ClientInfo#domain_ClientInfo.ip_address
    }.

-spec decode_user_interaction(_) ->
    _.

decode_user_interaction({payment_terminal_reciept, TerminalReceipt}) ->
    #{
        <<"interactionType">> => <<"PaymentTerminalReceipt">>,
        <<"shortPaymentID" >> => TerminalReceipt#'PaymentTerminalReceipt'.short_payment_id,
        <<"dueDate"        >> => TerminalReceipt#'PaymentTerminalReceipt'.due
    };
decode_user_interaction({redirect, BrowserRequest}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => decode_browser_request(BrowserRequest)
    }.

decode_browser_request({get_request, #'BrowserGetRequest'{uri = UriTemplate}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => UriTemplate
    };
decode_browser_request({post_request, #'BrowserPostRequest'{uri = UriTemplate, form = UserInteractionForm}}) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => UriTemplate,
        <<"form">> => decode_user_interaction_form(UserInteractionForm)
    }.

-spec decode_user_interaction_form(_) ->
    _.

decode_user_interaction_form(Form) ->
    maps:fold(
        fun(K, V, Acc) ->
            F = #{
                <<"key">> => K,
                <<"template">> => V
            },
            [F | Acc]
        end,
        [],
        Form
    ).

-spec decode_refund(_, _) ->
    _.

decode_refund(Refund, Context) ->
    #domain_Cash{amount = Amount, currency = Currency} = Refund#domain_InvoicePaymentRefund.cash,
    capi_handler_utils:merge_and_compact(
        #{
            <<"id"       >> => Refund#domain_InvoicePaymentRefund.id,
            <<"createdAt">> => Refund#domain_InvoicePaymentRefund.created_at,
            <<"reason"   >> => Refund#domain_InvoicePaymentRefund.reason,
            <<"amount"   >> => Amount,
            <<"currency" >> => decode_currency(Currency)
        },
        decode_refund_status(Refund#domain_InvoicePaymentRefund.status, Context)
    ).

-spec decode_refund_status(_, _) ->
    _.

decode_refund_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentRefundFailed{failure = OperationFailure} ->
                decode_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

-spec decode_residence(_) ->
    _.

decode_residence(undefined) ->
    undefined;
decode_residence(Residence) when is_atom(Residence) ->
    list_to_binary(string:to_upper(atom_to_list(Residence))).

-spec decode_category_ref(_) ->
    _.

decode_category_ref(#domain_CategoryRef{id = CategoryRef}) ->
    CategoryRef.

-spec decode_payout_tool_params(_, _) ->
    _.

decode_payout_tool_params(Currency, Info) ->
    #{
        <<"currency">> => decode_currency(Currency),
        <<"details">> => decode_payout_tool_details(Info)
    }.

-spec decode_payout_tool_details(_) ->
    _.

decode_payout_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankCard">>});
decode_payout_tool_details({russian_bank_account, V}) ->
    decode_russian_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankAccount">>});
decode_payout_tool_details({international_bank_account, V}) ->
    decode_international_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>});
decode_payout_tool_details({wallet_info, V}) ->
    #{
        <<"detailsType">> => <<"PayoutToolDetailsWalletInfo">>,
        <<"walletID">> => V#domain_WalletInfo.wallet_id
    }.

decode_russian_bank_account(BankAccount, V) ->
    V#{
        <<"account"        >> => BankAccount#domain_RussianBankAccount.account,
        <<"bankName"       >> => BankAccount#domain_RussianBankAccount.bank_name,
        <<"bankPostAccount">> => BankAccount#domain_RussianBankAccount.bank_post_account,
        <<"bankBik"        >> => BankAccount#domain_RussianBankAccount.bank_bik
    }.
decode_international_bank_account(undefined, _) ->
    undefined;
decode_international_bank_account(BankAccount, V) ->
    genlib_map:compact(V#{
        <<"number">>                   => BankAccount#domain_InternationalBankAccount.number,
        <<"iban">>                     => BankAccount#domain_InternationalBankAccount.iban,
        <<"bankDetails">>              => decode_international_bank_details(
            BankAccount#domain_InternationalBankAccount.bank
        ),
        <<"correspondentBankAccount">> => decode_international_bank_account(
            BankAccount#domain_InternationalBankAccount.correspondent_account, #{}
        )
    }).

decode_international_bank_details(undefined) ->
    undefined;
decode_international_bank_details(Bank) ->
    genlib_map:compact(#{
         <<"bic">>         => Bank#domain_InternationalBankDetails.bic,
         <<"abartn">>      => Bank#domain_InternationalBankDetails.aba_rtn,
         <<"name">>        => Bank#domain_InternationalBankDetails.name,
         <<"countryCode">> => decode_residence(Bank#domain_InternationalBankDetails.country),
         <<"address">>     => Bank#domain_InternationalBankDetails.address
    }).

-spec decode_optional(_, _) ->
    _.

decode_optional(Arg, DecodeFun) when Arg /= undefined ->
    DecodeFun(Arg);
decode_optional(undefined, _) ->
    undefined.

%%

-spec construct_payment_methods(_, _, _) ->
    _.

construct_payment_methods(ServiceName, Args, Context) ->
    case compute_terms(ServiceName, Args, Context) of
        {ok, #domain_TermSet{payments = undefined}} ->
            {ok, []};
        {ok, #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = PaymentMethodRefs
            }
        }} ->
            {ok, decode_payment_methods(PaymentMethodRefs)};
        Error ->
            Error
    end.
decode_payment_methods(undefined) ->
    [];
decode_payment_methods({value, PaymentMethodRefs}) ->
    PaymentMethods = [ID || #domain_PaymentMethodRef{id = ID} <- PaymentMethodRefs],
    lists:foldl(
        fun(Method, Acc) ->
            {_, MethodTerms} = lists:unzip(proplists:lookup_all(Method, PaymentMethods)),
            decode_payment_method(Method, MethodTerms) ++ Acc
        end,
        [],
        proplists:get_keys(PaymentMethods)
    ).

decode_payment_method(bank_card, PaymentSystems) ->
    [#{<<"method">> => <<"BankCard">>, <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems)}];
decode_payment_method(payment_terminal, Providers) ->
    [#{<<"method">> => <<"PaymentTerminal">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}];
decode_payment_method(digital_wallet, Providers) ->
    [#{<<"method">> => <<"DigitalWallet">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}];
decode_payment_method(tokenized_bank_card, TokenizedBankCards) ->
    decode_tokenized_bank_cards(TokenizedBankCards).

decode_tokenized_bank_cards(TokenizedBankCards) ->
    PropTokenizedBankCards = [
        {TP, PS} || #domain_TokenizedBankCard{payment_system = PS, token_provider = TP} <- TokenizedBankCards
    ],
    lists:map(
        fun(TokenProvider) ->
            {_, PaymentSystems} = lists:unzip(proplists:lookup_all(TokenProvider, PropTokenizedBankCards)),
            decode_tokenized_bank_card(TokenProvider, PaymentSystems)
        end,
        proplists:get_keys(PropTokenizedBankCards)
    ).

decode_tokenized_bank_card(TokenProvider, PaymentSystems) ->
    #{
        <<"method">> => <<"BankCard">>,
        <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems),
        <<"tokenProviders">> => [genlib:to_binary(TokenProvider)]
    }.

compute_terms(ServiceName, Args, Context) ->
    capi_handler_utils:service_call_with([user_info], {ServiceName, 'ComputeTerms', Args}, Context).

-spec make_invoice_and_token(_, _) ->
    _.

make_invoice_and_token(Invoice, PartyID) ->
    #{
        <<"invoice"           >> => decode_invoice(Invoice),
        <<"invoiceAccessToken">> => capi_handler_utils:issue_access_token(PartyID, {invoice, Invoice#domain_Invoice.id})
    }.

-spec decode_invoice(_) ->
    _.

decode_invoice(Invoice) ->
    #domain_Cash{amount = Amount, currency = Currency} = Invoice#domain_Invoice.cost,
    #domain_InvoiceDetails{product = Product, description = Description, cart = Cart} =
        Invoice#domain_Invoice.details,
    capi_handler_utils:merge_and_compact(#{
        <<"id"               >> => Invoice#domain_Invoice.id,
        <<"shopID"           >> => Invoice#domain_Invoice.shop_id,
        <<"createdAt"        >> => Invoice#domain_Invoice.created_at,
        <<"dueDate"          >> => Invoice#domain_Invoice.due,
        <<"amount"           >> => Amount,
        <<"currency"         >> => decode_currency(Currency),
        <<"metadata"         >> => decode_context(Invoice#domain_Invoice.context),
        <<"product"          >> => Product,
        <<"description"      >> => Description,
        <<"cart"             >> => decode_invoice_cart(Cart),
        <<"invoiceTemplateID">> => Invoice#domain_Invoice.template_id
    }, decode_invoice_status(Invoice#domain_Invoice.status)).

-spec decode_invoice_status(_) ->
    _.

decode_invoice_status({Status, StatusInfo}) ->
    Reason =
        case StatusInfo of
            #domain_InvoiceCancelled{details = Details} -> Details;
            #domain_InvoiceFulfilled{details = Details} -> Details;
            _ -> undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"reason">> => Reason
    }.

-spec decode_invoice_cart(_) ->
    _.

decode_invoice_cart(#domain_InvoiceCart{lines = Lines}) ->
    [decode_invoice_line(L) || L <- Lines];
decode_invoice_cart(undefined) ->
    undefined.

decode_invoice_line(InvoiceLine = #domain_InvoiceLine{quantity = Quantity, price = #domain_Cash{amount = Price}}) ->
    genlib_map:compact(#{
        <<"product" >> => InvoiceLine#domain_InvoiceLine.product,
        <<"quantity">> => Quantity,
        <<"price"   >> => Price,
        <<"cost"    >> => Price * Quantity,
        <<"taxMode" >> => decode_invoice_line_tax_mode(InvoiceLine#domain_InvoiceLine.metadata)
    }).

-spec decode_invoice_line_tax_mode(_) ->
    _.

decode_invoice_line_tax_mode(#{<<"TaxMode">> := {str, TM}}) ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    #{
       <<"type">> => <<"InvoiceLineTaxVAT">>,
       <<"rate">> => TM
    };
decode_invoice_line_tax_mode(_) ->
    undefined.

-spec decode_context(_) ->
    _.

decode_context(#'Content'{type = <<"application/json">>, data = InvoiceContext}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);
decode_context(undefined) ->
    undefined.
