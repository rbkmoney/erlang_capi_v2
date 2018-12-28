-module(capi_handler).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(swag_server_logic_handler).

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
-export([decode_optional/2]).

-export([encode_currency/1]).
-export([encode_contact_info/1]).

%% API callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% Handler behaviour
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

-define(DEFAULT_INVOICE_META, #{}).
-define(DEFAULT_INVOICE_LINE_META, #{}).
-define(DEFAULT_URL_LIFETIME, 60). % seconds

-define(payment_institution_ref(PaymentInstitutionID),
    #domain_PaymentInstitutionRef{id = PaymentInstitutionID}).

-define(CAPI_NS, <<"com.rbkmoney.capi">>).

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
            {ok, {400, [], capi_handler_utils:logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>)}}
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

-spec encode_currency(binary()) ->
    tuple().

encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{symbolic_code = SymbolicCode}.

-spec encode_contact_info(map()) ->
    tuple().

encode_contact_info(ContactInfo) ->
    #domain_ContactInfo{
        phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
        email        = genlib_map:get(<<"email">>, ContactInfo)
    }.

%%

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

is_blocked({blocked  , _}) -> true;
is_blocked({unblocked, _}) -> false.

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
        <<"contactInfo">>   => capi_handler_utils:decode_contact_info(PrivateEntity#domain_RussianPrivateEntity.contact_info)
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
    binary().

decode_payment_institution_ref(#domain_PaymentInstitutionRef{id = Ref}) ->
    Ref.

-spec decode_optional(_, _) ->
    _.

decode_optional(Arg, DecodeFun) when Arg /= undefined ->
    DecodeFun(Arg);
decode_optional(undefined, _) ->
    undefined.
