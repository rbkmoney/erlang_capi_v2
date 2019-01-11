-module(capi_handler_payment_institutions).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/4]).

-define(payment_institution_ref(PaymentInstitutionID),
    #domain_PaymentInstitutionRef{id = PaymentInstitutionID}).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetPaymentInstitutions', Req, #{woody_context := WoodyContext}, _) ->
    try
        Residence = capi_handler_encoder:encode_residence(genlib_map:get(residence, Req)),
        Realm = genlib_map:get(realm, Req),
        {ok, PaymentInstObjects} = capi_domain:get_payment_institutions(WoodyContext),
        Resp =
            lists:filtermap(
                fun(P) ->
                    case check_payment_institution(Realm, Residence, P) of
                        true ->
                            {true, decode_payment_institution_obj(P)};
                        false ->
                            false
                    end
                end,
                PaymentInstObjects
            ),
        {ok, {200, [], Resp}}
    catch
        throw:{encode_residence, invalid_residence} ->
            {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Invalid residence">>)}}
    end;

process_request('GetPaymentInstitutionByRef', Req, #{woody_context := WoodyContext}, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case capi_domain:get({payment_institution, ?payment_institution_ref(PaymentInstitutionID)}, WoodyContext) of
        {ok, PaymentInstitution} ->
            {ok, {200, [], decode_payment_institution_obj(PaymentInstitution)}};
        {error, not_found} ->
            {404, [], capi_handler_utils:general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPaymentTerms', Req, Context, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, #payproc_Varset{}, Context) of
        {ok, #domain_TermSet{payments = PaymentTerms}} ->
            {ok, {200, [], decode_payment_terms(PaymentTerms)}};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], capi_handler_utils:general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutMethods', Req, Context, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, prepare_varset(Req), Context) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_methods = PayoutMethods}}} ->
            {ok, {200, [], decode_payout_methods_selector(PayoutMethods)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, [], capi_handler_utils:general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], capi_handler_utils:general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutSchedules', Req, Context, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, prepare_varset(Req), Context) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_schedules = Schedules}}} ->
            {ok, {200, [], decode_business_schedules_selector(Schedules)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, [], capi_handler_utils:general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], capi_handler_utils:general_error(<<"Payment institution not found">>)}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

check_payment_institution(Realm, Residence, PaymentInstitution) ->
    check_payment_institution_realm(Realm, PaymentInstitution) andalso
        check_payment_institution_residence(Residence, PaymentInstitution).

check_payment_institution_realm(undefined, _) ->
    true;
check_payment_institution_realm(Realm1, #domain_PaymentInstitutionObject{
    data = #domain_PaymentInstitution{realm = Realm2}
}) ->
    Realm1 =:= Realm2.

check_payment_institution_residence(undefined, _) ->
    true;
check_payment_institution_residence(Residence, #domain_PaymentInstitutionObject{
    data = #domain_PaymentInstitution{residences = Residences}
}) ->
    ordsets:is_element(Residence, Residences).

compute_payment_institution_terms(PaymentInstitutionID, VS, Context) ->
    Call = {party_management, 'ComputePaymentInstitutionTerms', [?payment_institution_ref(PaymentInstitutionID), VS]},
    capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context).

prepare_varset(Req) ->
    #payproc_Varset{
        currency      = encode_optional_currency     (genlib_map:get(currency    , Req)),
        payout_method = encode_optional_payout_method(genlib_map:get(payoutMethod, Req))
    }.

%

encode_optional_payout_method('BankAccount') ->
    #domain_PayoutMethodRef{id = russian_bank_account};
encode_optional_payout_method('InternationalBankAccount') ->
    #domain_PayoutMethodRef{id = international_bank_account};
encode_optional_payout_method('Wallet') ->
    #domain_PayoutMethodRef{id = wallet_info};
encode_optional_payout_method(undefined) ->
    undefined.

encode_optional_currency(undefined   ) -> undefined;
encode_optional_currency(SymbolicCode) -> capi_handler_encoder:encode_currency(SymbolicCode).

%

decode_payment_institution_obj(#domain_PaymentInstitutionObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"id">> => Ref#domain_PaymentInstitutionRef.id,
        <<"name">> => Data#domain_PaymentInstitution.name,
        <<"description">> => Data#domain_PaymentInstitution.description,
        <<"realm">> => genlib:to_binary(Data#domain_PaymentInstitution.realm),
        <<"residences">> =>
            [
                capi_handler_decoder_party:decode_residence(R) ||
                R <- ordsets:to_list(Data#domain_PaymentInstitution.residences)
            ]
    }).

decode_payment_terms(#domain_PaymentsServiceTerms{currencies = Currencies, categories = Categories}) ->
    genlib_map:compact(#{
        <<"currencies">> => decode_payment_terms(fun capi_handler_decoder_utils:decode_currency    /1, Currencies),
        <<"categories">> => decode_payment_terms(fun capi_handler_decoder_utils:decode_category_ref/1, Categories)
    });
decode_payment_terms(undefined) ->
    #{}.

decode_payment_terms(Fun, {value, Val}) when is_list(Val) ->
    [Fun(V) || V <- Val];
decode_payment_terms(_, _) ->
    undefined.

decode_payout_method(#domain_PayoutMethodRef{id = russian_bank_account}) ->
    <<"BankAccount">>;
decode_payout_method(#domain_PayoutMethodRef{id = international_bank_account}) ->
    <<"InternationalBankAccount">>;
decode_payout_method(#domain_PayoutMethodRef{id = wallet_info}) ->
    <<"Wallet">>.

decode_payout_methods_selector({value, Val}) when is_list(Val) ->
    lists:map(fun decode_payout_method/1, Val);
decode_payout_methods_selector(_) ->
    [].

decode_business_schedules_selector({value, Val}) when is_list(Val) ->
    lists:map(fun capi_handler_decoder_utils:decode_business_schedule_ref/1, Val);
decode_business_schedules_selector(_) ->
    [].
