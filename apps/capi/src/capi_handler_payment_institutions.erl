-module(capi_handler_payment_institutions).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetPaymentInstitutions', Req, #{woody_context := WoodyContext}, _) ->
    try
        Residence = encode_residence(genlib_map:get(residence, Req)),
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
            {ok, {400, [], logic_error(invalidRequest, <<"Invalid residence">>)}}
    end;

process_request('GetPaymentInstitutionByRef', Req, #{woody_context := WoodyContext}, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case capi_domain:get({payment_institution, ?payment_institution_ref(PaymentInstitutionID)}, WoodyContext) of
        {ok, PaymentInstitution} ->
            {ok, {200, [], decode_payment_institution_obj(PaymentInstitution)}};
        {error, not_found} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPaymentTerms', Req, Context, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, #payproc_Varset{}, Context) of
        {ok, #domain_TermSet{payments = PaymentTerms}} ->
            {ok, {200, [], decode_payment_terms(PaymentTerms)}};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutMethods', Req, Context, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, prepare_varset(Req), Context) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_methods = PayoutMethods}}} ->
            {ok, {200, [], decode_payout_methods_selector(PayoutMethods)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, [], general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

process_request('GetPaymentInstitutionPayoutSchedules', Req, Context, _) ->
    PaymentInstitutionID = genlib:to_int(maps:get(paymentInstitutionID, Req)),
    case compute_payment_institution_terms(PaymentInstitutionID, prepare_varset(Req), Context) of
        {ok, #domain_TermSet{payouts = #domain_PayoutsServiceTerms{payout_schedules = Schedules}}} ->
            {ok, {200, [], decode_business_schedules_selector(Schedules)}};
        {ok, #domain_TermSet{payouts = undefined}} ->
            {404, [], general_error(<<"Automatic payouts not allowed">>)};
        {exception, #payproc_PaymentInstitutionNotFound{}} ->
            {404, [], general_error(<<"Payment institution not found">>)}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
