-module(capi_handler_contracts).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetContracts', _Req, Context, _) ->
    Party = capi_utils:unwrap(capi_handler_utils:get_my_party(Context)),
    {ok, {200, [], decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};

process_request('GetContractByID', Req, Context, _) ->
    ContractID = maps:get('contractID', Req),
    Party = capi_utils:unwrap(capi_handler_utils:get_my_party(Context)),
    case genlib_map:get(ContractID, Party#domain_Party.contracts) of
        undefined ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Contract not found">>)}};
        Contract ->
            {ok, {200, [], decode_contract(Contract, Party#domain_Party.contractors)}}
    end;

process_request('GetContractAdjustments', Req, Context, _) ->
    case capi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, [], Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Contract not found">>)}}
    end;

process_request('GetContractAdjustmentByID', Req, Context, _) ->
    case capi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            AdjustmentID = maps:get('adjustmentID', Req),
            case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
                #domain_ContractAdjustment{} = A ->
                    {ok, {200, [], decode_contract_adjustment(A)}};
                false ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Adjustment not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Contract not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

decode_contracts_map(Contracts, Contractors) ->
    capi_handler_decoder_utils:decode_map(Contracts, fun(C) -> decode_contract(C, Contractors) end).

decode_contract(Contract, Contractors) ->
    capi_handler_utils:merge_and_compact(#{
        <<"id"                  >> => Contract#domain_Contract.id,
        <<"createdAt"           >> => Contract#domain_Contract.created_at,
        <<"contractor"          >> => capi_handler_decoder_party:decode_contractor(
            get_contractor(Contract, Contractors)
        ),
        <<"paymentInstitutionID">> =>
            capi_handler_decoder_party:decode_payment_institution_ref(Contract#domain_Contract.payment_institution),
        <<"validSince"          >> => Contract#domain_Contract.valid_since,
        <<"validUntil"          >> => Contract#domain_Contract.valid_until,
        <<"legalAgreement"      >> => capi_handler_decoder_utils:decode_optional(
            Contract#domain_Contract.legal_agreement,
            fun capi_handler_decoder_party:decode_legal_agreement/1
        ),
        <<"reportingPreferences">> => capi_handler_decoder_utils:decode_optional(
            Contract#domain_Contract.report_preferences,
            fun capi_handler_decoder_party:decode_reporting_preferences/1
        )
    }, decode_contract_status(Contract#domain_Contract.status)).

decode_contract_status({active, _}) ->
    #{
        <<"status">> => <<"active">>
    };

decode_contract_status({terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}) ->
    #{
        <<"status">> => <<"terminated">>,
        <<"terminatedAt">> => TerminatedAt
    }.

get_contractor(#domain_Contract{contractor = Contractor}, _) when Contractor =/= undefined ->
    Contractor;
get_contractor(#domain_Contract{contractor_id = ContractorID}, Contractors) ->
    #domain_PartyContractor{
        contractor = Contractor
    } = maps:get(ContractorID, Contractors),
    Contractor.

decode_contract_adjustment(ContractAdjustment) ->
    genlib_map:compact(#{
        <<"id"        >> => ContractAdjustment#domain_ContractAdjustment.id,
        <<"createdAt" >> => ContractAdjustment#domain_ContractAdjustment.created_at,
        <<"validSince">> => ContractAdjustment#domain_ContractAdjustment.valid_since,
        <<"validUntil">> => ContractAdjustment#domain_ContractAdjustment.valid_until
    }).
