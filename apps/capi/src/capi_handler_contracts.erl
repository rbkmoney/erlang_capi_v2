-module(capi_handler_contracts).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare_request/3]).
-export([process_request/3]).
-export([authorize_request/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare_request(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare_request(OperationID, _Req, _Context) when
    OperationID =:= 'GetContracts' orelse
        OperationID =:= 'GetContractByID' orelse
        OperationID =:= 'GetContractAdjustments' orelse
        OperationID =:= 'GetContractAdjustmentByID' orelse
        OperationID =:= 'GetContractsForParty' orelse
        OperationID =:= 'GetContractByIDForParty' orelse
        OperationID =:= 'GetContractAdjustmentsForParty' orelse
        OperationID =:= 'GetContractAdjustmentByIDForParty'
->
    {ok, #{}};
prepare_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec authorize_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
authorize_request(OperationID, Context, ReqState) when
    OperationID =:= 'GetContracts' orelse
        OperationID =:= 'GetContractByID' orelse
        OperationID =:= 'GetContractAdjustments' orelse
        OperationID =:= 'GetContractAdjustmentByID' orelse
        OperationID =:= 'GetContractsForParty' orelse
        OperationID =:= 'GetContractByIDForParty' orelse
        OperationID =:= 'GetContractAdjustmentsForParty' orelse
        OperationID =:= 'GetContractAdjustmentByIDForParty'
->
    Resolution = capi_auth:authorize_operation(OperationID, [], Context, ReqState),
    {ok, ReqState#{resolution => Resolution}};
authorize_request(_OperationID, _Context, _ReqState) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetContracts', Context, _ReqState) ->
    Party = capi_utils:unwrap(capi_handler_utils:get_party(Context)),
    {ok, {200, #{}, decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};
process_request('GetContractByID', Context, #{data := Req}) ->
    ContractID = maps:get('contractID', Req),
    Party = capi_utils:unwrap(capi_handler_utils:get_party(Context)),
    case genlib_map:get(ContractID, Party#domain_Party.contracts) of
        undefined ->
            {ok, general_error(404, <<"Contract not found">>)};
        Contract ->
            {ok, {200, #{}, decode_contract(Contract, Party#domain_Party.contractors)}}
    end;
process_request('GetContractAdjustments', Context, #{data := Req}) ->
    case capi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, #{}, Resp}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, general_error(404, <<"Contract not found">>)}
    end;
process_request('GetContractAdjustmentByID', Context, #{data := Req}) ->
    case capi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            AdjustmentID = maps:get('adjustmentID', Req),
            case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
                #domain_ContractAdjustment{} = A ->
                    {ok, {200, #{}, decode_contract_adjustment(A)}};
                false ->
                    {ok, general_error(404, <<"Adjustment not found">>)}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, general_error(404, <<"Contract not found">>)}
    end;
process_request('GetContractsForParty', Context, #{data := Req}) ->
    PartyID = maps:get('partyID', Req),
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:get_party(PartyID, Context) of
        {ok, Party} ->
            {ok, {200, #{}, decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_PartyNotFound{}} ->
            {ok, general_error(404, <<"Party not found">>)}
    end;
process_request('GetContractByIDForParty', Context, #{data := Req}) ->
    ContractID = maps:get('contractID', Req),
    PartyID = maps:get('partyID', Req),
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:get_party(PartyID, Context) of
        {ok, Party} ->
            case genlib_map:get(ContractID, Party#domain_Party.contracts) of
                undefined ->
                    {ok, general_error(404, <<"Contract not found">>)};
                Contract ->
                    {ok, {200, #{}, decode_contract(Contract, Party#domain_Party.contractors)}}
            end;
        {exception, #payproc_InvalidUser{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_PartyNotFound{}} ->
            {ok, general_error(404, <<"Party not found">>)}
    end;
process_request('GetContractAdjustmentsForParty', Context, #{data := Req}) ->
    PartyID = maps:get('partyID', Req),
    ContractID = maps:get('contractID', Req),
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:get_contract_by_id(PartyID, ContractID, Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            Resp = [decode_contract_adjustment(A) || A <- Adjustments],
            {ok, {200, #{}, Resp}};
        {exception, #payproc_InvalidUser{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_PartyNotFound{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, general_error(404, <<"Contract not found">>)}
    end;
process_request('GetContractAdjustmentByIDForParty', Context, #{data := Req}) ->
    PartyID = maps:get('partyID', Req),
    ContractID = maps:get('contractID', Req),
    % TODO
    % Here we're relying on hellgate ownership check, thus no explicit authorization.
    % Hovewer we're going to drop hellgate authz eventually, then we'll need to make sure that operation
    % remains authorized.
    case capi_handler_utils:get_contract_by_id(PartyID, ContractID, Context) of
        {ok, #domain_Contract{adjustments = Adjustments}} ->
            AdjustmentID = maps:get('adjustmentID', Req),
            case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
                #domain_ContractAdjustment{} = A ->
                    {ok, {200, #{}, decode_contract_adjustment(A)}};
                false ->
                    {ok, general_error(404, <<"Adjustment not found">>)}
            end;
        {exception, #payproc_InvalidUser{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_PartyNotFound{}} ->
            {ok, general_error(404, <<"Party not found">>)};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, general_error(404, <<"Contract not found">>)}
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_contracts_map(Contracts, Contractors) ->
    capi_handler_decoder_utils:decode_map(Contracts, fun(C) -> decode_contract(C, Contractors) end).

decode_contract(Contract, Contractors) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Contract#domain_Contract.id,
            <<"createdAt">> => Contract#domain_Contract.created_at,
            <<"contractor">> => capi_handler_decoder_party:decode_contractor(
                get_contractor(Contract, Contractors)
            ),
            <<"paymentInstitutionID">> =>
                capi_handler_decoder_party:decode_payment_institution_ref(Contract#domain_Contract.payment_institution),
            <<"validSince">> => Contract#domain_Contract.valid_since,
            <<"validUntil">> => Contract#domain_Contract.valid_until,
            <<"legalAgreement">> => capi_handler_decoder_utils:decode_optional(
                Contract#domain_Contract.legal_agreement,
                fun capi_handler_decoder_party:decode_legal_agreement/1
            ),
            <<"reportingPreferences">> => capi_handler_decoder_utils:decode_optional(
                Contract#domain_Contract.report_preferences,
                fun capi_handler_decoder_party:decode_reporting_preferences/1
            )
        },
        decode_contract_status(Contract#domain_Contract.status)
    ).

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
        <<"id">> => ContractAdjustment#domain_ContractAdjustment.id,
        <<"createdAt">> => ContractAdjustment#domain_ContractAdjustment.created_at,
        <<"validSince">> => ContractAdjustment#domain_ContractAdjustment.valid_since,
        <<"validUntil">> => ContractAdjustment#domain_ContractAdjustment.valid_until
    }).
