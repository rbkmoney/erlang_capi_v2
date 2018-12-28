-module(capi_handler_payouts).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetPayoutTools', Req, Context, _) ->
    case get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            {ok, {200, [], [decode_payout_tool(P) || P <- PayoutTools]}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetPayoutToolByID', Req, Context, _) ->
    case get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            PayoutToolID = maps:get('payoutToolID', Req),
            case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                #domain_PayoutTool{} = P ->
                    {ok, {200, [], decode_payout_tool(P)}};
                false ->
                    {ok, {404, [], general_error(<<"PayoutTool not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], general_error(<<"Contract not found">>)}}
    end;

process_request('GetPayout', Req, Context, _) ->
    PayoutID = maps:get(payoutID, Req),
    case service_call({payouts, 'Get', [PayoutID]}, Context) of
        {ok, Payout} ->
            {ok, {200, [], decode_payout_proc_payout(Payout)}};
        {exception, #'payout_processing_PayoutNotFound'{}} ->
            {ok, {404, [], general_error(<<"Payout not found">>)}}
    end;

process_request('CreatePayout', Req, Context, _) ->
    CreateRequest = encode_payout_proc_payout_params(get_party_id(Context), maps:get('PayoutParams', Req)),
    case service_call({payouts, 'CreatePayout', [CreateRequest]}, Context) of
        {ok, Payout} ->
            {ok, {201, [], decode_payout_proc_payout(Payout)}};
        {exception, Exception} ->
            case Exception of
                #'payout_processing_InvalidPayoutTool'{} ->
                    {ok, {400, [], logic_error(invalidPayoutTool, <<"Invalid payout tool">>)}};
                #'payout_processing_InsufficientFunds'{} ->
                    {ok, {400, [], logic_error(invalidCash, <<"Invalid amount or currency">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
            end
    end;

process_request('GetScheduleByRef', Req, Context, _) ->
    case get_schedule_by_id(genlib:to_int(maps:get(scheduleID, Req)), Context) of
        {ok, Schedule} ->
            {ok, {200, [], decode_business_schedule(Schedule)}};
        {error, not_found} ->
            {404, [], general_error(<<"Schedule not found">>)}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
