-module(capi_handler_payouts).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_payout_processing_thrift.hrl").

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
    case capi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            {ok, {200, [], [decode_payout_tool(P) || P <- PayoutTools]}};
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Contract not found">>)}}
    end;

process_request('GetPayoutToolByID', Req, Context, _) ->
    case capi_handler_utils:get_contract_by_id(maps:get('contractID', Req), Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            PayoutToolID = maps:get('payoutToolID', Req),
            case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                #domain_PayoutTool{} = P ->
                    {ok, {200, [], decode_payout_tool(P)}};
                false ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"PayoutTool not found">>)}}
            end;
        {exception, #payproc_ContractNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Contract not found">>)}}
    end;

process_request('GetPayout', Req, Context, _) ->
    PayoutID = maps:get(payoutID, Req),
    case capi_handler_utils:service_call({payouts, 'Get', [PayoutID]}, Context) of
        {ok, Payout} ->
            case check_party_in_payout_proc_payout(capi_handler_utils:get_party_id(Context), Payout) of
                true ->
                    {ok, {200, [], decode_payout_proc_payout(Payout)}};
                false ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"Payout not found">>)}}
            end;
        {exception, #'payout_processing_PayoutNotFound'{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Payout not found">>)}}
    end;

process_request('CreatePayout', Req, Context, _) ->
    CreateRequest = encode_payout_proc_payout_params(
        capi_handler_utils:get_party_id(Context),
        maps:get('PayoutParams', Req)
    ),
    case capi_handler_utils:service_call({payouts, 'CreatePayout', [CreateRequest]}, Context) of
        {ok, Payout} ->
            {ok, {201, [], decode_payout_proc_payout(Payout)}};
        {exception, Exception} ->
            case Exception of
                #'payout_processing_InvalidPayoutTool'{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidPayoutTool, <<"Invalid payout tool">>)}};
                #'payout_processing_InsufficientFunds'{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidCash, <<"Invalid amount or currency">>)}};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}}
            end
    end;

process_request('GetScheduleByRef', Req, Context, _) ->
    case get_schedule_by_id(genlib:to_int(maps:get(scheduleID, Req)), Context) of
        {ok, Schedule} ->
            {ok, {200, [], decode_business_schedule(Schedule)}};
        {error, not_found} ->
            {404, [], capi_handler_utils:general_error(<<"Schedule not found">>)}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

check_party_in_payout_proc_payout(PartyID, #payout_processing_Payout{party_id = PartyID}) ->
    true;
check_party_in_payout_proc_payout(_PartyID, _) ->
    false.

get_schedule_by_id(ScheduleID, #{woody_context := WoodyContext}) ->
    Ref = {business_schedule, #domain_BusinessScheduleRef{id = ScheduleID}},
    capi_domain:get(Ref, WoodyContext).

%%

encode_msgpack_value(_Key, Value) ->
    capi_msgpack:wrap(Value).

encode_payout_proc_metadata(undefined) ->
    undefined;
encode_payout_proc_metadata(Data) ->
    maps:map(fun encode_msgpack_value/2, Data).

encode_payout_proc_payout_params(PartyID, PayoutParams) ->
    #'payout_processing_PayoutParams'{
        payout_id = maps:get(<<"id">>, PayoutParams),
        shop = #'payout_processing_ShopParams'{
            party_id = PartyID,
            shop_id = maps:get(<<"shopID">>, PayoutParams)
        },
        payout_tool_id = maps:get(<<"payoutToolID">>, PayoutParams),
        amount = capi_handler:encode_cash(maps:get(<<"amount">>, PayoutParams), maps:get(<<"currency">>, PayoutParams)),
        metadata = encode_payout_proc_metadata(maps:get(<<"metadata">>, PayoutParams, undefined))
    }.

%%

decode_payout_tool(#domain_PayoutTool{id = ID, currency = Currency, payout_tool_info = Info}) ->
    maps:merge(
        #{<<"id">> => ID},
        capi_handler:decode_payout_tool_params(Currency, Info)
    ).

decode_payout_proc_payout(Payout) ->
    capi_handler_utils:merge_and_compact(#{
        <<"id"               >> => Payout#payout_processing_Payout.id,
        <<"shopID"           >> => Payout#payout_processing_Payout.shop_id,
        <<"createdAt"        >> => Payout#payout_processing_Payout.created_at,
        <<"amount"           >> => Payout#payout_processing_Payout.amount,
        <<"fee"              >> => Payout#payout_processing_Payout.fee,
        <<"currency"         >> => capi_handler:decode_currency(Payout#payout_processing_Payout.currency),
        <<"payoutToolDetails">> => decode_payout_proc_payout_tool_details(Payout#payout_processing_Payout.type),
        <<"payoutSummary"    >> => decode_payout_proc_payout_summary(Payout#payout_processing_Payout.summary),
        <<"metadata"         >> => decode_payout_proc_metadata(Payout#payout_processing_Payout.metadata)
    }, decode_payout_proc_payout_status(Payout#payout_processing_Payout.status)).

decode_payout_proc_payout_status({cancelled, #payout_processing_PayoutCancelled{details = Details}}) ->
    #{
        <<"status"             >> => <<"cancelled">>,
        <<"cancellationDetails">> => genlib:to_binary(Details)
    };
decode_payout_proc_payout_status({Status, _}) ->
    #{
        <<"status">> => genlib:to_binary(Status)
    }.

decode_payout_proc_payout_tool_details(PayoutType) ->
    capi_handler:decode_payout_tool_details(payout_proc_to_domain(PayoutType)).

payout_proc_to_domain({bank_account, {russian_payout_account, PayoutAccount}}) ->
    #payout_processing_RussianPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {russian_bank_account, BankAccount};
payout_proc_to_domain({bank_account, {international_payout_account, PayoutAccount}}) ->
    #payout_processing_InternationalPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {international_bank_account, BankAccount};
payout_proc_to_domain({wallet, #payout_processing_Wallet{wallet_id = WalletID}}) ->
    {wallet_info, #domain_WalletInfo{wallet_id = WalletID}}.

decode_payout_proc_payout_summary(PayoutSummary) when is_list(PayoutSummary) ->
    [decode_payout_proc_payout_summary_item(PayoutSummaryItem) || PayoutSummaryItem <- PayoutSummary];
decode_payout_proc_payout_summary(undefined) ->
    undefined.

decode_payout_proc_payout_summary_item(PayoutSummary) ->
    genlib_map:compact(#{
        <<"amount"  >> => PayoutSummary#payout_processing_PayoutSummaryItem.amount,
        <<"fee"     >> => PayoutSummary#payout_processing_PayoutSummaryItem.fee,
        <<"currency">> => PayoutSummary#payout_processing_PayoutSummaryItem.currency_symbolic_code,
        <<"count"   >> => PayoutSummary#payout_processing_PayoutSummaryItem.count,
        <<"fromTime">> => PayoutSummary#payout_processing_PayoutSummaryItem.from_time,
        <<"toTime"  >> => PayoutSummary#payout_processing_PayoutSummaryItem.to_time,
        <<"type"    >> => genlib:to_binary(PayoutSummary#payout_processing_PayoutSummaryItem.operation_type)
    }).

decode_payout_proc_metadata(undefined) ->
    undefined;
decode_payout_proc_metadata(Data) ->
    maps:map(fun decode_msgpack_value/2, Data).

decode_msgpack_value(_Key, Value) ->
    capi_msgpack:unwrap(Value).

decode_business_schedule(#domain_BusinessScheduleObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"scheduleID" >> => Ref#domain_BusinessScheduleRef.id,
        <<"name"       >> => Data#domain_BusinessSchedule.name,
        <<"description">> => Data#domain_BusinessSchedule.description
    }).
