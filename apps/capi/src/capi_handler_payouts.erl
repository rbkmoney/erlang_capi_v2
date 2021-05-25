-module(capi_handler_payouts).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payout_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayout' ->
    PayoutID = maps:get(payoutID, Req),
    PartyID = capi_handler_utils:get_party_id(Context),
    OperationContext = #{
        id => OperationID,
        party => PartyID,
        payout => PayoutID
    },
    Payout =
        case capi_handler_call:service_call({payouts, 'Get', {PayoutID}}, Context) of
            {ok, Result} ->
                case check_party_in_payout(PartyID, Result) of
                    true ->
                        Result;
                    false ->
                        undefined
                end;
            {exception, #'payout_processing_PayoutNotFound'{}} ->
                undefined
        end,
    Prototypes = [
        {operation, OperationContext},
        {payouts, #{payout => Payout}}
    ],
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Payout, general_error(404, <<"Payout not found">>)),
        {ok, {200, #{}, decode_payout(Payout)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'CreatePayout' ->
    PayoutParams = maps:get('PayoutParams', Req),
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(<<"partyID">>, PayoutParams, UserID),
    OperationContext = #{
        id => OperationID,
        party => PartyID,
        shop => maps:get(<<"shopID">>, PayoutParams)
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, [{operation, OperationContext}], Context, Req)}
    end,
    Process = fun() ->
        CreateRequest = encode_payout_params(PartyID, PayoutParams),
        case capi_handler_call:service_call({payouts, 'CreatePayout', {CreateRequest}}, Context) of
            {ok, Payout} ->
                {ok, {201, #{}, decode_payout(Payout)}};
            {exception, Exception} ->
                case Exception of
                    #'payout_processing_InvalidPayoutTool'{} ->
                        {ok, logic_error(invalidPayoutTool, <<"Invalid payout tool">>)};
                    #'payout_processing_InsufficientFunds'{} ->
                        {ok, logic_error(invalidCash, <<"Invalid amount or currency">>)};
                    #'InvalidRequest'{errors = Errors} ->
                        FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                        {ok, logic_error(invalidRequest, FormattedErrors)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayoutTools' ->
    OperationContext = #{
        id => OperationID,
        party => capi_handler_utils:get_party_id(Context)
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, [{operation, OperationContext}], Context, Req)}
    end,
    Process = fun() ->
        case capi_handler_call:get_contract_by_id(maps:get('contractID', Req), Context) of
            {ok, #domain_Contract{payout_tools = PayoutTools}} ->
                {ok, {200, #{}, [decode_payout_tool(P) || P <- PayoutTools]}};
            {exception, #payproc_ContractNotFound{}} ->
                {ok, general_error(404, <<"Contract not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayoutToolByID' ->
    OperationContext = #{
        id => OperationID,
        party => capi_handler_utils:get_party_id(Context)
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, [{operation, OperationContext}], Context, Req)}
    end,
    Process = fun() ->
        case capi_handler_call:get_contract_by_id(maps:get('contractID', Req), Context) of
            {ok, #domain_Contract{payout_tools = PayoutTools}} ->
                PayoutToolID = maps:get('payoutToolID', Req),
                case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                    #domain_PayoutTool{} = P ->
                        {ok, {200, #{}, decode_payout_tool(P)}};
                    false ->
                        {ok, general_error(404, <<"PayoutTool not found">>)}
                end;
            {exception, #payproc_ContractNotFound{}} ->
                {ok, general_error(404, <<"Contract not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayoutToolsForParty' ->
    PartyID = maps:get('partyID', Req),
    OperationContext = #{
        id => OperationID,
        party => PartyID
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, [{operation, OperationContext}], Context, Req)}
    end,
    Process = fun() ->
        ContractID = maps:get('contractID', Req),
        case capi_handler_call:get_contract_by_id(PartyID, ContractID, Context) of
            {ok, #domain_Contract{payout_tools = PayoutTools}} ->
                {ok, {200, #{}, [decode_payout_tool(P) || P <- PayoutTools]}};
            {exception, #payproc_ContractNotFound{}} ->
                {ok, general_error(404, <<"Contract not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayoutToolByIDForParty' ->
    PartyID = maps:get('partyID', Req),
    OperationContext = #{
        id => OperationID,
        party => PartyID
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, [{operation, OperationContext}], Context, Req)}
    end,
    Process = fun() ->
        ContractID = maps:get('contractID', Req),
        case capi_handler_call:get_contract_by_id(PartyID, ContractID, Context) of
            {ok, #domain_Contract{payout_tools = PayoutTools}} ->
                PayoutToolID = maps:get('payoutToolID', Req),
                case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                    #domain_PayoutTool{} = P ->
                        {ok, {200, #{}, decode_payout_tool(P)}};
                    false ->
                        {ok, general_error(404, <<"PayoutTool not found">>)}
                end;
            {exception, #payproc_ContractNotFound{}} ->
                {ok, general_error(404, <<"Contract not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetScheduleByRef' ->
    OperationContext = #{
        id => OperationID
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(OperationID, [{operation, OperationContext}], Context, Req)}
    end,
    Process = fun() ->
        case get_schedule_by_id(genlib:to_int(maps:get(scheduleID, Req)), Context) of
            {ok, Schedule} ->
                {ok, {200, #{}, decode_business_schedule(Schedule)}};
            {error, not_found} ->
                {ok, general_error(404, <<"Schedule not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

check_party_in_payout(PartyID, #payout_processing_Payout{party_id = PartyID}) ->
    true;
check_party_in_payout(_PartyID, _) ->
    false.

get_schedule_by_id(ScheduleID, #{woody_context := WoodyContext}) ->
    Ref = {business_schedule, #domain_BusinessScheduleRef{id = ScheduleID}},
    capi_domain:get(Ref, WoodyContext).

%%

encode_msgpack_value(_Key, Value) ->
    capi_msgpack:wrap(Value).

encode_metadata(undefined) ->
    undefined;
encode_metadata(Data) ->
    maps:map(fun encode_msgpack_value/2, Data).

encode_payout_params(PartyID, PayoutParams) ->
    #'payout_processing_PayoutParams'{
        payout_id = maps:get(<<"id">>, PayoutParams),
        shop = #'payout_processing_ShopParams'{
            party_id = PartyID,
            shop_id = maps:get(<<"shopID">>, PayoutParams)
        },
        payout_tool_id = maps:get(<<"payoutToolID">>, PayoutParams),
        amount = capi_handler_encoder:encode_cash(
            maps:get(<<"amount">>, PayoutParams),
            maps:get(<<"currency">>, PayoutParams)
        ),
        metadata = encode_metadata(maps:get(<<"metadata">>, PayoutParams, undefined))
    }.

%%

decode_payout_tool(#domain_PayoutTool{id = ID, currency = Currency, payout_tool_info = Info}) ->
    maps:merge(
        #{<<"id">> => ID},
        capi_handler_decoder_party:decode_payout_tool_params(Currency, Info)
    ).

decode_payout(Payout) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Payout#payout_processing_Payout.id,
            <<"shopID">> => Payout#payout_processing_Payout.shop_id,
            <<"createdAt">> => Payout#payout_processing_Payout.created_at,
            <<"amount">> => Payout#payout_processing_Payout.amount,
            <<"fee">> => Payout#payout_processing_Payout.fee,
            <<"currency">> => capi_handler_decoder_utils:decode_currency(Payout#payout_processing_Payout.currency),
            <<"payoutToolDetails">> => decode_payout_tool_details(Payout#payout_processing_Payout.type),
            <<"payoutSummary">> => decode_payout_summary(Payout#payout_processing_Payout.summary),
            <<"metadata">> => decode_metadata(Payout#payout_processing_Payout.metadata)
        },
        decode_payout_status(Payout#payout_processing_Payout.status)
    ).

decode_payout_status({cancelled, #payout_processing_PayoutCancelled{details = Details}}) ->
    #{
        <<"status">> => <<"cancelled">>,
        <<"cancellationDetails">> => genlib:to_binary(Details)
    };
decode_payout_status({Status, _}) ->
    #{
        <<"status">> => genlib:to_binary(Status)
    }.

decode_payout_tool_details({wallet, #payout_processing_Wallet{wallet_id = WalletID}}) ->
    #{
        <<"detailsType">> => <<"PayoutToolDetailsWalletInfo">>,
        <<"walletID">> => WalletID
    };
decode_payout_tool_details(PayoutType) ->
    capi_handler_decoder_party:decode_payout_tool_details(payout_proc_to_domain(PayoutType)).

payout_proc_to_domain({bank_account, {russian_payout_account, PayoutAccount}}) ->
    #payout_processing_RussianPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {russian_bank_account, BankAccount};
payout_proc_to_domain({bank_account, {international_payout_account, PayoutAccount}}) ->
    #payout_processing_InternationalPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    {international_bank_account, BankAccount}.

decode_payout_summary(PayoutSummary) when is_list(PayoutSummary) ->
    [decode_payout_summary_item(PayoutSummaryItem) || PayoutSummaryItem <- PayoutSummary];
decode_payout_summary(undefined) ->
    undefined.

decode_payout_summary_item(PayoutSummary) ->
    genlib_map:compact(#{
        <<"amount">> => PayoutSummary#payout_processing_PayoutSummaryItem.amount,
        <<"fee">> => PayoutSummary#payout_processing_PayoutSummaryItem.fee,
        <<"currency">> => PayoutSummary#payout_processing_PayoutSummaryItem.currency_symbolic_code,
        <<"count">> => PayoutSummary#payout_processing_PayoutSummaryItem.count,
        <<"fromTime">> => PayoutSummary#payout_processing_PayoutSummaryItem.from_time,
        <<"toTime">> => PayoutSummary#payout_processing_PayoutSummaryItem.to_time,
        <<"type">> => genlib:to_binary(PayoutSummary#payout_processing_PayoutSummaryItem.operation_type)
    }).

decode_metadata(undefined) ->
    undefined;
decode_metadata(Data) ->
    maps:map(fun decode_msgpack_value/2, Data).

decode_msgpack_value(_Key, Value) ->
    capi_msgpack:unwrap(Value).

decode_business_schedule(#domain_BusinessScheduleObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"scheduleID">> => Ref#domain_BusinessScheduleRef.id,
        <<"name">> => Data#domain_BusinessSchedule.name,
        <<"description">> => Data#domain_BusinessSchedule.description
    }).
