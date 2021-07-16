-module(capi_handler_payouts).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("payout_manager_proto/include/payouts_payout_manager_thrift.hrl").

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
        case capi_handler_utils:service_call({payouts, 'GetPayout', {PayoutID}}, Context) of
            {ok, Result} ->
                case check_party_in_payout(PartyID, Result) of
                    true ->
                        Result;
                    false ->
                        undefined
                end;
            {exception, #payouts_NotFound{}} ->
                undefined
        end,
    ContractID = capi_utils:maybe(Payout, fun(_Payout) ->
        get_payout_contract_id(Payout, Context)
    end),
    Prototypes = [
        {operation, OperationContext},
        {payouts, #{
            payout => Payout,
            contract => ContractID
        }}
    ],
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Payout, general_error(404, <<"Payout not found">>)),
        {ok, PayoutTool} = get_payout_tool(Payout, Context),
        {ok, {200, #{}, decode_payout(Payout, PayoutTool)}}
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
        {ok, capi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun() ->
        CreateRequest = encode_payout_params(PartyID, PayoutParams),
        case capi_handler_utils:service_call({payouts, 'CreatePayout', {CreateRequest}}, Context) of
            {ok, Payout} ->
                {ok, PayoutTool} = get_payout_tool(Payout, Context),
                {ok, {201, #{}, decode_payout(Payout, PayoutTool)}};
            {exception, Exception} ->
                case Exception of
                    #payouts_InsufficientFunds{} ->
                        {ok, logic_error(invalidCash, <<"Invalid amount or currency">>)};
                    #payouts_InvalidRequest{errors = Errors} ->
                        FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                        {ok, logic_error(invalidRequest, FormattedErrors)};
                    #payouts_PayoutAlreadyExists{} ->
                        {ok, logic_error(invalidRequest, <<"Payout already exists">>)};
                    #payouts_NotFound{message = Message} ->
                        {ok, logic_error(invalidRequest, Message)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayoutTools' ->
    PartyID = capi_handler_utils:get_party_id(Context),
    OperationContext = #{
        id => OperationID,
        party => PartyID
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun() ->
        case capi_party:get_contract(PartyID, maps:get('contractID', Req), Context) of
            {ok, #domain_Contract{payout_tools = PayoutTools}} ->
                {ok, {200, #{}, [decode_payout_tool(P) || P <- PayoutTools]}};
            {error, #payproc_ContractNotFound{}} ->
                {ok, general_error(404, <<"Contract not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetPayoutToolByID' ->
    PartyID = capi_handler_utils:get_party_id(Context),
    OperationContext = #{
        id => OperationID,
        party => PartyID
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun() ->
        PayoutToolID = maps:get('payoutToolID', Req),
        ContractID = maps:get('contractID', Req),
        case get_payout_tool_by_id(PartyID, ContractID, PayoutToolID, Context) of
            {ok, PayoutTool} ->
                {ok, {200, #{}, decode_payout_tool(PayoutTool)}};
            {error, not_found} ->
                {ok, general_error(404, <<"PayoutTool not found">>)};
            {error, #payproc_ContractNotFound{}} ->
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
        {ok, capi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun() ->
        ContractID = maps:get('contractID', Req),
        case capi_party:get_contract(PartyID, ContractID, Context) of
            {ok, #domain_Contract{payout_tools = PayoutTools}} ->
                {ok, {200, #{}, [decode_payout_tool(P) || P <- PayoutTools]}};
            {error, #payproc_ContractNotFound{}} ->
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
        {ok, capi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun() ->
        PayoutToolID = maps:get('payoutToolID', Req),
        ContractID = maps:get('contractID', Req),
        case get_payout_tool_by_id(PartyID, ContractID, PayoutToolID, Context) of
            {ok, PayoutTool} ->
                {ok, {200, #{}, decode_payout_tool(PayoutTool)}};
            {error, not_found} ->
                {ok, general_error(404, <<"PayoutTool not found">>)};
            {error, #payproc_ContractNotFound{}} ->
                {ok, general_error(404, <<"Contract not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetScheduleByRef' ->
    OperationContext = #{
        id => OperationID
    },
    Authorize = fun() ->
        {ok, capi_auth:authorize_operation([{operation, OperationContext}], Context)}
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

check_party_in_payout(PartyID, #payouts_Payout{party_id = PartyID}) ->
    true;
check_party_in_payout(_PartyID, _) ->
    false.

get_schedule_by_id(ScheduleID, #{woody_context := WoodyContext}) ->
    Ref = {business_schedule, #domain_BusinessScheduleRef{id = ScheduleID}},
    capi_domain:get(Ref, WoodyContext).

get_payout_tool(Payout, Context) ->
    PayoutToolID = Payout#payouts_Payout.payout_tool_id,
    PartyID = Payout#payouts_Payout.party_id,
    ContractID = get_payout_contract_id(Payout, Context),
    get_payout_tool_by_id(PartyID, ContractID, PayoutToolID, Context).

get_payout_contract_id(Payout, Context) ->
    PartyID = Payout#payouts_Payout.party_id,
    ShopID = Payout#payouts_Payout.shop_id,
    {ok, Shop} = capi_party:get_shop(PartyID, ShopID, Context),
    Shop#domain_Shop.contract_id.

get_payout_tool_by_id(PartyID, ContractID, PayoutToolID, Context) ->
    case capi_party:get_contract(PartyID, ContractID, Context) of
        {ok, #domain_Contract{payout_tools = PayoutTools}} ->
            case lists:keyfind(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
                #domain_PayoutTool{} = PayoutTool ->
                    {ok, PayoutTool};
                false ->
                    {error, not_found}
            end;
        Error ->
            Error
    end.

%%

encode_payout_params(PartyID, PayoutParams) ->
    #payouts_PayoutParams{
        shop_params = #payouts_ShopParams{
            party_id = PartyID,
            shop_id = maps:get(<<"shopID">>, PayoutParams)
        },
        cash = encode_payout_cash(PayoutParams),
        payout_id = maps:get(<<"id">>, PayoutParams, undefined),
        payout_tool_id = maps:get(<<"payoutToolID">>, PayoutParams)
    }.

encode_payout_cash(#{<<"amount">> := Amount, <<"currency">> := Currency}) ->
    #payouts_Cash{
        amount = Amount,
        currency = encode_payout_currency(Currency)
    }.

encode_payout_currency(SymbolicCode) ->
    #payouts_CurrencyRef{symbolic_code = SymbolicCode}.

%%

decode_payout_tool(#domain_PayoutTool{id = ID, currency = Currency, payout_tool_info = Info}) ->
    #{
        <<"id">> => ID,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"details">> => capi_handler_decoder_party:decode_payout_tool_details(Info)
    }.

decode_payout(Payout, #domain_PayoutTool{payout_tool_info = Info}) ->
    Currency = decode_payout_currency(Payout#payouts_Payout.currency),
    PayoutToolDetails = capi_handler_decoder_party:decode_payout_tool_details(Info),
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Payout#payouts_Payout.payout_id,
            <<"createdAt">> => Payout#payouts_Payout.created_at,
            <<"shopID">> => Payout#payouts_Payout.shop_id,
            <<"payoutToolDetails">> => PayoutToolDetails,
            <<"amount">> => Payout#payouts_Payout.amount,
            <<"fee">> => Payout#payouts_Payout.fee,
            <<"currency">> => Currency
        },
        decode_payout_status(Payout#payouts_Payout.status)
    ).

decode_payout_status({cancelled, #payouts_PayoutCancelled{details = Details}}) ->
    #{
        <<"status">> => <<"cancelled">>,
        <<"cancellationDetails">> => genlib:to_binary(Details)
    };
decode_payout_status({Status, _}) ->
    #{
        <<"status">> => genlib:to_binary(Status)
    }.

decode_payout_currency(#payouts_CurrencyRef{symbolic_code = SymbolicCode}) ->
    capi_handler_decoder_utils:decode_currency(#domain_CurrencyRef{symbolic_code = SymbolicCode}).

decode_business_schedule(#domain_BusinessScheduleObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"scheduleID">> => Ref#domain_BusinessScheduleRef.id,
        <<"name">> => Data#domain_BusinessSchedule.name,
        <<"description">> => Data#domain_BusinessSchedule.description
    }).
