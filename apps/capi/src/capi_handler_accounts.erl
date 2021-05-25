-module(capi_handler_accounts).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID, Req, Context) when OperationID =:= 'GetAccountByID' ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        AccountID = genlib:to_int(maps:get('accountID', Req)),
        CallArgs = {PartyID, AccountID},
        Call = {party_management, 'GetAccountState', CallArgs},
        case capi_handler_call:service_call_with([user_info], Call, Context) of
            {ok, S} ->
                {ok, {200, #{}, decode_account_state(S)}};
            {exception, #payproc_AccountNotFound{}} ->
                {ok, general_error(404, <<"Account not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

decode_account_state(AccountState) ->
    #{
        <<"id">> => AccountState#payproc_AccountState.account_id,
        <<"ownAmount">> => AccountState#payproc_AccountState.own_amount,
        <<"availableAmount">> => AccountState#payproc_AccountState.available_amount,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(AccountState#payproc_AccountState.currency)
    }.
