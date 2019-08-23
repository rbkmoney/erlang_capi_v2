-module(capi_handler_accounts).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [general_error/2]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('GetAccountByID', Req, Context) ->
    try
        AccountId = genlib:to_int(maps:get('accountID', Req)),
        AccountState = get_account_state(AccountId, Context),
        AccountBalance = get_account_balance(AccountId, Context),
        {ok, {200, #{}, decode_account_state(AccountState, AccountBalance)}}
    catch
        throw:account_not_found  ->
            {ok, general_error(404, <<"Account not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_account_state(AccountState, AccountBalance) ->
    #{
        <<"id"             >> => AccountState#payproc_AccountState.account_id,
        <<"ownAmount"      >> => AccountBalance#payproc_AccountBalance.own_amount,
        <<"availableAmount">> => AccountBalance#payproc_AccountBalance.available_amount,
        <<"currency"       >> => capi_handler_decoder_utils:decode_currency(AccountState#payproc_AccountState.currency)
    }.

get_account_state(AccountId, Context) ->
    AccountStateCall = {party_management, 'GetAccountState', [AccountId]},
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], AccountStateCall, Context) of
        {ok, AccountState} ->
            AccountState;
        {exception, #payproc_AccountNotFound{}} ->
            throw(account_not_found)
    end.

get_account_balance(AccountId, Context) ->
    AccountStateCall = {party_management, 'GetAccountBalance', [AccountId]},
    case capi_handler_utils:service_call_with([user_info, party_id], AccountStateCall, Context) of
        {ok, AccountBalance} ->
            AccountBalance;
        {exception, #payproc_AccountNotFound{}} ->
            throw(account_not_found)
    end.
