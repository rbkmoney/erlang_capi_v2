-module(capi_handler_accounts).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetAccountByID', Req, Context, _) ->
    Call = {party_management, 'GetAccountState', [genlib:to_int(maps:get('accountID', Req))]},
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, S} ->
            {ok, {200, [], decode_account_state(S)}};
        {exception, #payproc_AccountNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Account not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

decode_account_state(AccountState) ->
    #{
        <<"id"             >> => AccountState#payproc_AccountState.account_id,
        <<"ownAmount"      >> => AccountState#payproc_AccountState.own_amount,
        <<"availableAmount">> => AccountState#payproc_AccountState.available_amount,
        <<"currency"       >> => capi_handler:decode_currency(AccountState#payproc_AccountState.currency)
    }.
