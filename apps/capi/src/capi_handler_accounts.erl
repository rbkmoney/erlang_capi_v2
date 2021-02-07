-module(capi_handler_accounts).

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
) ->
   {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec authorize_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) ->
    {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
authorize_request(OperationID, Context, ReqState) ->
    Resolution = capi_auth:authorize_operation(OperationID, [], Context, ReqState),
    {ok, ReqState#{resolution => Resolution}}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetAccountByID', Context, #{data := Req}) ->
    CallArgs = {genlib:to_int(maps:get('accountID', Req))},
    Call = {party_management, 'GetAccountState', CallArgs},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, S} ->
            {ok, {200, #{}, decode_account_state(S)}};
        {exception, #payproc_AccountNotFound{}} ->
            {ok, general_error(404, <<"Account not found">>)}
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_account_state(AccountState) ->
    #{
        <<"id">> => AccountState#payproc_AccountState.account_id,
        <<"ownAmount">> => AccountState#payproc_AccountState.own_amount,
        <<"availableAmount">> => AccountState#payproc_AccountState.available_amount,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(AccountState#payproc_AccountState.currency)
    }.
