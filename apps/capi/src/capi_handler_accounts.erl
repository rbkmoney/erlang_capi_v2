-module(capi_handler_accounts).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([process_request/3]).
-export([get_authorize_prototypes/3]).

-import(capi_handler_utils, [general_error/2]).

-spec get_authorize_prototypes(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) ->
    {ok,
        {capi_bouncer_context:prototype_operation(), capi_bouncer_context:prototypes()}}
        | capi_handler:response()
    | {error, noimpl}.
get_authorize_prototypes(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok | error, capi_handler:response() | noimpl}.
process_request('GetAccountByID', Req, Context) ->
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
