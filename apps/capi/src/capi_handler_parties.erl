-module(capi_handler_parties).

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

process_request('GetMyParty', _Req, Context, _) ->
    Party = capi_utils:unwrap(capi_handler_utils:get_my_party(Context)),
    {ok, {200, [], capi_handler:decode_party(Party)}};
process_request('ActivateMyParty', _Req, Context, _) ->
    Call = {party_management, 'Activate', []},
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, [], undefined}}
    end;
process_request('SuspendMyParty', _Req, Context, _) ->
    Call = {party_management, 'Suspend', []},
    case capi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, [], undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
            {ok, {204, [], undefined}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
