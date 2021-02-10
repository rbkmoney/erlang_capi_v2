-module(capi_handler_parties).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare_request/3]).
-export([process_request/3]).
-export([authorize_request/3]).

-type processing_context() :: capi_handler:processing_context().

-spec prepare_request(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare_request(OperationID, _Req, _Context) when
    OperationID =:= 'GetMyParty' orelse
        OperationID =:= 'ActivateMyParty' orelse
        OperationID =:= 'SuspendMyParty'
->
    {ok, #{}};
prepare_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec authorize_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
authorize_request(OperationID, Context, ReqState) when
    OperationID =:= 'GetMyParty' orelse
        OperationID =:= 'ActivateMyParty' orelse
        OperationID =:= 'SuspendMyParty'
->
    Resolution = capi_auth:authorize_operation(OperationID, [], Context, ReqState),
    {ok, ReqState#{resolution => Resolution}};
authorize_request(_OperationID, _Context, _ReqState) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetMyParty', Context, _ReqState) ->
    Party = capi_utils:unwrap(get_party(Context)),
    {ok, {200, #{}, capi_handler_decoder_party:decode_party(Party)}};
process_request('ActivateMyParty', Context, _ReqState) ->
    Call = {party_management, 'Activate', {}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, #{}, undefined}}
    end;
process_request('SuspendMyParty', Context, _ReqState) ->
    Call = {party_management, 'Suspend', {}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
            {ok, {204, #{}, undefined}}
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

-spec get_party(processing_context()) -> woody:result().
get_party(Context) ->
    GetCall = {party_management, 'Get', {}},
    Flags = [user_info, party_id],
    case capi_handler_utils:service_call_with(Flags, GetCall, Context) of
        {exception, #payproc_PartyNotFound{}} ->
            _ = logger:info("Attempting to create a missing party"),
            PartyParams = #payproc_PartyParams{
                contact_info = #domain_PartyContactInfo{
                    email = uac_authorizer_jwt:get_claim(
                        <<"email">>,
                        capi_handler_utils:get_auth_context(Context)
                    )
                }
            },
            CreateCall = {party_management, 'Create', {PartyParams}},
            case capi_handler_utils:service_call_with(Flags, CreateCall, Context) of
                {ok, _} ->
                    capi_handler_utils:service_call_with(Flags, GetCall, Context);
                {exception, #payproc_PartyExists{}} ->
                    capi_handler_utils:service_call_with(Flags, GetCall, Context);
                Error ->
                    Error
            end;
        Result ->
            Result
    end.
