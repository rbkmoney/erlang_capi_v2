-module(capi_handler_parties).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-type processing_context() :: capi_handler:processing_context().

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare(OperationID, Req, Context) when
    OperationID =:= 'GetMyParty' orelse
        OperationID =:= 'ActivateMyParty' orelse
        OperationID =:= 'SuspendMyParty'
->
    Authorize = fun() -> {ok, capi_auth:authorize_operation(OperationID, [], Context, Req)} end,
    Process = fun() -> process_request(OperationID, Context, Req) end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetMyParty', Context, _Req) ->
    Party = capi_utils:unwrap(get_party(Context)),
    {ok, {200, #{}, capi_handler_decoder_party:decode_party(Party)}};
process_request('ActivateMyParty', Context, _Req) ->
    Call = {party_management, 'Activate', {}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, #{}, undefined}}
    end;
process_request('SuspendMyParty', Context, _Req) ->
    Call = {party_management, 'Suspend', {}},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
            {ok, {204, #{}, undefined}}
    end.

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
