-module(capi_handler_parties).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([process_request/3]).

-export([get_my_party/1]).

-type processing_context() :: capi_handler:processing_context().

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: processing_context()
) -> {ok | error, capi_handler:response() | noimpl}.
process_request('GetMyParty', _Req, Context) ->
    Party = capi_utils:unwrap(get_my_party_with_create(Context)),
    {ok, {200, #{}, capi_handler_decoder_party:decode_party(Party)}};
process_request('ActivateMyParty', _Req, Context) ->
    Call = {party_management, 'Activate', []},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
            {ok, {204, #{}, undefined}}
    end;
process_request('SuspendMyParty', _Req, Context) ->
    Call = {party_management, 'Suspend', []},
    case capi_handler_utils:service_call_with([user_info, party_id], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
            {ok, {204, #{}, undefined}}
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%% Common functions

-spec get_my_party(processing_context()) -> woody:result().
get_my_party(Context) ->
    GetCall = {party_management, 'Get', []},
    Flags = [user_info, party_id],
    capi_handler_utils:service_call_with(Flags, GetCall, Context).

-spec get_my_party_with_create(processing_context()) -> woody:result().
get_my_party_with_create(Context) ->
    GetCall = {party_management, 'Get', []},
    Flags = [user_info, party_id],
    case capi_handler_utils:service_call_with(Flags, GetCall, Context) of
        {exception, #payproc_PartyNotFound{}} ->
            _ = logger:info("Attempting to create a missing party"),
            PartyParams = #payproc_PartyParams{
                contact_info = #domain_PartyContactInfo{
                    email = uac_authorizer_jwt:get_claim(
                        <<"email">>,
                        capi_handler_utils:get_auth_context(Context))
                }
            },
            CreateCall = {party_management, 'Create', [PartyParams]},
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
