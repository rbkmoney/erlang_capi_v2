-module(capi_handler_parties).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-type processing_context() :: capi_handler:processing_context().

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('GetMyParty' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case get_or_create_party(PartyID, Context) of
            {ok, Party} ->
                DecodedParty = capi_handler_decoder_party:decode_party(Party),
                {ok, {200, #{}, DecodedParty}};
            {error, #payproc_InvalidUser{}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('ActivateMyParty' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case capi_party:activate_party(PartyID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_InvalidUser{}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Party not found">>)};
            {error, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Invalid party status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('SuspendMyParty' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case capi_party:suspend_party(PartyID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_InvalidUser{}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Party not found">>)};
            {error, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Invalid party status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetPartyByID' = OperationID, Req, Context) ->
    PartyID = maps:get(partyID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                DecodedParty = capi_handler_decoder_party:decode_party(Party),
                {ok, {200, #{}, DecodedParty}};
            {error, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('ActivatePartyByID' = OperationID, Req, Context) ->
    PartyID = maps:get(partyID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case capi_party:activate_party(PartyID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_InvalidPartyStatus{status = {suspension, {active, _}}}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Invalid party status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('SuspendPartyByID' = OperationID, Req, Context) ->
    PartyID = maps:get(partyID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case capi_party:suspend_party(PartyID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_InvalidPartyStatus{status = {suspension, {suspended, _}}}} ->
                {ok, logic_error(<<"invalidRequest">>, <<"Invalid party status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

-spec get_or_create_party(binary(), processing_context()) -> woody:result().
get_or_create_party(PartyID, Context) ->
    case capi_party:get_party(PartyID, Context) of
        {error, #payproc_PartyNotFound{}} ->
            _ = logger:info("Attempting to create a missing party"),
            create_party(PartyID, Context);
        Reply ->
            Reply
    end.

-spec create_party(binary(), processing_context()) -> woody:result().
create_party(PartyID, Context) ->
    PartyParams = #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = uac_authorizer_jwt:get_claim(
                <<"email">>,
                capi_handler_utils:get_auth_context(Context)
            )
        }
    },
    case capi_party:create_party(PartyID, PartyParams, Context) of
        ok ->
            capi_party:get_party(PartyID, Context);
        {error, #payproc_PartyExists{}} ->
            capi_party:get_party(PartyID, Context);
        Error ->
            Error
    end.
