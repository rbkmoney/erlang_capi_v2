-module(capi_handler).

-behaviour(swag_server_logic_handler).

-type error_type() :: swag_server_logic_handler:error_type().

%% API callbacks
-export([authorize_api_key/4]).
-export([map_error/2]).
-export([handle_request/4]).
-export([respond/1]).
-export([respond_if_undefined/2]).
-export([respond_if_forbidden/2]).

%%

-type request_data() :: #{atom() | binary() => term()}.

-type operation_id() :: swag_server:operation_id().
-type request_context() :: swag_server:request_context().
-type response() :: swag_server:response().
-type processing_context() :: #{
    operation_id := operation_id(),
    swagger_context := swag_server:request_context(),
    woody_context := woody_context:ctx(),
    party_client := party_client:client(),
    party_client_context := party_client:context()
}.

-type throw(_T) :: no_return().

-type request_state() :: #{
    authorize := fun(() -> {ok, capi_auth:resolution()} | throw(response())),
    process := fun(() -> {ok, response()} | throw(response()))
}.

-type handler_opts() ::
    swag_server:handler_opts(#{
        party_client := party_client:client()
    }).

-export_type([operation_id/0]).
-export_type([request_data/0]).
-export_type([request_context/0]).
-export_type([processing_context/0]).
-export_type([request_state/0]).
-export_type([response/0]).
-export_type([handler_opts/0]).

%% Handler behaviour

-callback prepare(
    OperationID :: operation_id(),
    Req :: request_data(),
    Context :: processing_context()
) -> {ok, request_state()} | {error, noimpl}.

-import(capi_handler_utils, [logic_error/2, server_error/1]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-spec authorize_api_key(operation_id(), swag_server:api_key(), request_context(), handler_opts()) ->
    Result :: false | {true, capi_auth:preauth_context()}.
authorize_api_key(OperationID, ApiKey, _Context, _HandlerOpts) ->
    %% Since we require the request id field to create a woody context for our trip to token_keeper
    %% it seems it is no longer possible to perform any authorization in this method.
    %% To gain this ability back be would need to rewrite the swagger generator to perform its
    %% request validation checks before this stage.
    %% But since a decent chunk of authorization logic is already defined in the handler function
    %% it is probably easier to move it there in its entirety.
    case capi_auth:preauthorize_api_key(ApiKey) of
        {ok, Context} ->
            {true, Context};
        {error, Error} ->
            _ = logger:info("API Key preauthorization failed for ~p due to ~p", [OperationID, Error]),
            false
    end.

-spec map_error(error_type(), swag_server_validation:error()) -> swag_server:error_reason().
map_error(validation_error, Error) ->
    Type = genlib:to_binary(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message =
        case maps:get(description, Error, undefined) of
            undefined ->
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
            Description ->
                DescriptionBin = genlib:to_binary(Description),
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary, ", description: ",
                    DescriptionBin/binary>>
        end,
    jsx:encode(#{
        <<"code">> => <<"invalidRequest">>,
        <<"message">> => Message
    }).

get_handlers() ->
    [
        capi_handler_accounts,
        capi_handler_analytics,
        capi_handler_categories,
        capi_handler_claims,
        capi_handler_contracts,
        capi_handler_countries,
        capi_handler_customers,
        capi_handler_geo,
        capi_handler_invoice_templates,
        capi_handler_invoices,
        capi_handler_parties,
        capi_handler_payment_institutions,
        capi_handler_payments,
        capi_handler_payouts,
        capi_handler_reports,
        capi_handler_search,
        capi_handler_shops,
        capi_handler_trade_blocs,
        capi_handler_webhooks
    ].

-spec handle_request(
    OperationID :: operation_id(),
    Req :: request_data(),
    SwagContext :: request_context(),
    HandlerOpts :: handler_opts()
) -> {ok | error, response()}.
handle_request(OperationID, Req, SwagContext, HandlerOpts) ->
    scoper:scope(swagger, fun() ->
        handle_function_(OperationID, Req, SwagContext, HandlerOpts)
    end).

-spec handle_function_(
    OperationID :: operation_id(),
    Req :: request_data(),
    SwagContext :: request_context(),
    HandlerOpts :: handler_opts()
) -> {ok | error, response()}.
handle_function_(OperationID, Req, SwagContext0, HandlerOpts) ->
    try
        RpcID = create_rpc_id(Req),
        ok = set_rpc_meta(RpcID),
        ok = set_request_meta(OperationID, Req),
        _ = logger:info("Processing request ~p", [OperationID]),
        WoodyContext0 = attach_deadline(Req, create_woody_context(RpcID)),
        SwagContext = do_authorize_api_key(SwagContext0, WoodyContext0),
        WoodyContext = put_user_identity(WoodyContext0, get_auth_context(SwagContext)),
        Context = create_processing_context(OperationID, SwagContext, WoodyContext, HandlerOpts),
        ok = set_context_meta(Context),
        {ok, RequestState} = prepare(OperationID, Req, Context, get_handlers()),
        #{authorize := Authorize, process := Process} = RequestState,
        {ok, Resolution} = Authorize(),
        case Resolution of
            allowed ->
                Process();
            forbidden ->
                {ok, {401, #{}, undefined}}
        end
    catch
        throw:{token_auth_failed, Reason} ->
            _ = logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Reason]),
            {error, {401, #{}, undefined}};
        throw:{handler_respond, HandlerResponse} ->
            {ok, HandlerResponse};
        throw:{bad_deadline, _Deadline} ->
            {ok, logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>)};
        throw:{handler_function_clause, _OperationID} ->
            _ = logger:error("Operation ~p failed due to missing handler", [OperationID]),
            {error, {501, #{}, undefined}};
        throw:invalid_payment_token ->
            {ok,
                logic_error(
                    invalidPaymentToolToken,
                    <<"Specified payment tool token is invalid">>
                )};
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        Class:Reason:Stacktrace ->
            process_general_error(Class, Reason, Stacktrace, Req, SwagContext0)
    after
        ok = clear_rpc_meta()
    end.

-spec prepare(
    OperationID :: operation_id(),
    Req :: request_data(),
    Context :: processing_context(),
    Handlers :: list(module())
) -> {ok, request_state()}.
prepare(OperationID, _Req, _Context, []) ->
    erlang:throw({handler_function_clause, OperationID});
prepare(OperationID, Req, Context, [Handler | Rest]) ->
    case Handler:prepare(OperationID, Req, Context) of
        {error, noimpl} ->
            prepare(OperationID, Req, Context, Rest);
        {ok, State} ->
            {ok, State}
    end.

-spec respond(response()) -> throw(response()).
respond(Response) ->
    erlang:throw({handler_respond, Response}).

-spec respond_if_undefined(undefined | _Entity, response()) -> ok | throw(response()).
respond_if_undefined(undefined, Response) ->
    respond(Response);
respond_if_undefined(_, _Response) ->
    ok.

-spec respond_if_forbidden(Resolution, response()) -> Resolution | throw(response()) when
    Resolution :: capi_auth:resolution().
respond_if_forbidden(forbidden, Response) ->
    respond(Response);
respond_if_forbidden(allowed, _Response) ->
    allowed.

%%

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

do_authorize_api_key(SwagContext = #{auth_context := PreAuthContext}, WoodyContext) ->
    case capi_auth:authorize_api_key(PreAuthContext, make_token_context(SwagContext), WoodyContext) of
        {ok, AuthContext} ->
            SwagContext#{auth_context => AuthContext};
        {error, Error} ->
            throw({token_auth_failed, Error})
    end.

make_token_context(#{cowboy_req := CowboyReq}) ->
    case cowboy_req:header(<<"origin">>, CowboyReq) of
        Origin when is_binary(Origin) ->
            #{request_origin => Origin};
        undefined ->
            #{}
    end.

create_processing_context(OperationID, SwaggerContext, WoodyContext, HandlerOpts) ->
    #{
        operation_id => OperationID,
        woody_context => WoodyContext,
        swagger_context => SwaggerContext,
        party_client_context => party_client:create_context(#{woody_context => WoodyContext}),
        party_client => maps:get(party_client, HandlerOpts)
    }.

-spec create_rpc_id(request_data()) -> woody:rpc_id().
create_rpc_id(Req) ->
    RequestID = maps:get('X-Request-ID', Req),
    woody_context:new_rpc_id(genlib:to_binary(RequestID)).

create_woody_context(RpcID) ->
    woody_context:new(RpcID).

put_user_identity(WoodyContext, AuthContext) ->
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyContext).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => capi_auth:get_subject_id(AuthContext),
        %%TODO: Store user realm in authdata meta and extract it here
        realm => ?REALM,
        email => capi_auth:get_user_email(AuthContext)
    }).

attach_deadline(#{'X-Request-Deadline' := undefined}, Context) ->
    Context;
attach_deadline(#{'X-Request-Deadline' := Header}, Context) ->
    case capi_utils:parse_deadline(Header) of
        {ok, Deadline} when Deadline /= undefined ->
            woody_context:set_deadline(Deadline, Context);
        _ ->
            throw({bad_deadline, Header})
    end.

process_woody_error(_Source, result_unexpected, _Details) ->
    {error, server_error(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, server_error(503)};
process_woody_error(_Source, result_unknown, _Details) ->
    {error, server_error(504)}.

process_general_error(Class, Reason, Stacktrace, Req, SwagContext) ->
    _ = logger:error(
        "Operation failed due to ~p:~p given req: ~p and context: ~p ~n~p",
        [Class, Reason, Req, SwagContext, Stacktrace],
        #{
            error => #{
                class => genlib:to_binary(Class),
                reason => genlib:format(Reason),
                stack_trace => genlib_format:format_stacktrace(Stacktrace)
            }
        }
    ),
    {error, server_error(500)}.

-spec set_context_meta(processing_context()) -> ok.
set_context_meta(Context) ->
    AuthContext = capi_handler_utils:get_auth_context(Context),
    Meta = #{
        metadata => #{
            'user-identity' => collect_user_identity(AuthContext)
        }
    },
    scoper:add_meta(Meta).

-spec set_request_meta(operation_id(), request_data()) -> ok.
set_request_meta(OperationID, Req) ->
    InterestParams = [
        invoiceID,
        invoiceTemplateID,
        contractID,
        webhookID,
        reportID,
        shopID,
        customerID
    ],
    Meta = #{
        operation_id => OperationID,
        request_id => maps:get('X-Request-ID', Req),
        parameters => maps:with(InterestParams, Req)
    },
    scoper:add_meta(genlib_map:compact(Meta)).

-spec set_rpc_meta(woody:rpc_id()) -> ok.
set_rpc_meta(RpcID) ->
    %% trace_id, parent_id and span_id must be top-level meta keys
    logger:update_process_metadata(maps:with([trace_id, parent_id, span_id], RpcID)).

-spec clear_rpc_meta() -> ok.
clear_rpc_meta() ->
    case logger:get_process_metadata() of
        undefined ->
            ok;
        Metadata ->
            logger:set_process_metadata(maps:without([trace_id, parent_id, span_id], Metadata))
    end.
