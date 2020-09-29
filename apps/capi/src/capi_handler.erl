-module(capi_handler).

-behaviour(swag_server_logic_handler).

-type error_type() :: swag_server_logic_handler:error_type().

%% API callbacks
-export([authorize_api_key/3]).
-export([map_error/2]).
-export([handle_request/4]).

%% Handler behaviour

-export_type([operation_id/0]).
-export_type([request_data/0]).
-export_type([request_context/0]).
-export_type([response/0]).
-export_type([processing_context/0]).

-callback process_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    Context     :: processing_context()
) ->
    {ok | error, response() | noimpl}.

-import(capi_handler_utils, [logic_error/2, server_error/1]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).
-define(DOMAIN, <<"common-api">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key(), handler_opts()) ->
    Result :: false | {true, capi_auth:context()}.

authorize_api_key(OperationID, ApiKey, _HandlerOpts) ->
    case uac:authorize_api_key(ApiKey, get_verification_options()) of
        {ok, Context} ->
            check_blacklist(ApiKey, Context);
        {error, Error} ->
            _ = logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]),
            false
    end.

-spec map_error(error_type(), swag_server_validation:error()) ->
    swag_server:error_reason().

map_error(validation_error, Error) ->
    Type = genlib:to_binary(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message = case maps:get(description, Error, undefined) of
        undefined ->
            <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
        Description ->
            DescriptionBin = genlib:to_binary(Description),
            <<"Request parameter: ", Name/binary,
            ", error type: ", Type/binary,
            ", description: ", DescriptionBin/binary>>
    end,
    jsx:encode(#{
        <<"code">> => <<"invalidRequest">>,
        <<"message">> => Message
    }).

-type request_data()        :: #{atom() | binary() => term()}.

-type operation_id()        :: swag_server:operation_id().
-type request_context()     :: swag_server:request_context().
-type response()            :: swag_server:response().
-type handler_opts()        :: swag_server:handler_opts(_).
-type processing_context()  :: #{
    swagger_context := swag_server:request_context(),
    woody_context   := woody_context:ctx()
}.

get_handlers() ->
    [
        capi_handler_accounts,
        capi_handler_analytics,
        capi_handler_categories,
        capi_handler_claims,
        capi_handler_contracts,
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
        capi_handler_webhooks
    ].

-spec handle_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    SwagContext :: request_context(),
    HandlerOpts :: handler_opts()
) ->
    {ok | error,   response()}.

handle_request(OperationID, Req, SwagContext, HandlerOpts) ->
    scoper:scope(swagger, fun() ->
        handle_function_(OperationID, Req, SwagContext, HandlerOpts)
    end).

-spec handle_function_(
    OperationID :: operation_id(),
    Req         :: request_data(),
    SwagContext :: request_context(),
    HandlerOpts :: handler_opts()
) ->
    {ok | error,   response()}.

handle_function_(OperationID, Req, SwagContext = #{auth_context := AuthContext}, _HandlerOpts) ->
    try
        RpcID = create_rpc_id(Req),
        ok = set_rpc_meta(RpcID),
        ok = set_request_meta(OperationID, Req),
        _ = logger:info("Processing request ~p", [OperationID]),
        OperationACL = capi_auth:get_operation_access(OperationID, Req),
        case uac:authorize_operation(OperationACL, AuthContext) of
            ok ->
                WoodyContext = attach_deadline(Req, create_woody_context(RpcID, AuthContext)),
                Context = create_processing_context(SwagContext, WoodyContext),
                ok = set_context_meta(Context),
                process_request(OperationID, Req, Context, get_handlers());
            {error, _} = Error ->
                _ = logger:info("Authorization failed due to ~p", [Error]),
                {ok, {401, #{}, undefined}}
        end
    catch
        throw:{bad_deadline, _Deadline} ->
            {ok, logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>)};
        throw:{handler_function_clause, _OperationID} ->
            _ = logger:error("Operation ~p failed due to missing handler", [OperationID]),
            {error, {501, #{}, undefined}};
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        Class:Reason:Stacktrace ->
            process_general_error(Class, Reason, Stacktrace, Req, SwagContext)
    after
        ok = clear_rpc_meta()
    end.

-spec process_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    Context     :: processing_context(),
    Handlers    :: list(module())
) ->
    {ok | error,   response()}.

process_request(OperationID, _Req, _Context, []) ->
    erlang:throw({handler_function_clause, OperationID});
process_request(OperationID, Req, Context, [Handler | Rest]) ->
    case Handler:process_request(OperationID, Req, Context) of
        {error, noimpl} ->
            process_request(OperationID, Req, Context, Rest);
        Response ->
            Response
    end.

%%

create_processing_context(SwaggerContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwaggerContext
    }.

-spec create_rpc_id(request_data()) -> woody:rpc_id().
create_rpc_id(Req) ->
    RequestID = maps:get('X-Request-ID', Req),
    woody_context:new_rpc_id(genlib:to_binary(RequestID)).

create_woody_context(RpcID, AuthContext) ->
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => uac_authorizer_jwt:get_subject_id(AuthContext),
        realm    => ?REALM,
        email    => uac_authorizer_jwt:get_claim(<<"email">>, AuthContext, undefined),
        username => uac_authorizer_jwt:get_claim(<<"name">> , AuthContext, undefined)
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

process_woody_error(_Source, result_unexpected   , _Details) ->
    {error, server_error(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, server_error(503)};
process_woody_error(_Source, result_unknown      , _Details) ->
    {error, server_error(504)}.

process_general_error(Class, Reason, Stacktrace, Req, SwagContext) ->
    _ = logger:error(
        "Operation failed due to ~p:~p given req: ~p and context: ~p",
        [Class, Reason, Req, SwagContext],
        #{error => #{
            class       => genlib:to_binary(Class),
            reason      => genlib:format(Reason),
            stack_trace => genlib_format:format_stacktrace(Stacktrace)
        }}
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

get_verification_options() ->
    #{
        domains_to_decode => [?DOMAIN]
    }.

check_blacklist(ApiKey, Context) ->
    case capi_api_key_blacklist:check(ApiKey) of
        true ->
            SubjectId = uac_authorizer_jwt:get_subject_id(Context),
            _ = logger:warning("Blacklisted API Key usage detected for subject_id: ~p", [SubjectId]),
            false;
        false ->
            {true, Context}
    end.
