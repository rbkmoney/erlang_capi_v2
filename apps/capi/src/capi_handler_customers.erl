-module(capi_handler_customers).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreateCustomer' = OperationID, Req, Context) ->
    CustomerParams = maps:get('CustomerParams', Req),
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(<<"partyID">>, CustomerParams, UserID),
    ShopID = maps:get(<<"shopID">>, CustomerParams),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        try
            CustomerID = generate_customer_id(OperationID, PartyID, CustomerParams, Context),
            EncodedCustomerParams = encode_customer_params(CustomerID, PartyID, CustomerParams),
            Call = {customer_management, 'Create', {EncodedCustomerParams}},
            case capi_handler_call:service_call(Call, Context) of
                {ok, Customer} ->
                    {ok, {201, #{}, make_customer_and_token(Customer, PartyID)}};
                {exception, #payproc_InvalidUser{}} ->
                    {ok, logic_error(invalidPartyID, <<"Party not found">>)};
                {exception, #payproc_InvalidPartyStatus{}} ->
                    {ok, logic_error(<<"invalidPartyStatus">>, <<"Invalid party status">>)};
                {exception, #payproc_InvalidShopStatus{}} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                {exception, #payproc_ShopNotFound{}} ->
                    {ok, logic_error(invalidShopID, <<"Shop not found">>)};
                {exception, #payproc_PartyNotFound{}} ->
                    {ok, logic_error(<<"invalidPartyID">>, <<"Party not found">>)};
                {exception, #payproc_OperationNotPermitted{}} ->
                    {ok, logic_error(operationNotPermitted, <<"Operation not permitted">>)}
            end
        catch
            throw:{external_id_conflict, ID, UsedExternalID, _Schema} ->
                {ok, logic_error(externalIDConflict, {ID, UsedExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerById' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    CustomerReply = get_customer_by_id(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => maybe_woody_reply(CustomerReply)}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case CustomerReply of
            {ok, Customer} ->
                {ok, {200, #{}, decode_customer(Customer)}};
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('DeleteCustomer' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    CustomerReply = get_customer_by_id(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => maybe_woody_reply(CustomerReply)}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        Call = {customer_management, 'Delete', {CustomerID}},
        case capi_handler_call:service_call(Call, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateCustomerAccessToken' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    CustomerReply = get_customer_by_id(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => maybe_woody_reply(CustomerReply)}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case CustomerReply of
            {ok, #payproc_Customer{owner_id = PartyID}} ->
                Response = capi_handler_utils:issue_access_token(PartyID, {customer, CustomerID}),
                {ok, {201, #{}, Response}};
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateBinding' = OperationID, Req, Context) ->
    CustomerID = maps:get(customerID, Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => CustomerID}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        Result =
            try
                CustomerBindingParams = maps:get('CustomerBindingParams', Req),

                {CustomerBindingID, RecPaymentToolID} = generate_binding_ids(
                    OperationID,
                    CustomerBindingParams,
                    Context
                ),

                EncodedCustomerBindingParams = encode_customer_binding_params(
                    CustomerBindingID,
                    RecPaymentToolID,
                    CustomerBindingParams
                ),

                Call = {customer_management, 'StartBinding', {CustomerID, EncodedCustomerBindingParams}},
                capi_handler_call:service_call(Call, Context)
            catch
                throw:invalid_token ->
                    {error, invalid_token};
                throw:invalid_payment_session ->
                    {error, invalid_payment_session};
                throw:Error = {external_id_conflict, _, _, _} ->
                    {error, Error}
            end,
        case Result of
            {ok, CustomerBinding} ->
                {ok, {201, #{}, decode_customer_binding(CustomerBinding, Context)}};
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
            {exception, #payproc_InvalidContractStatus{}} ->
                {ok, logic_error(invalidRequest, <<"Invalid contract status">>)};
            {exception, #payproc_OperationNotPermitted{}} ->
                {ok, logic_error(operationNotPermitted, <<"Operation not permitted">>)};
            {error, invalid_token} ->
                {ok, logic_error(invalidPaymentToolToken, <<"Specified payment tool token is invalid">>)};
            {error, invalid_payment_session} ->
                {ok, logic_error(invalidPaymentSession, <<"Specified payment session is invalid">>)};
            {error, {external_id_conflict, ID, UsedExternalID, _Schema}} ->
                {ok, logic_error(externalIDConflict, {ID, UsedExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetBindings' = OperationID, Req, Context) ->
    CustomerID = maps:get(customerID, Req),
    CustomerReply = get_customer_by_id(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => maybe_woody_reply(CustomerReply)}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case CustomerReply of
            {ok, #payproc_Customer{bindings = Bindings}} ->
                {ok, {200, #{}, [decode_customer_binding(B, Context) || B <- Bindings]}};
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetBinding' = OperationID, Req, Context) ->
    CustomerID = maps:get(customerID, Req),
    CustomerBindingID = maps:get(customerBindingID, Req),
    CustomerReply = get_customer_by_id(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID, binding => CustomerBindingID}},
            {payproc, #{customer => maybe_woody_reply(CustomerReply)}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        case CustomerReply of
            {ok, #payproc_Customer{bindings = Bindings}} ->
                case lists:keyfind(CustomerBindingID, #payproc_CustomerBinding.id, Bindings) of
                    #payproc_CustomerBinding{} = B ->
                        {ok, {200, #{}, decode_customer_binding(B, Context)}};
                    false ->
                        {ok, general_error(404, <<"Customer binding not found">>)}
                end;
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerEvents' = OperationID, Req, Context) ->
    CustomerID = maps:get(customerID, Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => CustomerID}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        GetterFun = fun(Range) ->
            capi_handler_call:service_call(
                {customer_management, 'GetEvents', {CustomerID, Range}},
                Context
            )
        end,
        Result = capi_handler_utils:collect_events(
            maps:get(limit, Req),
            genlib_map:get(eventID, Req),
            GetterFun,
            fun decode_customer_event/2,
            undefined
        ),
        case Result of
            {ok, Events} when is_list(Events) ->
                {ok, {200, #{}, Events}};
            {exception, #payproc_InvalidUser{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_EventNotFound{}} ->
                {ok, general_error(404, <<"Event not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

maybe_woody_reply({ok, Reply}) ->
    Reply;
maybe_woody_reply({exception, _}) ->
    undefined.

get_customer_by_id(CustomerID, Context) ->
    EventRange = #payproc_EventRange{},
    capi_handler_call:service_call({customer_management, 'Get', {CustomerID, EventRange}}, Context).

generate_customer_id(OperationID, PartyID, CustomerParams, #{woody_context := WoodyContext}) ->
    ExternalID = maps:get(<<"externalID">>, CustomerParams, undefined),
    IdempKey = {OperationID, PartyID, ExternalID},
    Identity = {schema, capi_feature_schemas:customer(), CustomerParams},
    capi_bender:try_gen_snowflake(IdempKey, Identity, WoodyContext).

encode_customer_params(CustomerID, PartyID, Params) ->
    #payproc_CustomerParams{
        customer_id = CustomerID,
        party_id = PartyID,
        shop_id = genlib_map:get(<<"shopID">>, Params),
        contact_info = capi_handler_encoder:encode_contact_info(genlib_map:get(<<"contactInfo">>, Params)),
        metadata = encode_customer_metadata(genlib_map:get(<<"metadata">>, Params))
    }.

encode_customer_metadata(Meta) ->
    capi_json_marshalling:marshal(Meta).

generate_binding_ids(OperationID, CustomerBindingParams, Context = #{woody_context := WoodyContext}) ->
    ExternalID = maps:get(<<"externalID">>, CustomerBindingParams, undefined),
    UserID = capi_handler_utils:get_user_id(Context),

    PaymentResource = maps:get(<<"paymentResource">>, CustomerBindingParams),
    PaymentToolToken = maps:get(<<"paymentToolToken">>, PaymentResource),
    PaymentTool = capi_handler_decoder_party:decode_payment_tool(encode_payment_tool_token(PaymentToolToken)),
    CustomerBindingParamsEncrypted =
        maps:put(
            <<"paymentResource">>,
            maps:put(
                <<"paymentTool">>,
                PaymentTool,
                maps:remove(<<"paymentToolToken">>, PaymentResource)
            ),
            CustomerBindingParams
        ),

    Identity = capi_bender:make_identity(
        {schema, capi_feature_schemas:customer_binding(), CustomerBindingParamsEncrypted}
    ),

    OperationIDBin = erlang:atom_to_binary(OperationID),
    CustomerBindingID = capi_bender:try_gen_snowflake(
        {<<OperationIDBin/binary, "+CustomerBindingID">>, UserID, ExternalID},
        Identity,
        WoodyContext
    ),
    RecPaymentToolID = capi_bender:try_gen_snowflake(
        {<<OperationIDBin/binary, "+RecPaymentToolID">>, UserID, ExternalID},
        Identity,
        WoodyContext
    ),
    {CustomerBindingID, RecPaymentToolID}.

encode_customer_binding_params(
    CustomerBindingID,
    RecPaymentToolID,
    #{<<"paymentResource">> := PaymentResource}
) ->
    PaymentToolToken = maps:get(<<"paymentToolToken">>, PaymentResource),
    PaymentTool = encode_payment_tool_token(PaymentToolToken),

    {ClientInfo, PaymentSession} =
        capi_handler_utils:unwrap_payment_session(maps:get(<<"paymentSession">>, PaymentResource)),

    #payproc_CustomerBindingParams{
        customer_binding_id = CustomerBindingID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = capi_handler_encoder:encode_client_info(ClientInfo)
        }
    }.

encode_payment_tool_token(Token) ->
    case capi_payment_tool:decrypt_payment_tool_token(Token) of
        {ok, {PaymentTool, ValidUntil}} ->
            case capi_utils:deadline_is_reached(ValidUntil) of
                true ->
                    logger:warning("Payment tool token expired: ~p", [capi_utils:deadline_to_binary(ValidUntil)]),
                    erlang:throw(invalid_token);
                _ ->
                    PaymentTool
            end;
        unrecognized ->
            encode_legacy_payment_tool_token(Token);
        {error, {decryption_failed, Error}} ->
            logger:warning("Payment tool token decryption failed: ~p", [Error]),
            erlang:throw(invalid_token)
    end.

encode_legacy_payment_tool_token(Token) ->
    try
        capi_handler_encoder:encode_payment_tool(capi_utils:base64url_to_map(Token))
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end.

make_customer_and_token(Customer, PartyID) ->
    #{
        <<"customer">> => decode_customer(Customer),
        <<"customerAccessToken">> =>
            capi_handler_utils:issue_access_token(PartyID, {customer, Customer#payproc_Customer.id})
    }.

decode_customer(Customer) ->
    #{
        <<"id">> => Customer#payproc_Customer.id,
        <<"shopID">> => Customer#payproc_Customer.shop_id,
        <<"status">> => decode_customer_status(Customer#payproc_Customer.status),
        <<"contactInfo">> =>
            capi_handler_decoder_party:decode_contact_info(Customer#payproc_Customer.contact_info),
        <<"metadata">> => decode_customer_metadata(Customer#payproc_Customer.metadata)
    }.

decode_customer_status({Status, _}) ->
    erlang:atom_to_binary(Status, utf8).

decode_customer_metadata(Meta) ->
    capi_json_marshalling:unmarshal(Meta).

decode_customer_binding(CustomerBinding, Context) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => CustomerBinding#payproc_CustomerBinding.id,
            <<"paymentResource">> =>
                capi_handler_decoder_party:decode_disposable_payment_resource(
                    CustomerBinding#payproc_CustomerBinding.payment_resource
                )
        },
        decode_customer_binding_status(CustomerBinding#payproc_CustomerBinding.status, Context)
    ).

decode_customer_binding_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #payproc_CustomerBindingFailed{failure = OperationFailure} ->
                capi_handler_decoder_utils:decode_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_customer_event(Event = #payproc_Event{source = {customer_id, _}, payload = Payload}, Context) ->
    case decode_customer_changes(Payload, Context) of
        [_Something | _] = Changes ->
            {true, #{
                <<"id">> => Event#payproc_Event.id,
                <<"createdAt">> => Event#payproc_Event.created_at,
                <<"changes">> => Changes
            }};
        [] ->
            false
    end.

decode_customer_changes({customer_changes, CustomerChanges}, Context) ->
    lists:filtermap(
        fun(V) -> decode_customer_change(V, Context) end,
        CustomerChanges
    ).

decode_customer_change({customer_binding_changed, CustomerBindingChanged}, Context) ->
    #payproc_CustomerBindingChanged{id = BindingID, payload = Payload} = CustomerBindingChanged,
    decode_customer_binding_change(BindingID, Payload, Context);
decode_customer_change(_, _) ->
    false.

decode_customer_binding_change(_, {started, Start}, Context) ->
    #payproc_CustomerBindingStarted{binding = CustomerBinding} = Start,
    {true, #{
        <<"changeType">> => <<"CustomerBindingStarted">>,
        <<"customerBinding">> => decode_customer_binding(CustomerBinding, Context)
    }};
decode_customer_binding_change(BindingID, {status_changed, StatusChange}, Context) ->
    #payproc_CustomerBindingStatusChanged{status = Status} = StatusChange,
    {true,
        capi_handler_utils:merge_and_compact(
            #{
                <<"changeType">> => <<"CustomerBindingStatusChanged">>,
                <<"customerBindingID">> => BindingID
            },
            decode_customer_binding_status(Status, Context)
        )};
decode_customer_binding_change(BindingID, {interaction_requested, InteractionRequest}, _) ->
    #payproc_CustomerBindingInteractionRequested{interaction = UserInteraction} = InteractionRequest,
    {true, #{
        <<"changeType">> => <<"CustomerBindingInteractionRequested">>,
        <<"customerBindingID">> => BindingID,
        <<"userInteraction">> => capi_handler_decoder_invoicing:decode_user_interaction(UserInteraction)
    }}.
