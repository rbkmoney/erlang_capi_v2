-module(capi_handler_customers).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('CreateCustomer', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Call = {customer_management, 'Create', [encode_customer_params(PartyID, maps:get('Customer', Req))]},
    case capi_handler_utils:service_call_with([party_creation], Call, Context) of
        {ok, Customer} ->
            {ok, {201, #{}, make_customer_and_token(Customer, PartyID)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_ShopNotFound{} ->
                    {ok, logic_error(invalidShopID, <<"Shop not found">>)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp}
            end
    end;

process_request('GetCustomerById', Req, Context) ->
    case get_customer_by_id(maps:get('customerID', Req), Context) of
        {ok, Customer} ->
            {ok, {200, #{}, decode_customer(Customer)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)}
            end
    end;

process_request('DeleteCustomer', Req, Context) ->
    case capi_handler_utils:service_call({customer_management, 'Delete', [maps:get(customerID, Req)]}, Context) of
        {ok, _} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)}
            end
    end;

process_request('CreateCustomerAccessToken', Req, Context) ->
    CustomerID = maps:get(customerID, Req),
    case get_customer_by_id(CustomerID, Context) of
        {ok, #payproc_Customer{}} ->
            Response = capi_handler_utils:issue_access_token(
                capi_handler_utils:get_party_id(Context),
                {customer, CustomerID}
            ),
            {ok, {201, #{}, Response}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)}
            end
    end;

process_request('CreateBinding', Req, Context) ->
    CallArgs = [maps:get(customerID, Req), encode_customer_binding_params(maps:get('CustomerBindingParams', Req))],
    Result =
        try
            capi_handler_utils:service_call({customer_management, 'StartBinding', CallArgs}, Context)
        catch
            throw:Error when Error =:= invalid_token orelse Error =:= invalid_payment_session ->
                {error, Error}
        end,

    case Result of
        {ok, CustomerBinding} ->
            {ok, {201, #{}, decode_customer_binding(CustomerBinding, Context)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvalidPaymentTool{} ->
                    ErrorResp = logic_error(invalidPaymentResource, <<"Invalid payment resource">>),
                    {ok, ErrorResp};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(operationNotPermitted, <<"Operation not permitted">>),
                    {ok, ErrorResp};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)}
            end;
        {error, invalid_token} ->
            ErrorResp = logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            ),
            {ok, ErrorResp};
        {error, invalid_payment_session} ->
            ErrorResp = logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            ),
            {ok, ErrorResp}
    end;

process_request('GetBindings', Req, Context) ->
    case get_customer_by_id(maps:get(customerID, Req), Context) of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            {ok, {200, #{}, [decode_customer_binding(B, Context) || B <- Bindings]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)}
            end
    end;

process_request('GetBinding', Req, Context) ->
    case get_customer_by_id(maps:get(customerID, Req), Context) of
        {ok, #payproc_Customer{bindings = Bindings}} ->
            case lists:keyfind(maps:get(customerBindingID, Req), #payproc_CustomerBinding.id, Bindings) of
                #payproc_CustomerBinding{} = B ->
                    {ok, {200, #{}, decode_customer_binding(B, Context)}};
                false ->
                    {ok, general_error(404, <<"Customer binding not found">>)}
            end;
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)}
            end
    end;

process_request('GetCustomerEvents', Req, Context) ->
    GetterFun =
        fun(Range) ->
            capi_handler_utils:service_call(
                {customer_management, 'GetEvents', [maps:get(customerID, Req), Range]},
                Context
            )
        end,
    Result =
        capi_handler_utils:collect_events(
            maps:get(limit, Req),
            genlib_map:get(eventID, Req),
            GetterFun,
            fun decode_customer_event/2,
            undefined
        ),
    case Result of
        {ok, Events} when is_list(Events) ->
            {ok, {200, #{}, Events}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_CustomerNotFound{} ->
                    {ok, general_error(404, <<"Customer not found">>)};
                #payproc_EventNotFound{} ->
                    {ok, general_error(404, <<"Event not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)}
            end
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

get_customer_by_id(CustomerID, Context) ->
    capi_handler_utils:service_call({customer_management, 'Get', [CustomerID]}, Context).

encode_customer_params(PartyID, Params) ->
    #payproc_CustomerParams{
        party_id     = PartyID,
        shop_id      = genlib_map:get(<<"shopID">>, Params),
        contact_info = capi_handler_encoder:encode_contact_info(genlib_map:get(<<"contactInfo">>, Params)),
        metadata     = encode_customer_metadata(genlib_map:get(<<"metadata">>, Params))
    }.

encode_customer_metadata(Meta) ->
    capi_json_marshalling:marshal(Meta).


encode_customer_binding_params(#{<<"paymentResource">> := PaymentResource}) ->
    PaymentTool = capi_handler_encoder:encode_payment_tool_token(maps:get(<<"paymentToolToken">>, PaymentResource)),
    {ClientInfo, PaymentSession} =
        capi_handler_utils:unwrap_payment_session(maps:get(<<"paymentSession">>, PaymentResource)),
    #payproc_CustomerBindingParams{
        payment_resource =
            #domain_DisposablePaymentResource{
                payment_tool       = PaymentTool,
                payment_session_id = PaymentSession,
                client_info        = capi_handler_encoder:encode_client_info(ClientInfo)
            }
    }.

make_customer_and_token(Customer, PartyID) ->
    #{
        <<"customer"           >> => decode_customer(Customer),
        <<"customerAccessToken">> =>
            capi_handler_utils:issue_access_token(PartyID, {customer, Customer#payproc_Customer.id})
    }.

decode_customer(Customer) ->
    #{
        <<"id"         >> => Customer#payproc_Customer.id,
        <<"shopID"     >> => Customer#payproc_Customer.shop_id,
        <<"status"     >> => decode_customer_status(Customer#payproc_Customer.status),
        <<"contactInfo">> =>
            capi_handler_decoder_party:decode_contact_info(Customer#payproc_Customer.contact_info),
        <<"metadata"   >> => decode_customer_metadata(Customer#payproc_Customer.metadata)
    }.

decode_customer_status({Status, _}) ->
    erlang:atom_to_binary(Status, utf8).

decode_customer_metadata(Meta) ->
    capi_json_marshalling:unmarshal(Meta).

decode_customer_binding(CustomerBinding, Context) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id"             >> => CustomerBinding#payproc_CustomerBinding.id,
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
        <<"error" >> => Error
    }.

decode_customer_event(Event = #payproc_Event{source = {customer_id, _}, payload = Payload}, Context) ->
    case decode_customer_changes(Payload, Context) of
        [_Something | _] = Changes ->
            {true, #{
                <<"id"       >> => Event#payproc_Event.id,
                <<"createdAt">> => Event#payproc_Event.created_at,
                <<"changes"  >> => Changes
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
        <<"changeType"     >> => <<"CustomerBindingStarted">>,
        <<"customerBinding">> => decode_customer_binding(CustomerBinding, Context)
    }};
decode_customer_binding_change(BindingID, {status_changed, StatusChange}, Context) ->
    #payproc_CustomerBindingStatusChanged{status = Status} = StatusChange,
    {true, capi_handler_utils:merge_and_compact(
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
