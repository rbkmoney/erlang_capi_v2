-module(capi_handler_webhooks).

-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreateWebhook' = OperationID, Req, Context) ->
    Params = maps:get('Webhook', Req),
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(<<"partyID">>, Params, UserID),
    Authorize = fun() ->
        Prototypes = [{operation, #{party => PartyID, id => OperationID}}],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        WebhookParams = encode_webhook_params(PartyID, Params),
        ShopID = validate_webhook_params(WebhookParams),
        Call = {party_management, 'GetShop', {PartyID, ShopID}},
        case capi_handler_call:service_call_with([user_info], Call, Context) of
            {ok, _} ->
                case capi_handler_call:service_call({webhook_manager, 'Create', {WebhookParams}}, Context) of
                    {ok, Webhook} ->
                        {ok, {201, #{}, decode_webhook(Webhook)}};
                    {exception, #webhooker_LimitExceeded{}} ->
                        {ok, general_error(429, <<"Webhook limit exceeded">>)}
                end;
            {exception, #payproc_InvalidUser{}} ->
                {ok, logic_error(invalidPartyID, <<"Party not found">>)};
            {exception, #payproc_ShopNotFound{}} ->
                {ok, logic_error(invalidShopID, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetWebhooks' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{party => PartyID, id => OperationID}}],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        Webhooks = capi_utils:unwrap(
            capi_handler_call:service_call({webhook_manager, 'GetList', {PartyID}}, Context)
        ),
        {ok, {200, #{}, [decode_webhook(V) || V <- Webhooks]}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetWebhookByID' = OperationID, Req, Context) ->
    WebhookID = maps:get(webhookID, Req),
    Webhook =
        case encode_webhook_id(WebhookID) of
            {ok, ID} ->
                case get_webhook(ID, Context) of
                    {ok, Result} ->
                        Result;
                    {exception, #webhooker_WebhookNotFound{}} ->
                        undefined
                end;
            error ->
                undefined
        end,
    Authorize = fun() ->
        Prototypes = [
            {operation, #{webhook => WebhookID, id => OperationID}},
            {webhooks, #{webhook => Webhook}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Webhook, general_error(404, <<"Webhook not found">>)),
        {ok, {200, #{}, decode_webhook(Webhook)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('DeleteWebhookByID' = OperationID, Req, Context) ->
    WebhookID = maps:get(webhookID, Req),
    {EncodedWebhookID, Webhook} =
        case encode_webhook_id(WebhookID) of
            {ok, ID} ->
                case get_webhook(ID, Context) of
                    {ok, Result} ->
                        {ID, Result};
                    {exception, #webhooker_WebhookNotFound{}} ->
                        capi_handler:respond({204, #{}, undefined})
                end;
            error ->
                {undefined, undefined}
        end,
    Authorize = fun() ->
        Prototypes = [
            {operation, #{webhook => WebhookID, id => OperationID}},
            {webhooks, #{webhook => Webhook}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(EncodedWebhookID, general_error(404, <<"Webhook not found">>)),
        case delete_webhook(EncodedWebhookID, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, #webhooker_WebhookNotFound{}} ->
                {ok, {204, #{}, undefined}}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
%%

prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

validate_webhook_params(#webhooker_WebhookParams{event_filter = EventFilter}) ->
    validate_event_filter(EventFilter).

validate_event_filter({invoice, #webhooker_InvoiceEventFilter{shop_id = ShopID}}) ->
    validate_event_filter_shop(ShopID);
validate_event_filter({customer, #webhooker_CustomerEventFilter{shop_id = ShopID}}) ->
    validate_event_filter_shop(ShopID).

validate_event_filter_shop(ShopID) when ShopID /= undefined ->
    ShopID.

encode_webhook_id(WebhookID) ->
    try
        {ok, binary_to_integer(WebhookID)}
    catch
        error:badarg ->
            error
    end.

get_webhook(WebhookID, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    case capi_handler_call:service_call({webhook_manager, 'Get', {WebhookID}}, Context) of
        {ok, Webhook = #webhooker_Webhook{party_id = PartyID}} ->
            {ok, Webhook};
        {ok, _Webhook} ->
            {exception, #webhooker_WebhookNotFound{}};
        {exception, Exception} ->
            {exception, Exception}
    end.

delete_webhook(WebhookID, Context) ->
    case get_webhook(WebhookID, Context) of
        {ok, #webhooker_Webhook{}} ->
            capi_handler_call:service_call({webhook_manager, 'Delete', {WebhookID}}, Context);
        Exception ->
            Exception
    end.

%%

encode_webhook_params(PartyID, #{<<"scope">> := Scope, <<"url">> := URL}) ->
    #webhooker_WebhookParams{
        party_id = PartyID,
        url = URL,
        event_filter = encode_webhook_scope(Scope)
    }.

encode_webhook_scope(#{<<"topic">> := <<"InvoicesTopic">>, <<"shopID">> := ShopID, <<"eventTypes">> := EventTypes}) ->
    {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ShopID,
        types = ordsets:from_list([encode_invoice_event_type(V) || V <- EventTypes])
    }};
encode_webhook_scope(#{<<"topic">> := <<"CustomersTopic">>, <<"shopID">> := ShopID, <<"eventTypes">> := EventTypes}) ->
    {customer, #webhooker_CustomerEventFilter{
        shop_id = ShopID,
        types = ordsets:from_list([encode_customer_event_type(V) || V <- EventTypes])
    }}.

-define(invpaid(), {paid, #webhooker_InvoicePaid{}}).
-define(invcancelled(), {cancelled, #webhooker_InvoiceCancelled{}}).
-define(invfulfilled(), {fulfilled, #webhooker_InvoiceFulfilled{}}).

-define(pmtprocessed(), {processed, #webhooker_InvoicePaymentProcessed{}}).
-define(pmtcaptured(), {captured, #webhooker_InvoicePaymentCaptured{}}).
-define(pmtcancelled(), {cancelled, #webhooker_InvoicePaymentCancelled{}}).
-define(pmtrefunded(), {refunded, #webhooker_InvoicePaymentRefunded{}}).
-define(pmtfailed(), {failed, #webhooker_InvoicePaymentFailed{}}).

-define(pmtrfndcreated(), {invoice_payment_refund_created, #webhooker_InvoicePaymentRefundCreated{}}).
-define(pmtrfndstatus(Value),
    {
        invoice_payment_refund_status_changed,
        #webhooker_InvoicePaymentRefundStatusChanged{value = Value}
    }
).

-define(pmtrfndfailed(), {failed, #webhooker_InvoicePaymentRefundFailed{}}).
-define(pmtrfndsucceeded(), {succeeded, #webhooker_InvoicePaymentRefundSucceeded{}}).

encode_invoice_event_type(<<"InvoiceCreated">>) ->
    {created, #webhooker_InvoiceCreated{}};
encode_invoice_event_type(<<"InvoicePaid">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invpaid()}};
encode_invoice_event_type(<<"InvoiceCancelled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invcancelled()}};
encode_invoice_event_type(<<"InvoiceFulfilled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invfulfilled()}};
encode_invoice_event_type(<<"PaymentStarted">>) ->
    {payment, {created, #webhooker_InvoicePaymentCreated{}}};
encode_invoice_event_type(<<"PaymentProcessed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtprocessed()}}};
encode_invoice_event_type(<<"PaymentCaptured">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcaptured()}}};
encode_invoice_event_type(<<"PaymentCancelled">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcancelled()}}};
encode_invoice_event_type(<<"PaymentRefunded">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtrefunded()}}};
encode_invoice_event_type(<<"PaymentFailed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtfailed()}}};
encode_invoice_event_type(<<"PaymentRefundCreated">>) ->
    {payment, {invoice_payment_refund_change, ?pmtrfndcreated()}};
encode_invoice_event_type(<<"PaymentRefundFailed">>) ->
    {payment, {invoice_payment_refund_change, ?pmtrfndstatus(?pmtrfndfailed())}};
encode_invoice_event_type(<<"PaymentRefundSucceeded">>) ->
    {payment, {invoice_payment_refund_change, ?pmtrfndstatus(?pmtrfndsucceeded())}}.

encode_customer_event_type(<<"CustomerCreated">>) ->
    {created, #webhooker_CustomerCreated{}};
encode_customer_event_type(<<"CustomerDeleted">>) ->
    {deleted, #webhooker_CustomerDeleted{}};
encode_customer_event_type(<<"CustomerReady">>) ->
    {ready, #webhooker_CustomerStatusReady{}};
encode_customer_event_type(<<"CustomerBindingStarted">>) ->
    {binding, {started, #webhooker_CustomerBindingStarted{}}};
encode_customer_event_type(<<"CustomerBindingSucceeded">>) ->
    {binding, {succeeded, #webhooker_CustomerBindingSucceeded{}}};
encode_customer_event_type(<<"CustomerBindingFailed">>) ->
    {binding, {failed, #webhooker_CustomerBindingFailed{}}}.

%%

decode_webhook(Hook) ->
    #{
        <<"id">> => integer_to_binary(Hook#webhooker_Webhook.id),
        <<"active">> => Hook#webhooker_Webhook.enabled,
        <<"scope">> => decode_event_filter(Hook#webhooker_Webhook.event_filter),
        <<"url">> => Hook#webhooker_Webhook.url,
        <<"publicKey">> => Hook#webhooker_Webhook.pub_key
    }.

decode_event_filter({invoice, #webhooker_InvoiceEventFilter{shop_id = ShopID, types = EventTypes}}) ->
    genlib_map:compact(#{
        <<"topic">> => <<"InvoicesTopic">>,
        <<"shopID">> => ShopID,
        <<"eventTypes">> => lists:flatmap(fun decode_invoice_event_type/1, ordsets:to_list(EventTypes))
    });
decode_event_filter({customer, #webhooker_CustomerEventFilter{shop_id = ShopID, types = EventTypes}}) ->
    genlib_map:compact(#{
        <<"topic">> => <<"CustomersTopic">>,
        <<"shopID">> => ShopID,
        <<"eventTypes">> => lists:map(fun decode_customer_event_type/1, ordsets:to_list(EventTypes))
    }).

decode_invoice_event_type({created, #webhooker_InvoiceCreated{}}) ->
    [<<"InvoiceCreated">>];
decode_invoice_event_type({status_changed, #webhooker_InvoiceStatusChanged{value = undefined}}) ->
    % TODO seems unmaintainable
    [
        decode_invoice_status_event_type(V)
        || V <- [
               ?invpaid(),
               ?invcancelled(),
               ?invfulfilled()
           ]
    ];
decode_invoice_event_type({status_changed, #webhooker_InvoiceStatusChanged{value = Value}}) ->
    [decode_invoice_status_event_type(Value)];
decode_invoice_event_type({payment, {created, #webhooker_InvoicePaymentCreated{}}}) ->
    [<<"PaymentStarted">>];
decode_invoice_event_type({payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = undefined}}}) ->
    % TODO seems unmaintainable
    [
        decode_payment_status_event_type(V)
        || V <- [
               ?pmtprocessed(),
               ?pmtcaptured(),
               ?pmtcancelled(),
               ?pmtrefunded(),
               ?pmtfailed()
           ]
    ];
decode_invoice_event_type({payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = Value}}}) ->
    [decode_payment_status_event_type(Value)];
decode_invoice_event_type({payment, {invoice_payment_refund_change, ?pmtrfndcreated()}}) ->
    [<<"PaymentRefundCreated">>];
decode_invoice_event_type({payment, {invoice_payment_refund_change, ?pmtrfndstatus(Value)}}) ->
    [decode_payment_refund_status_event_type(Value)].

decode_invoice_status_event_type(?invpaid()) -> <<"InvoicePaid">>;
decode_invoice_status_event_type(?invcancelled()) -> <<"InvoiceCancelled">>;
decode_invoice_status_event_type(?invfulfilled()) -> <<"InvoiceFulfilled">>.

decode_payment_status_event_type(?pmtprocessed()) -> <<"PaymentProcessed">>;
decode_payment_status_event_type(?pmtcaptured()) -> <<"PaymentCaptured">>;
decode_payment_status_event_type(?pmtcancelled()) -> <<"PaymentCancelled">>;
decode_payment_status_event_type(?pmtrefunded()) -> <<"PaymentRefunded">>;
decode_payment_status_event_type(?pmtfailed()) -> <<"PaymentFailed">>.

decode_payment_refund_status_event_type(?pmtrfndfailed()) -> <<"PaymentRefundFailed">>;
decode_payment_refund_status_event_type(?pmtrfndsucceeded()) -> <<"PaymentRefundSucceeded">>.

decode_customer_event_type({created, #webhooker_CustomerCreated{}}) ->
    <<"CustomerCreated">>;
decode_customer_event_type({deleted, #webhooker_CustomerDeleted{}}) ->
    <<"CustomerDeleted">>;
decode_customer_event_type({ready, #webhooker_CustomerStatusReady{}}) ->
    <<"CustomerReady">>;
decode_customer_event_type({binding, {started, #webhooker_CustomerBindingStarted{}}}) ->
    <<"CustomerBindingStarted">>;
decode_customer_event_type({binding, {succeeded, #webhooker_CustomerBindingSucceeded{}}}) ->
    <<"CustomerBindingSucceeded">>;
decode_customer_event_type({binding, {failed, #webhooker_CustomerBindingFailed{}}}) ->
    <<"CustomerBindingFailed">>.
