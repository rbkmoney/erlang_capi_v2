-module(capi_handler_webhooks).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('CreateWebhook', Req, Context, _) ->
    WebhookParams = encode_webhook_params(get_party_id(Context), maps:get('Webhook', Req)),
    ShopID = validate_webhook_params(WebhookParams),
    Call = {party_management, 'GetShop', [ShopID]},
    case service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _} ->
            Webhook = capi_utils:unwrap(service_call({webhook_manager, 'Create', [WebhookParams]}, Context)),
            {ok, {201, [], decode_webhook(Webhook)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}}
    end;

process_request('GetWebhooks', _Req, Context, _) ->
    Webhooks = capi_utils:unwrap(service_call_with([party_id], {webhook_manager, 'GetList', []}, Context)),
    {ok, {200, [], [decode_webhook(V) || V <- Webhooks]}};

process_request('GetWebhookByID', Req, Context, _) ->
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case get_webhook(WebhookID, Context) of
                {ok, Webhook} ->
                    {ok, {200, [], decode_webhook(Webhook)}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {404, [], general_error(<<"Webhook not found">>)}}
            end;
        error ->
            {ok, {404, [], general_error(<<"Webhook not found">>)}}
    end;

process_request('DeleteWebhookByID', Req, Context, _) ->
    case encode_webhook_id(maps:get(webhookID, Req)) of
        {ok, WebhookID} ->
            case delete_webhook(WebhookID, Context) of
                {ok, _} ->
                    {ok, {204, [], undefined}};
                {exception, #webhooker_WebhookNotFound{}} ->
                    {ok, {204, [], undefined}}
            end;
        error ->
            {ok, {404, [], general_error(<<"Webhook not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
