-module(capi_client_webhooks).

-export([get_webhooks/1]).
-export([create_webhook/2]).
-export([get_webhook_by_id/2]).
-export([delete_webhook_by_id/2]).

-type webhook_id()     :: binary().
-type webhook_params() :: map().
-type webhook()        :: map().
-type context()        :: capi_client_lib:context().

-spec get_webhooks(context()) -> [webhook()].
get_webhooks(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_webhooks_api:get_webhooks(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_webhook(context(), webhook_params()) -> {ok, webhook()} | {error, term()}.
create_webhook(Context, Params) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{body => Params}),
    Response = swag_client_webhooks_api:create_webhook(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_webhook_by_id(context(), webhook_id()) -> {ok, webhook()} | {error, term()}.
get_webhook_by_id(Context, WebhookID) ->
    Params = #{binding => #{<<"webhookID">> => WebhookID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_webhooks_api:get_webhook_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec delete_webhook_by_id(context(), webhook_id()) -> ok | {error, term()}.
delete_webhook_by_id(Context, WebhookID) ->
    Params = #{binding => #{<<"webhookID">> => WebhookID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_webhooks_api:delete_webhook_by_id(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.
