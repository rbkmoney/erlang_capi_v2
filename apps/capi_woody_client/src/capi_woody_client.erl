-module(capi_woody_client).

-export([call_service/4]).

-export([get_service_modname/1]).

%%

-type service_name() :: atom().
-type client_opts() :: #{
    url            := woody:url(),
    %% See hackney:request/5 for available transport options.
    transport_opts => woody_client_thrift_http_transport:transport_options()
}.

-spec call_service(service_name(), woody:func(), [term()], woody_context:ctx()) ->
    woody:result().

call_service(ServiceName, Function, Args, Context0) ->
    Deadline = get_service_deadline(ServiceName),
    Context1 = set_deadline(Deadline, Context0),
    Retry = get_service_retry(ServiceName, Function),
    EventHandler = scoper_woody_event_handler,
    call_service(ServiceName, Function, Args, Context1, EventHandler, Retry).

call_service(ServiceName, Function, Args, Context, EventHandler, Retry) ->
    Options = get_service_options(ServiceName),
    Service = get_service_modname(ServiceName),
    Request = {Service, Function, Args},
    try
        woody_client:call(
            Request,
            Options#{event_handler => EventHandler},
            Context
        )
    catch
        error:{woody_error, {_Source, Class, _Details}} = Error
        when Class =:= resource_unavailable orelse Class =:= result_unknown
        ->
            NextRetry = apply_retry_strategy(Retry, Error, Context),
            call_service(ServiceName, Function, Args, Context, EventHandler, NextRetry)
    end.

-spec get_service_options(service_name()) ->
    client_opts().

get_service_options(ServiceName) ->
    construct_opts(maps:get(ServiceName, genlib_app:env(?MODULE, services))).

construct_opts(Opts = #{url := Url}) ->
    Opts#{url := genlib:to_binary(Url)};
construct_opts(Url) ->
    #{url => genlib:to_binary(Url)}.

apply_retry_strategy(Retry, Error, Context) ->
    apply_retry_step(genlib_retry:next_step(Retry), woody_context:get_deadline(Context), Error).

apply_retry_step(finish, _, Error) ->
    erlang:error(Error);
apply_retry_step({wait, Timeout, Retry}, undefined, _) ->
    ok = timer:sleep(Timeout),
    Retry;
apply_retry_step({wait, Timeout, Retry}, Deadline0, Error) ->
    Deadline1 = woody_deadline:from_unixtime_ms(
        woody_deadline:to_unixtime_ms(Deadline0) - Timeout
    ),
    case woody_deadline:is_reached(Deadline1) of
        true ->
            % no more time for retries
            erlang:error(Error);
        false ->
            ok = timer:sleep(Timeout),
            Retry
    end.

-spec get_service_modname(service_name()) -> woody:service().

get_service_modname(invoicing) ->
    {dmsl_payment_processing_thrift, 'Invoicing'};
get_service_modname(invoice_templating) ->
    {dmsl_payment_processing_thrift, 'InvoiceTemplating'};
get_service_modname(merchant_stat) ->
    {dmsl_merch_stat_thrift, 'MerchantStatistics'};
get_service_modname(reporting) ->
    {reporter_reports_thrift, 'Reporting'};
get_service_modname(payouts) ->
    {dmsl_payout_processing_thrift, 'PayoutManagement'};
get_service_modname(accounter) ->
    {dmsl_accounter_thrift, 'Accounter'};
get_service_modname(geo_ip_service) ->
    {dmsl_geo_ip_thrift, 'GeoIpService'};
get_service_modname(webhook_manager) ->
    {dmsl_webhooker_thrift, 'WebhookManager'};
get_service_modname(customer_management) ->
    {dmsl_payment_processing_thrift, 'CustomerManagement'};
get_service_modname(party_management) ->
    {dmsl_payment_processing_thrift, 'PartyManagement'};
get_service_modname(bender) ->
    {bender_thrift, 'Bender'};
get_service_modname(generator) ->
    {bender_thrift, 'Generator'}.


get_service_deadline(ServiceName) ->
    ServiceDeadlines = genlib_app:env(?MODULE, service_deadlines, #{}),
    case maps:get(ServiceName, ServiceDeadlines, undefined) of
        Timeout when is_integer(Timeout) andalso Timeout >= 0 ->
            woody_deadline:from_timeout(Timeout);
        undefined ->
            undefined
    end.

set_deadline(Deadline, Context) ->
    case woody_context:get_deadline(Context) of
        undefined ->
            woody_context:set_deadline(Deadline, Context);
        _AlreadySet ->
            Context
    end.

get_service_retry(ServiceName, Function) ->
    ServiceRetries = genlib_app:env(?MODULE, service_retries, #{}),
    FunctionReties = maps:get(ServiceName, ServiceRetries, #{}),
    DefaultRetry = maps:get('_', FunctionReties, finish),
    maps:get(Function, FunctionReties, DefaultRetry).
