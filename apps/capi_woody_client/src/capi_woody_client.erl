-module(capi_woody_client).

-export([call_service/4]).
-export([call_service/5]).

-export([get_service_modname/1]).

%%

-type service_name() :: atom().

-spec call_service(service_name(), woody:func(), [term()], woody_context:ctx()) ->
    woody:result().

call_service(ServiceName, Function, Args, Context) ->
    call_service(ServiceName, Function, Args, Context, capi_woody_event_handler).

-spec call_service(service_name(), woody:func(), [term()], woody_context:ctx(), woody:ev_handler()) ->
    woody:result().

call_service(ServiceName, Function, Args, Context, EventHandler) ->
    {Url, Service} = get_service_spec(ServiceName),
    Request = {Service, Function, Args},
    woody_client:call(Request, #{url => Url, event_handler => EventHandler}, Context).

get_service_spec(ServiceName) ->
    {get_service_url(ServiceName), get_service_modname(ServiceName)}.

get_service_url(ServiceName) ->
    maps:get(ServiceName, genlib_app:env(?MODULE, service_urls)).

-spec get_service_modname(service_name()) -> woody:service().

get_service_modname(invoicing) ->
    {dmsl_payment_processing_thrift, 'Invoicing'};
get_service_modname(invoice_templating) ->
    {dmsl_payment_processing_thrift, 'InvoiceTemplating'};
get_service_modname(cds_storage) ->
    {dmsl_cds_thrift, 'Storage'};
get_service_modname(merchant_stat) ->
    {dmsl_merch_stat_thrift, 'MerchantStatistics'};
get_service_modname(reporting) ->
    {dmsl_reporting_thrift, 'Reporting'};
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
get_service_modname(payment_tool_provider) ->
    {dmsl_payment_tool_provider_thrift, 'PaymentToolProvider'}.
