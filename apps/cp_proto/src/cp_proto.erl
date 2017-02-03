-module(cp_proto).

-export([call_service/5]).

%%

-type service_name() :: atom().

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

get_service_modname(invoicing) ->
    {cp_payment_processing_thrift, 'Invoicing'};
get_service_modname(cds_storage) ->
    {cp_cds_thrift, 'Storage'};
get_service_modname(merchant_stat) ->
    {cp_merch_stat_thrift, 'MerchantStatistics'};
get_service_modname(repository) ->
    {cp_domain_config_thrift, 'Repository'};
get_service_modname(accounter) ->
    {cp_accounter_thrift, 'Accounter'};
get_service_modname(geo_ip_service) ->
    {cp_geo_ip_thrift, 'GeoIpService'};
get_service_modname(merchant_config) ->
    {cp_proxy_merch_config_thrift, 'ConfigureMerchantProxy'};
get_service_modname(party_management) ->
    {cp_payment_processing_thrift, 'PartyManagement'}.
