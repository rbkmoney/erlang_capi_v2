-module(cp_proto).

-export([call_service_safe/4]).

%%

-type service_name() :: atom().

-spec call_service_safe(service_name(), woody_t:func(), [term()], woody_client:context()) ->
    {woody_client:result_ok() | woody_client:result_error(), woody_client:context()}.

call_service_safe(ServiceName, Function, Args, Context) ->
    {Url, Service} = get_service_spec(ServiceName),
    woody_client:call_safe(Context, {Service, Function, Args}, #{url => Url}).

get_service_spec(ServiceName) ->
    {get_service_url(ServiceName), get_service_modname(ServiceName)}.

get_service_url(ServiceName) ->
    maps:get(ServiceName, genlib_app:env(?MODULE, service_urls)).

get_service_modname(invoicing) ->
    {cp_payment_processing_thrift, 'Invoicing'};
get_service_modname(cds_storage) ->
    {cp_cds_thrift, 'Storage'};
get_service_modname(merchant_stat) ->
    {cp_merch_stat_thrift, 'MerchantStatistics'}.
