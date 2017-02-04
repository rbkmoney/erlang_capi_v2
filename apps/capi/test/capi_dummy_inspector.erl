-module(capi_dummy_inspector).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(capi_test_proxy).

-export([get_service_spec/0]).


-include_lib("cp_proto/include/cp_proxy_inspector_thrift.hrl").

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/inspector/dummy", {cp_proxy_inspector_thrift, 'InspectorProxy'}}.


-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), #{}) ->
    {ok, term()}.

handle_function(
    'InspectPayment',
    [#proxy_inspector_Context{
        payment = _PaymentInfo,
        options = #{
            <<"risk_score">> := RiskScore
        }
    }],
    _Context,
    _Options
) ->
    {ok, binary_to_atom(RiskScore, utf8)}.
