-module(capi_handler_trade_blocs).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).
-import(capi_utils, [unwrap/1]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.

prepare(OperationID, Req, Context) when OperationID =:= 'GetTradeBlocs'; OperationID =:= 'GetTradeBlocByID' ->
    Authorize = fun() -> {ok, capi_auth:authorize_operation([], Context)} end,
    Process = fun() -> process_request(OperationID, Req, Context) end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

process_request('GetTradeBlocs', _Req, #{woody_context := WoodyContext}) ->
    Countries = unwrap(capi_domain:get_objects_by_type(WoodyContext, trade_bloc)),
    {ok, {200, #{}, lists:map(fun decode_trade_bloc_object/1, Countries)}};
process_request('GetTradeBlocByID', Req, #{woody_context := WoodyContext}) ->
    Ref = {trade_bloc, #domain_TradeBlocRef{id = maps:get('tradeBlocID', Req)}},
    case capi_domain:get(Ref, WoodyContext) of
        {ok, TradeBlocObject} ->
            {ok, {200, #{}, decode_trade_bloc_object(TradeBlocObject)}};
        {error, not_found} ->
            {ok, general_error(404, <<"Trade Bloc not found">>)}
    end.

decode_trade_bloc_object(#domain_TradeBlocObject{ref = Ref, data = Data}) ->
    #domain_TradeBlocRef{id = ID} = Ref,
    #domain_TradeBloc{name = Name, description = Description} = Data,
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"name">> => Name,
        <<"description">> => Description
    }).
