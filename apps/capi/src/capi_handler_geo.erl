-module(capi_handler_geo).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare(OperationID, Req, Context) when OperationID =:= 'GetLocationsNames' ->
    Authorize = fun() -> {ok, capi_auth:authorize_operation(OperationID, [], Context, Req)} end,
    Process = fun() -> process_request(OperationID, Context, Req) end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetLocationsNames', Context, Req) ->
    CallArgs = {ordsets:from_list(maps:get('geoIDs', Req)), maps:get('language', Req)},
    Call = {geo_ip_service, 'GetLocationName', CallArgs},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, LocationNames = #{}} ->
            PreparedLocationNames =
                maps:fold(
                    fun(GeoID, Name, Acc) -> [decode_location_name(GeoID, Name) | Acc] end,
                    [],
                    LocationNames
                ),
            {ok, {200, #{}, PreparedLocationNames}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
            {ok, logic_error(invalidRequest, FormattedErrors)}
    end.

%%

decode_location_name(GeoID, Name) ->
    #{
        <<"geoID">> => GeoID,
        <<"name">> => Name
    }.
