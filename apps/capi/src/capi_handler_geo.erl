-module(capi_handler_geo).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare_request/3]).
-export([process_request/3]).
-export([authorize_request/3]).

-import(capi_handler_utils, [logic_error/2]).

-spec prepare_request(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
prepare_request(OperationID, _Req, _Context) when OperationID =:= 'GetLocationsNames' ->
    {ok, #{}};
prepare_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec authorize_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> {ok, capi_handler:request_state()} | {done, capi_handler:request_response()} | {error, noimpl}.
authorize_request(OperationID, Context, ReqState) when OperationID =:= 'GetLocationsNames' ->
    Resolution = capi_auth:authorize_operation(OperationID, [], Context, ReqState),
    {ok, ReqState#{resolution => Resolution}};
authorize_request(_OperationID, _Context, _ReqState) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> capi_handler:request_response() | {error, noimpl}.
process_request('GetLocationsNames', Context, #{data := Req}) ->
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
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_location_name(GeoID, Name) ->
    #{
        <<"geoID">> => GeoID,
        <<"name">> => Name
    }.
