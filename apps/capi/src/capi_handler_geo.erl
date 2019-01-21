-module(capi_handler_geo).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [logic_error/3]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('GetLocationsNames', Req, Context) ->
    CallArgs = [ordsets:from_list(maps:get('geoIDs', Req)), maps:get('language', Req)],
    Call = {geo_ip_service, 'GetLocationName', CallArgs},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, LocationNames = #{}} ->
            PreparedLocationNames =
                maps:fold(
                    fun(GeoID, Name, Acc) -> [decode_location_name(GeoID, Name) | Acc] end,
                    [],
                    LocationNames
                ),
            {ok, {200, [], PreparedLocationNames}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
            {ok, logic_error(400, invalidRequest, FormattedErrors)}
    end;
%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_location_name(GeoID, Name) ->
    #{
        <<"geoID">> => GeoID,
        <<"name">> => Name
    }.
