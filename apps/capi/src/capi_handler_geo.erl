-module(capi_handler_geo).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetLocationsNames', Req, Context, _) ->
    CallArgs = [ordsets:from_list(maps:get('geoIDs', Req)), maps:get('language', Req)],
    Call = {geo_ip_service, 'GetLocationName', CallArgs},
    case service_call(Call, Context) of
        {ok, LocationNames = #{}} ->
            PreparedLocationNames =
                maps:fold(
                    fun(GeoID, Name, Acc) -> [decode_location_name(GeoID, Name) | Acc] end,
                    [],
                    LocationNames
                ),
            {ok, {200, [], PreparedLocationNames}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}}
    end;
%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
