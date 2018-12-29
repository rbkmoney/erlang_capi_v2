-module(capi_handler_geo).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

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
            {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}}
    end;
%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

decode_location_name(GeoID, Name) ->
    #{
        <<"geoID">> => GeoID,
        <<"name">> => Name
    }.
