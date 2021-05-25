-module(capi_handler_geo).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('GetLocationsNames' = OperationID, Req, Context) ->
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID}}],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        GeoIDs = ordsets:from_list(maps:get('geoIDs', Req)),
        Language = maps:get('language', Req),
        Call = {geo_ip_service, 'GetLocationName', {GeoIDs, Language}},
        case capi_handler_call:service_call(Call, Context) of
            {ok, LocationNames} ->
                PreparedLocationNames = maps:fold(
                    fun(GeoID, Name, Acc) -> [decode_location_name(GeoID, Name) | Acc] end,
                    [],
                    LocationNames
                ),
                {ok, {200, #{}, PreparedLocationNames}};
            {exception, #'InvalidRequest'{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error(invalidRequest, FormattedErrors)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

decode_location_name(GeoID, Name) ->
    #{
        <<"geoID">> => GeoID,
        <<"name">> => Name
    }.
