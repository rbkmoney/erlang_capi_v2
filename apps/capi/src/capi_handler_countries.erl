-module(capi_handler_countries).

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
prepare(OperationID, Req, Context) when OperationID =:= 'GetCountries'; OperationID =:= 'GetCountryByID' ->
    Authorize = fun() -> {ok, capi_auth:authorize_operation([], Context, Req)} end,
    Process = fun() -> process_request(OperationID, Req, Context) end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

process_request('GetCountries', _Req, #{woody_context := WoodyContext}) ->
    Countries = unwrap(capi_domain:get_objects_by_type(WoodyContext, country)),
    {ok, {200, #{}, lists:map(fun decode_country_object/1, Countries)}};
process_request('GetCountryByID', Req, #{woody_context := WoodyContext}) ->
    CountryCode = capi_coder_utils:encode_country_code(maps:get(countryID, Req)),
    Ref = {country, #domain_CountryRef{id = CountryCode}},
    case capi_domain:get(Ref, WoodyContext) of
        {ok, CountryObject} ->
            {ok, {200, #{}, decode_country_object(CountryObject)}};
        {error, not_found} ->
            {ok, general_error(404, <<"Country not found">>)}
    end.

decode_country_object(#domain_CountryObject{ref = Ref, data = Data}) ->
    ID = capi_coder_utils:decode_country_code(Ref#domain_CountryRef.id),
    #domain_Country{name = Name, trade_blocs = TradeBlocRefsThrift} = Data,
    TradeBlocRefs = decode_trade_bloc_refs(TradeBlocRefsThrift),
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"name">> => Name,
        <<"tradeBlocs">> => TradeBlocRefs
    }).

decode_trade_bloc_refs(undefined) ->
    undefined;
decode_trade_bloc_refs(TradeBlocRefs) ->
    lists:map(
        fun(#domain_TradeBlocRef{id = Id}) ->
            Id
        end,
        ordsets:to_list(TradeBlocRefs)
    ).
