-module(capi_handler_analytics).

-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetPaymentConversionStats', Req, Context, _) ->
    process_merchant_stat(payments_conversion_stat, Req, Context);

process_request('GetPaymentRevenueStats', Req, Context, _) ->
    process_merchant_stat(payments_turnover, Req, Context);

process_request('GetPaymentGeoStats', Req, Context, _) ->
    process_merchant_stat(payments_geo_stat, Req, Context);

process_request('GetPaymentRateStats', Req, Context, _) ->
    process_merchant_stat(customers_rate_stat, Req, Context);

process_request('GetPaymentMethodStats', Req, Context, _) ->
    bankCard =  maps:get(paymentMethod, Req),
    StatType = payments_pmt_cards_stat,
    process_merchant_stat(StatType, Req, Context);

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).

create_stat_dsl(StatType, Req, Context) ->
    FromTime = capi_handler_utils:get_time('fromTime', Req),
    ToTime   = capi_handler_utils:get_time('toTime'  , Req),
    SplitInterval =
        case StatType of
            customers_rate_stat ->
                capi_handler_utils:get_time_diff(FromTime, ToTime);
            _ ->
                SplitUnit = genlib_map:get('splitUnit', Req),
                SplitSize = genlib_map:get('splitSize', Req),
                capi_handler_utils:get_split_interval(SplitSize, SplitUnit)
        end,
    Query = #{
        <<"merchant_id"   >> => capi_handler_utils:get_party_id(Context),
        <<"shop_id"       >> => genlib_map:get('shopID', Req),
        <<"from_time"     >> => FromTime,
        <<"to_time"       >> => ToTime,
        <<"split_interval">> => SplitInterval
    },
    create_dsl(StatType, Query, #{}).

create_dsl(QueryType, QueryBody, QueryParams) ->
    capi_handler_utils:merge_and_compact(
        #{<<"query">> => maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{})},
        QueryParams
    ).

process_merchant_stat(StatType, Req, Context) ->
    Call = {merchant_stat, 'GetStatistics', [capi_handler:encode_stat_request(create_stat_dsl(StatType, Req, Context))]},
    process_merchant_stat_result(StatType, capi_handler_utils:service_call(Call, Context)).

process_merchant_stat_result(customers_rate_stat = StatType, {ok, #merchstat_StatResponse{data = {records, Stats}}}) ->
    Resp =
        case Stats of
            [            ] -> #{<<"uniqueCount">> => 0};
            [StatResponse] -> decode_stat_info(StatType, StatResponse)
        end,
    {ok, {200, [], Resp}};

process_merchant_stat_result(StatType, Result) ->
    case Result of
        {ok, #merchstat_StatResponse{data = {'records', Stats}}} ->
            Resp = [decode_stat_info(StatType, S) || S <- Stats],
            {ok, {200, [], Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
            {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
        {exception, #merchstat_BadToken{}} ->
            {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Invalid token">>)}}
    end.

decode_stat_info(payments_conversion_stat, Response) ->
    #{
        <<"offset"         >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"successfulCount">> => genlib:to_int(maps:get(<<"successful_count">>, Response)),
        <<"totalCount"     >> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"conversion"     >> => genlib:to_float(maps:get(<<"conversion">>, Response))
    };

decode_stat_info(payments_geo_stat, Response) ->
    #{
        <<"offset"  >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"geoID"   >> => genlib:to_int(maps:get(<<"city_id">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit"  >> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue" >> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_info(payments_turnover, Response) ->
    #{
        <<"offset"  >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit"  >> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue" >> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat_info(customers_rate_stat, Response) ->
    #{
        <<"uniqueCount">> => genlib:to_int(maps:get(<<"unic_count">>, Response))
    };

decode_stat_info(payments_pmt_cards_stat, Response) ->
    #{
        <<"statType"     >> => <<"PaymentMethodBankCardStat">>, %% @TODO deal with nested responses decoding
        <<"offset"       >> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"totalCount"   >> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"paymentSystem">> => maps:get(<<"payment_system">>, Response),
        <<"profit"       >> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue"      >> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    }.
