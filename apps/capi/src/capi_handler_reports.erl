-module(capi_handler_reports).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('GetReports', Req, Context, _) ->
    ReportRequest = #reports_ReportRequest{
        party_id   = get_party_id(Context),
        shop_id    = maps:get(shopID, Req),
        time_range =
            #reports_ReportTimeRange{
                from_time = get_time('fromTime', Req),
                to_time   = get_time('toTime'  , Req)
            }
    },
    ReportTypes = [],
    Call = {reporting, 'GetReports', [ReportRequest, ReportTypes]},
    case service_call(Call, Context) of
        {ok, Reports} ->
            {ok, {200, [], [decode_report(R) || R <- Reports]}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, {400, [], limit_exceeded_error(Limit)}}
            end
    end;

process_request('GetReport', Req, Context, _) ->
    PartyId  = get_party_id(Context),
    ShopId   = maps:get(shopID, Req),
    ReportId = maps:get(reportID, Req),
    Call = {reporting, 'GetReport', [PartyId, ShopId, ReportId]},
    case service_call(Call, Context) of
        {ok, Report} ->
            {ok, {200, [], decode_report(Report)}};
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], general_error(<<"Report not found">>)}}
    end;

process_request('CreateReport', Req, Context, _) ->
    PartyId = get_party_id(Context),
    ShopId = maps:get(shopID, Req),
    ReportParams = maps:get('ReportParams', Req),
    ReportRequest = #reports_ReportRequest{
        party_id   = PartyId,
        shop_id    = ShopId,
        time_range =
            #reports_ReportTimeRange{
                from_time = get_time(<<"fromTime">>, ReportParams),
                to_time   = get_time(<<"toTime">>  , ReportParams)
            }
    },
    ReportType = encode_report_type(maps:get(<<"reportType">>, ReportParams)),
    case service_call({reporting, 'GenerateReport', [ReportRequest, ReportType]}, Context) of
        {ok, ReportId} ->
            {ok, Report} = service_call({reporting, 'GetReport', [PartyId, ShopId, ReportId]}, Context),
            {ok, {201, [], decode_report(Report)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
                #reports_ShopNotFound{} ->
                    {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}}
            end
    end;

process_request('DownloadFile', Req, Context, _) ->
    Call = {reporting, 'GetReport', [get_party_id(Context), maps:get(shopID, Req), maps:get(reportID, Req)]},
    case service_call(Call, Context) of
        {ok, #reports_Report{status = created, files = Files}} ->
            FileID = maps:get(fileID, Req),
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, Context);
                false ->
                    {ok, {404, [], general_error(<<"File not found">>)}}
            end;
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], general_error(<<"Report not found">>)}}
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
