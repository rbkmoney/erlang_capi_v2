-module(capi_handler_reports).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).

-define(DEFAULT_URL_LIFETIME, 60). % seconds

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('GetReports', Req, Context) ->
    ReportRequest = #reports_ReportRequest{
        party_id   = capi_handler_utils:get_party_id(Context),
        shop_id    = maps:get(shopID, Req),
        time_range =
            #reports_ReportTimeRange{
                from_time = capi_handler_utils:get_time('fromTime', Req),
                to_time   = capi_handler_utils:get_time('toTime'  , Req)
            }
    },
    ReportTypes = [],
    Call = {reporting, 'GetReports', [ReportRequest, ReportTypes]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, Reports} ->
            {ok, {200, [], [decode_report(R) || R <- Reports]}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, {400, [], limit_exceeded_error(Limit)}}
            end
    end;

process_request('GetReport', Req, Context) ->
    PartyId  = capi_handler_utils:get_party_id(Context),
    ShopId   = maps:get(shopID, Req),
    ReportId = maps:get(reportID, Req),
    Call = {reporting, 'GetReport', [PartyId, ShopId, ReportId]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, Report} ->
            {ok, {200, [], decode_report(Report)}};
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Report not found">>)}}
    end;

process_request('CreateReport', Req, Context) ->
    PartyId = capi_handler_utils:get_party_id(Context),
    ShopId = maps:get(shopID, Req),
    ReportParams = maps:get('ReportParams', Req),
    ReportRequest = #reports_ReportRequest{
        party_id   = PartyId,
        shop_id    = ShopId,
        time_range =
            #reports_ReportTimeRange{
                from_time = capi_handler_utils:get_time(<<"fromTime">>, ReportParams),
                to_time   = capi_handler_utils:get_time(<<"toTime">>  , ReportParams)
            }
    },
    ReportType = encode_report_type(maps:get(<<"reportType">>, ReportParams)),
    case capi_handler_utils:service_call({reporting, 'GenerateReport', [ReportRequest, ReportType]}, Context) of
        {ok, ReportId} ->
            {ok, Report} = capi_handler_utils:service_call(
                {reporting, 'GetReport', [PartyId, ShopId, ReportId]},
                Context
            ),
            {ok, {201, [], decode_report(Report)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
                #reports_ShopNotFound{} ->
                    {ok, {400, [], capi_handler_utils:logic_error(invalidShopID, <<"Shop not found">>)}}
            end
    end;

process_request('DownloadFile', Req, Context) ->
    Call = {
        reporting,
        'GetReport',
        [capi_handler_utils:get_party_id(Context), maps:get(shopID, Req), maps:get(reportID, Req)]
    },
    case capi_handler_utils:service_call(Call, Context) of
        {ok, #reports_Report{status = created, files = Files}} ->
            FileID = maps:get(fileID, Req),
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, Context);
                false ->
                    {ok, {404, [], capi_handler_utils:general_error(<<"File not found">>)}}
            end;
        {exception, #reports_ReportNotFound{}} ->
            {ok, {404, [], capi_handler_utils:general_error(<<"Report not found">>)}}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

limit_exceeded_error(Limit) ->
    capi_handler_utils:logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit])).

generate_report_presigned_url(FileID, Context) ->
    ExpiresAt = get_default_url_lifetime(),
    Call = {reporting, 'GeneratePresignedUrl', [FileID, ExpiresAt]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, URL} ->
            {ok, {200, [], #{<<"url">> => URL}}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, {400, [], capi_handler_utils:logic_error(invalidRequest, FormattedErrors)}};
                #reports_FileNotFound{}->
                    {ok, {404, [], capi_handler_utils:general_error(<<"File not found">>)}}
            end
    end.

get_default_url_lifetime() ->
    Now      = erlang:system_time(second),
    Lifetime = application:get_env(capi, reporter_url_lifetime, ?DEFAULT_URL_LIFETIME),
    capi_utils:unwrap(rfc3339:format(Now + Lifetime, second)).

%%

encode_report_type(<<"provisionOfService">>) -> provision_of_service;
encode_report_type(<<"paymentRegistry">>) -> payment_registry.

%%

decode_report(Report) ->
    #reports_ReportTimeRange{from_time = FromTime, to_time = ToTime} = Report#reports_Report.time_range,
    #{
        <<"id"       >> => Report#reports_Report.report_id,
        <<"createdAt">> => Report#reports_Report.created_at,
        <<"fromTime" >> => FromTime,
        <<"toTime"   >> => ToTime,
        <<"status"   >> => decode_report_status(Report#reports_Report.status),
        <<"type"     >> => decode_report_type(Report#reports_Report.report_type),
        <<"files"    >> => [decode_report_file(F) || F <- Report#reports_Report.files]
    }.

decode_report_status(pending) -> <<"pending">>;
decode_report_status(created) -> <<"created">>.

decode_report_type(provision_of_service) -> <<"provisionOfService">>;
decode_report_type(payment_registry) -> <<"paymentRegistry">>.

decode_report_file(#reports_FileMeta{file_id = ID, filename = Filename, signature = Signature}) ->
    #{
        <<"id"        >> => ID,
        <<"filename"  >> => Filename,
        <<"signatures">> => decode_report_file_signature(Signature)
    }.

decode_report_file_signature(#reports_Signature{md5 = MD5, sha256 = SHA256}) ->
    #{<<"md5">> => MD5, <<"sha256">> => SHA256}.
