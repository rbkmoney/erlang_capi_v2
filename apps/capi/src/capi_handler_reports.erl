-module(capi_handler_reports).

-include_lib("reporter_proto/include/reporter_base_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

% seconds
-define(DEFAULT_URL_LIFETIME, 60).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID, Req, Context) when
    OperationID =:= 'GetReports' orelse
        OperationID =:= 'GetReportsForParty' orelse
        OperationID =:= 'GetReport' orelse
        OperationID =:= 'GetReportForParty' orelse
        OperationID =:= 'CreateReport' orelse
        OperationID =:= 'CreateReportForParty' orelse
        OperationID =:= 'DownloadFile' orelse
        OperationID =:= 'DownloadFileForParty'
->
    Authorize = fun() -> {ok, capi_auth:authorize_operation(OperationID, [], Context, Req)} end,
    Process = fun() -> process_request(OperationID, Context, Req) end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Context :: capi_handler:processing_context(),
    ReqState :: capi_handler:request_state()
) -> {ok, capi_handler:response()}.
process_request('GetReports', Context, Req) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    get_reports(PartyID, Req, Context);
process_request('GetReportsForParty', Context, Req) ->
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(partyID, Req),
    capi_handler_utils:run_if_party_accessible(UserID, PartyID, fun() ->
        get_reports(PartyID, Req, Context)
    end);
process_request('GetReport', Context, Req) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    get_report(PartyID, Req, Context);
process_request('GetReportForParty', Context, Req) ->
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(partyID, Req),
    capi_handler_utils:run_if_party_accessible(UserID, PartyID, fun() ->
        get_report(PartyID, Req, Context)
    end);
process_request('CreateReport', Context, Req) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    create_report(PartyID, Req, Context);
process_request('CreateReportForParty', Context, Req) ->
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(partyID, Req),
    capi_handler_utils:run_if_party_accessible(UserID, PartyID, fun() ->
        create_report(PartyID, Req, Context)
    end);
process_request('DownloadFile', Context, Req) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    download_file(PartyID, Req, Context);
process_request('DownloadFileForParty', Context, Req) ->
    UserID = capi_handler_utils:get_user_id(Context),
    PartyID = maps:get(partyID, Req),
    capi_handler_utils:run_if_party_accessible(UserID, PartyID, fun() ->
        download_file(PartyID, Req, Context)
    end).

%%

create_report(PartyID, Req, Context) ->
    ShopID = maps:get(shopID, Req),
    ReportParams = maps:get('ReportParams', Req),
    ReportRequest = #reports_ReportRequest{
        party_id = PartyID,
        shop_id = ShopID,
        time_range = #reports_ReportTimeRange{
            from_time = capi_handler_utils:get_time(<<"fromTime">>, ReportParams),
            to_time = capi_handler_utils:get_time(<<"toTime">>, ReportParams)
        }
    },
    ReportType = encode_report_type(maps:get(<<"reportType">>, ReportParams)),
    case capi_handler_call:service_call({reporting, 'CreateReport', {ReportRequest, ReportType}}, Context) of
        {ok, ReportId} ->
            {ok, Report} = capi_handler_call:service_call(
                {reporting, 'GetReport', {ReportId}},
                Context
            ),
            {ok, {201, #{}, decode_report(Report)}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_ShopNotFound{} ->
                    {ok, logic_error(invalidShopID, <<"Shop not found">>)}
            end
    end.

get_report(PartyID, Req, Context) ->
    ShopID = maps:get(shopID, Req),
    ReportID = maps:get(reportID, Req),
    Call = {reporting, 'GetReport', {ReportID}},
    case capi_handler_call:service_call(Call, Context) of
        {ok, Report = #'reports_Report'{party_id = PartyID, shop_id = ShopID}} ->
            {ok, {200, #{}, decode_report(Report)}};
        {ok, _WrongReport} ->
            {ok, general_error(404, <<"Report not found">>)};
        {exception, #reports_ReportNotFound{}} ->
            {ok, general_error(404, <<"Report not found">>)}
    end.

get_reports(PartyID, Req, Context) ->
    ShopID = maps:get(shopID, Req),
    FromTime = capi_handler_utils:get_time('fromTime', Req),
    ToTime = capi_handler_utils:get_time('toTime', Req),
    ReportRequest = #reports_ReportRequest{
        party_id = PartyID,
        shop_id = ShopID,
        time_range = #reports_ReportTimeRange{
            from_time = FromTime,
            to_time = ToTime
        }
    },
    StatReportRequest = #reports_StatReportRequest{
        request = ReportRequest
    },
    Call = {reporting, 'GetReports', {StatReportRequest}},
    case capi_handler_call:service_call(Call, Context) of
        {ok, #reports_StatReportResponse{reports = Reports}} ->
            {ok, {200, #{}, [decode_report(R) || R <- Reports]}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit]))}
            end
    end.

download_file(PartyID, Req, Context) ->
    ShopID = maps:get(shopID, Req),
    Call = {
        reporting,
        'GetReport',
        {maps:get(reportID, Req)}
    },
    case capi_handler_call:service_call(Call, Context) of
        {ok, #reports_Report{status = created, files = Files, party_id = PartyID, shop_id = ShopID}} ->
            FileID = maps:get(fileID, Req),
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, Context);
                false ->
                    {ok, general_error(404, <<"File not found">>)}
            end;
        {ok, _WrongReport} ->
            {ok, general_error(404, <<"Report not found">>)};
        {exception, #reports_ReportNotFound{}} ->
            {ok, general_error(404, <<"Report not found">>)}
    end.

generate_report_presigned_url(FileID, Context) ->
    ExpiresAt = get_default_url_lifetime(),
    Call = {reporting, 'GeneratePresignedUrl', {FileID, ExpiresAt}},
    case capi_handler_call:service_call(Call, Context) of
        {ok, URL} ->
            {ok, {200, #{}, #{<<"url">> => URL}}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_FileNotFound{} ->
                    {ok, general_error(404, <<"File not found">>)}
            end
    end.

get_default_url_lifetime() ->
    Now = erlang:system_time(second),
    Lifetime = application:get_env(capi, reporter_url_lifetime, ?DEFAULT_URL_LIFETIME),
    genlib_rfc3339:format_relaxed(Now + Lifetime, second).

%%

encode_report_type(<<"provisionOfService">>) -> <<"provision_of_service">>;
encode_report_type(<<"paymentRegistry">>) -> <<"payment_registry">>.

%%

decode_report(Report) ->
    #reports_ReportTimeRange{from_time = FromTime, to_time = ToTime} = Report#reports_Report.time_range,
    #{
        <<"id">> => Report#reports_Report.report_id,
        <<"createdAt">> => Report#reports_Report.created_at,
        <<"fromTime">> => FromTime,
        <<"toTime">> => ToTime,
        <<"status">> => decode_report_status(Report#reports_Report.status),
        <<"type">> => decode_report_type(Report#reports_Report.report_type),
        <<"files">> => [decode_report_file(F) || F <- Report#reports_Report.files]
    }.

decode_report_status(pending) -> <<"pending">>;
decode_report_status(created) -> <<"created">>.

decode_report_type(<<"provision_of_service">>) -> <<"provisionOfService">>;
decode_report_type(<<"payment_registry">>) -> <<"paymentRegistry">>.

decode_report_file(#reports_FileMeta{file_id = ID, filename = Filename, signature = Signature}) ->
    #{
        <<"id">> => ID,
        <<"filename">> => Filename,
        <<"signatures">> => decode_report_file_signature(Signature)
    }.

decode_report_file_signature(#reports_Signature{md5 = MD5, sha256 = SHA256}) ->
    #{<<"md5">> => MD5, <<"sha256">> => SHA256}.
