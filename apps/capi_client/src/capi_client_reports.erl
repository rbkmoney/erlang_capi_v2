-module(capi_client_reports).

-export([get_reports/4]).
-export([issue_download_url/4]).

-type context() :: capi_client_lib:context().

-spec get_reports(context(), binary(), binary(), binary()) -> {ok, list()} | {error, term()}.
get_reports(Context, ShopID, FromTime, ToTime) ->
    Params = #{
        binding => #{
            <<"shopID">> => ShopID
        },
        qs_val => #{
            <<"fromTime">> => FromTime,
            <<"toTime">> => ToTime
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:get_reports(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec issue_download_url(context(), binary(), binary(), binary()) -> {ok, map()} | {error, term()}.
issue_download_url(Context, ShopID, ReportID, FileID) ->
    Params = #{
        binding => #{
            <<"shopID">> => ShopID,
            <<"reportID">> => ReportID,
            <<"fileID">> => FileID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:issue_download_url(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

