-module(capi_client_reports).

-export([get_reports/4]).
-export([download_file/4]).

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

-spec download_file(context(), binary(), binary(), binary()) -> {ok, redirect} | {error, term()}.
download_file(Context, ShopID, ReportID, FileID) ->
    Params = #{
        binding => #{
            <<"shopID">> => ShopID,
            <<"reportID">> => ReportID,
            <<"fileID">> => FileID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:download_file(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

