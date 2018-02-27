-module(capi_client_payouts).

-export([get_payout_tools/2]).
-export([get_payout_tool_by_id/3]).
-export([get_schedule_by_ref/2]).

-type context() :: capi_client_lib:context().

-spec get_payout_tools(context(), binary()) -> {ok, term()} | {error, term()}.
get_payout_tools(Context, ContractID) ->
    Params = #{
        binding => #{
            <<"contractID">> => ContractID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:get_payout_tools(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payout_tool_by_id(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_payout_tool_by_id(Context, ContractID, PayoutToolID) ->
    Params = #{
        binding => #{
            <<"contractID">> => ContractID,
            <<"payoutToolID">> => PayoutToolID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:get_payout_tool_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_schedule_by_ref(context(), term()) -> {ok, term()} | {error, term()}.
get_schedule_by_ref(Context, ScheduleRef) ->
    Params = #{
        binding => #{
            <<"scheduleID">> => genlib:to_list(ScheduleRef)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:get_schedule_by_ref(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
