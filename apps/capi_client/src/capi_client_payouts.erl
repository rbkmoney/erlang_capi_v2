-module(capi_client_payouts).

-export([get_payout_tools/2]).
-export([get_payout_tools/3]).
-export([get_payout_tool_by_id/3]).
-export([get_payout_tool_by_id/4]).
-export([get_schedule_by_ref/2]).
-export([create_payout/3]).
-export([get_payout/2]).

-type context() :: capi_client_lib:context().

-spec get_payout_tools(context(), binary()) -> {ok, term()} | {error, term()}.
get_payout_tools(Context, ContractID) ->
    get_payout_tools(Context, undefined, ContractID).

-spec get_payout_tools(context(), binary() | undefined, binary()) -> {ok, term()} | {error, term()}.
get_payout_tools(Context, PartyID, ContractID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"contractID">> => ContractID
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:get_payout_tools(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payout_tool_by_id(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_payout_tool_by_id(Context, ContractID, PayoutToolID) ->
    get_payout_tool_by_id(Context, undefined, ContractID, PayoutToolID).

-spec get_payout_tool_by_id(context(), binary() | undefined, binary(), binary()) -> {ok, term()} | {error, term()}.
get_payout_tool_by_id(Context, PartyID, ContractID, PayoutToolID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"contractID">> => ContractID,
            <<"payoutToolID">> => PayoutToolID
        })
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

-spec create_payout(context(), map(), binary()) -> {ok, term()} | {error, term()}.
create_payout(Context, Request, PayoutID) ->
    Params = #{
        binding => #{
            <<"payoutID">> => PayoutID
        },
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:create_payout(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payout(context(), binary()) -> {ok, term()} | {error, term()}.
get_payout(Context, PayoutID) ->
    Params = #{
        binding => #{
            <<"payoutID">> => PayoutID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payouts_api:get_payout(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
