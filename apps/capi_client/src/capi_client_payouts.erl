-module(capi_client_payouts).

-export([get_payout_tools/2]).

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
