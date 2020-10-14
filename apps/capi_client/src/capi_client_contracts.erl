-module(capi_client_contracts).

-export([get_contract_by_id/2]).
-export([get_contract_by_id/3]).
-export([get_contracts/1]).
-export([get_contracts/2]).
-export([get_contract_adjustment_by_id/3]).
-export([get_contract_adjustment_by_id/4]).
-export([get_contract_adjustments/2]).
-export([get_contract_adjustments/3]).

-type context() :: capi_client_lib:context().

-spec get_contract_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_contract_by_id(Context, ContractID) ->
    get_contract_by_id(Context, undefined, ContractID).

-spec get_contract_by_id(context(), binary() | undefined, binary()) ->
    {ok, term()} | {error, term()}.
get_contract_by_id(Context, PartyID, ContractID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"contractID">> => ContractID
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_contracts_api:get_contract_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_contracts(context()) -> {ok, term()} | {error, term()}.
get_contracts(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_contracts_api:get_contracts(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_contracts(context(), binary()) -> {ok, term()} | {error, term()}.
get_contracts(Context, PartyID) ->
    Params = #{
        binding => #{
            <<"partyID">> => PartyID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_contracts_api:get_contracts(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_contract_adjustment_by_id(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_contract_adjustment_by_id(Context, ContractID, AdjID) ->
    get_contract_adjustment_by_id(Context, undefined, ContractID, AdjID).

-spec get_contract_adjustment_by_id(context(), binary() | undefined, binary(), binary()) ->
    {ok, term()} | {error, term()}.
get_contract_adjustment_by_id(Context, PartyID, ContractID, AdjID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"contractID">> => ContractID,
            <<"adjustmentID">> => AdjID
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_contracts_api:get_contract_adjustment_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_contract_adjustments(context(), binary()) -> {ok, term()} | {error, term()}.
get_contract_adjustments(Context, ContractID) ->
    get_contract_adjustments(Context, undefined, ContractID).

-spec get_contract_adjustments(context(), binary() | undefined, binary()) -> {ok, term()} | {error, term()}.
get_contract_adjustments(Context, PartyID, ContractID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"contractID">> => ContractID
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_contracts_api:get_contract_adjustments(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
