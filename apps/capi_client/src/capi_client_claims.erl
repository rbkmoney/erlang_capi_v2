-module(capi_client_claims).

-export([create_claim/2]).
-export([get_claim_by_id/2]).
-export([get_claims/1]).
-export([get_claims_by_status/2]).
-export([revoke_claim_by_id/4]).

-type context() :: capi_client_lib:context().

-spec create_claim(context(), [term()]) -> {ok, term()} | {error, term()}.
create_claim(Context, Changeset) ->
    Params = #{
        body => Changeset
    },
    {Host, Port, PreparedParams} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_claims_api:create_claim(Host, Port, PreparedParams),
    capi_client_lib:handle_response(Response).

-spec get_claim_by_id(context(), integer()) -> {ok, term()} | {error, term()}.
get_claim_by_id(Context, ClaimID) ->
    Params = #{
        binding => #{
            <<"claimID">> => ClaimID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_claims_api:get_claim_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_claims(context()) -> {ok, term()} | {error, term()}.
get_claims(Context) ->
    Params = #{
        qs_val => #{}
    },
    {Host, Port, PreparedParams} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_claims_api:get_claims(Host, Port, PreparedParams),
    capi_client_lib:handle_response(Response).

-spec get_claims_by_status(context(), term()) -> {ok, term()} | {error, term()}.
get_claims_by_status(Context, Status) ->
    Params = #{
        qs_val => #{
            <<"claimStatus">> => genlib:to_binary(Status)
        }
    },
    {Host, Port, PreparedParams} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_claims_api:get_claims(Host, Port, PreparedParams),
    capi_client_lib:handle_response(Response).

-spec revoke_claim_by_id(context(), binary(), integer(), integer()) -> ok | {error, term()}.
revoke_claim_by_id(Context, Reason, ClaimID, Revision) ->
    Params = #{
        binding => #{
            <<"claimID">> => ClaimID
        },
        qs_val => #{
            <<"claimRevision">> => genlib:to_binary(Revision)
        },
        body => #{
            <<"reason">> => genlib:to_binary(Reason)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_claims_api:revoke_claim_by_id(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, _Body} -> ok;
        {error, Error} -> {error, Error}
    end.
