-module(capi_bouncer).

-include_lib("bouncer_proto/include/bouncer_context_thrift.hrl").

-export([gather_context_fragments/3]).
-export([judge/2]).

%%

-spec gather_context_fragments(
    AuthData :: token_keeper_auth_data:auth_data(),
    RequestContext :: swag_server:request_context(),
    WoodyContext :: woody_context:ctx()
) ->
    capi_bouncer_context:fragments().
gather_context_fragments(AuthData, ReqCtx, WoodyCtx) ->
    {Base, External0} = capi_bouncer_context:new(),
    External1 = External0#{<<"token-keeper">> => token_keeper_auth_data:get_context_fragment(AuthData)},
    {add_requester_context(ReqCtx, Base), maybe_add_userorg(External1, AuthData, WoodyCtx)}.

-spec judge(capi_bouncer_context:fragments(), woody_context:ctx()) -> capi_auth:resolution().
judge({Acc, External}, WoodyCtx) ->
    % TODO error out early?
    {ok, RulesetID} = application:get_env(capi, bouncer_ruleset_id),
    JudgeContext = #{fragments => External#{<<"capi">> => Acc}},
    bouncer_client:judge(RulesetID, JudgeContext, WoodyCtx).

%%

maybe_add_userorg(External, AuthData, WoodyCtx) ->
    case token_keeper_auth_data:get_user_id(AuthData) of
        UserID when UserID =/= undefined ->
            case bouncer_context_helpers:get_user_orgs_fragment(UserID, WoodyCtx) of
                {ok, UserOrgsFragment} ->
                    External#{<<"userorg">> => UserOrgsFragment};
                {error, {user, notfound}} ->
                    External
            end;
        undefined ->
            External
    end.

-spec add_requester_context(swag_server:request_context(), capi_bouncer_context:acc()) -> capi_bouncer_context:acc().
add_requester_context(ReqCtx, FragmentAcc) ->
    ClientPeer = maps:get(peer, ReqCtx, #{}),
    bouncer_context_helpers:add_requester(
        #{ip => maps:get(ip_address, ClientPeer, undefined)},
        FragmentAcc
    ).
