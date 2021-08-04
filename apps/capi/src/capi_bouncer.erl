-module(capi_bouncer).

-include_lib("bouncer_proto/include/bouncer_context_thrift.hrl").

-export([gather_context_fragments/3]).
-export([judge/2]).

-export([get_claim/1]).
-export([set_claim/2]).
-export([decode_claim/1]).
-export([encode_claim/1]).

-define(CLAIM_BOUNCER_CTX, <<"bouncer_ctx">>).

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

%%

-define(CLAIM_CTX_TYPE, <<"ty">>).
-define(CLAIM_CTX_CONTEXT, <<"ct">>).

-define(CLAIM_CTX_TYPE_V1_THRIFT_BINARY, <<"v1_thrift_binary">>).

-type claim() :: term().
-type claims() :: uac_authorizer_jwt:claims().

-spec get_claim(claims()) -> {ok, capi_bouncer_context:fragment()} | {error, {unsupported, claim()}} | undefined.
get_claim(Claims) ->
    case maps:get(?CLAIM_BOUNCER_CTX, Claims, undefined) of
        Claim when Claim /= undefined ->
            decode_claim(Claim);
        undefined ->
            undefined
    end.

-spec decode_claim(claim()) ->
    {ok, capi_bouncer_context:fragment()} | {error, {unsupported, claim()} | {malformed, binary()}}.
decode_claim(#{
    ?CLAIM_CTX_TYPE := ?CLAIM_CTX_TYPE_V1_THRIFT_BINARY,
    ?CLAIM_CTX_CONTEXT := Content
}) ->
    try
        {ok,
            {encoded_fragment, #bctx_ContextFragment{
                type = v1_thrift_binary,
                content = base64:decode(Content)
            }}}
    catch
        % NOTE
        % The `base64:decode/1` fails in unpredictable ways.
        error:_ ->
            {error, {malformed, Content}}
    end;
decode_claim(Ctx) ->
    {error, {unsupported, Ctx}}.

-spec set_claim(capi_bouncer_context:fragment(), claims()) -> claims().
set_claim(ContextFragment, Claims) ->
    false = maps:is_key(?CLAIM_BOUNCER_CTX, Claims),
    Claims#{?CLAIM_BOUNCER_CTX => encode_claim(ContextFragment)}.

-spec encode_claim(capi_bouncer_context:fragment()) -> claim().
encode_claim(
    {encoded_fragment, #bctx_ContextFragment{
        type = v1_thrift_binary,
        content = Content
    }}
) ->
    #{
        ?CLAIM_CTX_TYPE => ?CLAIM_CTX_TYPE_V1_THRIFT_BINARY,
        ?CLAIM_CTX_CONTEXT => base64:encode(Content)
    };
encode_claim(ContextFragment) ->
    encode_claim(bouncer_client:bake_context_fragment(ContextFragment)).
