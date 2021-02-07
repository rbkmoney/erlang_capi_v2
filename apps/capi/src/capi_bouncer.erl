-module(capi_bouncer).

-include_lib("bouncer_proto/include/bouncer_context_thrift.hrl").

-export([extract_context_fragments/2]).
-export([judge/2]).

-export([get_claim/1]).
-export([set_claim/2]).
-export([decode_claim/1]).
-export([encode_claim/1]).

-define(CLAIM_BOUNCER_CTX, <<"bouncer_ctx">>).

%%

-spec extract_context_fragments(swag_server:request_context(), woody_context:ctx()) ->
    capi_bouncer_context:fragments() | undefined.
extract_context_fragments(ReqCtx, WoodyCtx) ->
    extract_context_fragments([claim, metadata], ReqCtx, WoodyCtx).

extract_context_fragments([Method | Rest], ReqCtx, WoodyCtx) ->
    case extract_context_fragments_by(Method, get_auth_context(ReqCtx), WoodyCtx) of
        {FragmentAcc, ExternalFragments} ->
            {add_requester_context(ReqCtx, FragmentAcc), ExternalFragments};
        undefined ->
            extract_context_fragments(Rest, ReqCtx, WoodyCtx)
    end;
extract_context_fragments([], _, _) ->
    undefined.

-spec judge(capi_bouncer_context:fragments(), woody_context:ctx()) -> capi_auth:resolution().
judge({Acc, External}, WoodyCtx) ->
    % TODO error out early?
    {ok, RulesetID} = application:get_env(capi, bouncer_ruleset_id),
    JudgeContext = #{fragments => External#{<<"capi">> => Acc}},
    bouncer_client:judge(RulesetID, JudgeContext, WoodyCtx).

%%

extract_context_fragments_by(claim, {_, _, Claims, _}, _) ->
    % TODO
    % We deliberately do not handle decoding errors here since we extract claims from verified
    % tokens only, hence they must be well-formed here.
    case get_claim(Claims) of
        {ok, ClaimFragment} ->
            {Acc, External} = capi_bouncer_context:new(),
            {Acc, External#{<<"claim">> => ClaimFragment}};
        undefined ->
            undefined
    end;
extract_context_fragments_by(metadata, AuthCtx = {_, _, _, Metadata}, WoodyCtx) ->
    case Metadata of
        #{auth_method := AuthMethod} ->
            build_auth_context_fragments(AuthMethod, AuthCtx, WoodyCtx);
        #{} ->
            undefined
    end.

-spec build_auth_context_fragments(
    capi_auth:auth_method(),
    uac:context(capi_auth:metadata()),
    woody_context:ctx()
) -> capi_bouncer_context:fragments().
build_auth_context_fragments(user_session_token, AuthCtx = {_, _, _, Metadata}, WoodyCtx) ->
    UserID = uac_authorizer_jwt:get_subject_id(AuthCtx),
    Expiration = uac_authorizer_jwt:get_expires_at(AuthCtx),
    {Acc0, External} = capi_bouncer_context:new(),
    Acc1 = bouncer_context_helpers:add_user(
        #{
            id => UserID,
            email => uac_authorizer_jwt:get_subject_email(AuthCtx),
            realm => #{id => maps:get(user_realm, Metadata, undefined)}
        },
        Acc0
    ),
    Acc2 = bouncer_context_helpers:add_auth(
        #{
            method => <<"SessionToken">>,
            expiration => make_auth_expiration(Expiration),
            token => #{id => uac_authorizer_jwt:get_token_id(AuthCtx)}
        },
        Acc1
    ),
    case bouncer_context_helpers:get_user_orgs_fragment(UserID, WoodyCtx) of
        {ok, UserOrgsFragment} ->
            {Acc2, External#{<<"userorg">> => UserOrgsFragment}};
        {error, {user, notfound}} ->
            {Acc2, External}
    end.

make_auth_expiration(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
make_auth_expiration(unlimited) ->
    undefined.

get_auth_context(#{auth_context := AuthCtx}) ->
    AuthCtx.

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
