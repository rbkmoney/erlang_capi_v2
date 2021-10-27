-module(capi_auth).

%% API functions

-export([get_subject_id/1]).
-export([get_party_id/1]).
-export([get_user_id/1]).
-export([get_user_email/1]).

-export([preauthorize_api_key/1]).
-export([authorize_api_key/3]).
-export([authorize_operation/2]).
-export([issue_access_token/2]).

-export([get_consumer/1]).

%% API types

-type token_type() :: bearer.
-type preauth_context() :: {unauthorized, {token_type(), token_keeper_client:token()}}.
-type auth_context() :: {authorized, token_keeper_auth_data:auth_data()}.
-type resolution() :: allowed | forbidden.
-type consumer() :: client | merchant | provider.
-type token_spec() :: #{
    party := binary(),
    scope := {invoice | invoice_template | customer, binary()},
    shop => binary(),
    lifetime => pos_integer() | unlimited,
    metadata => token_keeper_auth_data:metadata()
}.

-export_type([preauth_context/0]).
-export_type([auth_context/0]).
-export_type([resolution/0]).
-export_type([consumer/0]).
-export_type([token_spec/0]).

%% Internal types

-define(authorized(Ctx), {authorized, Ctx}).
-define(unauthorized(Ctx), {unauthorized, Ctx}).

%%
%% API functions
%%

-spec get_subject_id(auth_context()) -> binary() | undefined.
get_subject_id(AuthContext) ->
    case get_party_id(AuthContext) of
        PartyId when is_binary(PartyId) ->
            PartyId;
        undefined ->
            get_user_id(AuthContext)
    end.

-spec get_party_id(auth_context()) -> binary() | undefined.
get_party_id(?authorized(AuthData)) ->
    get_metadata(get_metadata_mapped_key(party_id), token_keeper_auth_data:get_metadata(AuthData)).

-spec get_user_id(auth_context()) -> binary() | undefined.
get_user_id(?authorized(AuthData)) ->
    get_metadata(get_metadata_mapped_key(user_id), token_keeper_auth_data:get_metadata(AuthData)).

-spec get_user_email(auth_context()) -> binary() | undefined.
get_user_email(?authorized(AuthData)) ->
    get_metadata(get_metadata_mapped_key(user_email), token_keeper_auth_data:get_metadata(AuthData)).

-spec get_consumer(auth_context()) -> consumer().
get_consumer(?authorized(AuthData)) ->
    case get_metadata(get_metadata_mapped_key(token_consumer), token_keeper_auth_data:get_metadata(AuthData)) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider;
        _Default -> merchant
    end.

%%

-spec preauthorize_api_key(swag_server:api_key()) -> {ok, preauth_context()} | {error, _Reason}.
preauthorize_api_key(ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, Token} ->
            {ok, ?unauthorized(Token)};
        {error, Error} ->
            {error, Error}
    end.

-spec authorize_api_key(preauth_context(), token_keeper_client:source_context(), woody_context:ctx()) ->
    {ok, auth_context()} | {error, _Reason}.
authorize_api_key(?unauthorized({TokenType, Token}), TokenContext, WoodyContext) ->
    authorize_token_by_type(TokenType, Token, TokenContext, WoodyContext).

-spec authorize_operation(
    Prototypes :: capi_bouncer_context:prototypes(),
    ProcessingContext :: capi_handler:processing_context()
) -> resolution().
authorize_operation(Prototypes, ProcessingContext) ->
    AuthContext = extract_auth_context(ProcessingContext),
    #{swagger_context := SwagContext, woody_context := WoodyContext} = ProcessingContext,
    Fragments = capi_bouncer:gather_context_fragments(
        get_token_keeper_fragment(AuthContext),
        get_user_id(AuthContext),
        SwagContext,
        WoodyContext
    ),
    Fragments1 = capi_bouncer_context:build(Prototypes, Fragments, WoodyContext),
    capi_bouncer:judge(Fragments1, WoodyContext).

%%

-spec issue_access_token(TokenSpec :: token_spec(), WoodyContext :: woody_context:ctx()) ->
    token_keeper_client:token().
issue_access_token(TokenSpec, WoodyContext) ->
    ContextFragment = create_context_fragment(TokenSpec),
    Metadata = create_metadata(TokenSpec),
    %%TODO InvoiceTemplateAccessTokens are technically not ephemeral and should become so in the future
    AuthData = token_keeper_client:create_ephemeral(ContextFragment, Metadata, WoodyContext),
    token_keeper_auth_data:get_token(AuthData).

%%
%% Internal functions
%%

-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

create_context_fragment(TokenSpec) ->
    AuthContext = resolve_auth_context(TokenSpec),
    ContextFragment0 = bouncer_context_helpers:make_auth_fragment(AuthContext),
    {encoded_fragment, ContextFragment} = bouncer_client:bake_context_fragment(ContextFragment0),
    ContextFragment.

-spec resolve_auth_context(token_spec()) ->
    bouncer_context_helpers:auth_params().
resolve_auth_context(TokenSpec) ->
    Scope = resolve_auth_scope(TokenSpec),
    #{
        method => resolve_auth_method(TokenSpec),
        expiration => resolve_auth_expiration(TokenSpec),
        scope => [Scope]
    }.

resolve_auth_scope(TokenSpec) ->
    maps:fold(
        fun
            (party = Entity, EntityID, Scope) ->
                Scope#{Entity => #{id => EntityID}};
            (scope, {Entity, EntityID}, Scope) ->
                Scope#{Entity => #{id => EntityID}};
            (shop = Entity, EntityID, Scope) ->
                Scope#{Entity => #{id => EntityID}};
            (_Key, _Value, Scope) ->
                Scope
        end,
        #{},
        TokenSpec
    ).

resolve_auth_method(#{scope := {invoice, _}}) -> ?BCTX_V1_AUTHMETHOD_INVOICEACCESSTOKEN;
resolve_auth_method(#{scope := {customer, _}}) -> ?BCTX_V1_AUTHMETHOD_CUSTOMERACCESSTOKEN;
resolve_auth_method(#{scope := {invoice_template, _}}) -> ?BCTX_V1_AUTHMETHOD_INVOICETEMPLATEACCESSTOKEN.

resolve_auth_expiration(TokenSpec) ->
    case get_token_lifetime(TokenSpec) of
        unlimited ->
            undefined;
        LifeTime ->
            Deadline = genlib_time:unow() + LifeTime,
            genlib_rfc3339:format(Deadline, second)
    end.

get_token_lifetime(#{lifetime := LifeTime} = TokenSpec) when LifeTime =/= undefined ->
    ok = verify_token_lifetime(TokenSpec, LifeTime),
    LifeTime;
get_token_lifetime(#{scope := {invoice, _}}) ->
    ?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME;
get_token_lifetime(#{scope := {invoice_template, _}}) ->
    unlimited;
get_token_lifetime(#{scope := {customer, _}}) ->
    ?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME.

%% Forbid creation of unlimited lifetime invoice and customer tokens
verify_token_lifetime(#{scope := {invoice, _}}, LifeTime) when LifeTime =/= unlimited -> ok;
verify_token_lifetime(#{scope := {customer, _}}, LifeTime) when LifeTime =/= unlimited -> ok;
verify_token_lifetime(#{scope := {invoice_template, _}}, _LifeTime) -> ok.

%%

create_metadata(TokenSpec) ->
    PartyID = maps:get(party, TokenSpec),
    Metadata0 = maps:get(metadata, TokenSpec, #{}),
    Metadata1 = put_metadata(get_metadata_mapped_key(party_id), PartyID, Metadata0),
    Metadata2 = put_metadata(get_metadata_mapped_key(token_consumer), <<"client">>, Metadata1),
    put_token_link_metadata(TokenSpec, Metadata2).

put_token_link_metadata(#{scope := {invoice, EntityID}}, Meta) ->
    put_metadata(<<"invoice_link">>, EntityID, Meta);
put_token_link_metadata(#{scope := {customer, EntityID}}) ->
    put_metadata(<<"customer_link">>, EntityID, Meta).

extract_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

get_token_keeper_fragment(?authorized(AuthData)) ->
    token_keeper_auth_data:get_context_fragment(AuthData).

authorize_token_by_type(bearer, Token, TokenContext, WoodyContext) ->
    case token_keeper_client:get_by_token(Token, TokenContext, WoodyContext) of
        {ok, AuthData} ->
            {ok, ?authorized(AuthData)};
        {error, TokenKeeperError} ->
            _ = logger:warning("Token keeper authorization failed: ~p", [TokenKeeperError]),
            {error, {auth_failed, TokenKeeperError}}
    end.

parse_api_key(<<"Bearer ", Token/binary>>) ->
    {ok, {bearer, Token}};
parse_api_key(_) ->
    {error, unsupported_auth_scheme}.

%%

get_metadata(Key, Metadata) ->
    maps:get(Key, Metadata, undefined).

put_metadata(Key, Value, Metadata) ->
    maps:put(Key, Value, Metadata).

get_metadata_mapped_key(Key) ->
    maps:get(Key, get_meta_mappings()).

get_meta_mappings() ->
    AuthConfig = genlib_app:env(capi, auth_config),
    maps:get(metadata_mappings, AuthConfig).
