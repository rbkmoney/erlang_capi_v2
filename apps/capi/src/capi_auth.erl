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
-export([issue_access_token/3]).

-export([get_consumer/1]).

%% API types

-type token_type() :: bearer.

-type preauth_context() :: {unauthorized, {token_type(), token_keeper_client:token()}}.
-type auth_context() ::
    {authorized, token_keeper_auth_data:auth_data()}.

-type resolution() :: allowed | forbidden.

-type consumer() :: client | merchant | provider.

-export_type([preauth_context/0]).
-export_type([auth_context/0]).
-export_type([resolution/0]).
-export_type([consumer/0]).

%% Internal types

-define(authorized(Ctx), {authorized, Ctx}).
-define(unauthorized(Ctx), {unauthorized, Ctx}).

-type token_scope() ::
    {invoice, InvoiceID :: binary()}
    | {invoice_tpl, InvoiceTplID :: binary()}
    | {customer, CustomerID :: binary()}.
-type token_lifetime() :: pos_integer() | unlimited.

-type token_spec() :: #{
    scope := token_scope(),
    party_id := binary(),
    lifetime => token_lifetime()
}.

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
    token_keeper_auth_data:get_metadata(get_api_key_namespace(), <<"party_id">>, AuthData).

-spec get_user_id(auth_context()) -> binary() | undefined.
get_user_id(?authorized(AuthData)) ->
    token_keeper_auth_data:get_metadata(get_user_session_namespace(), <<"user_id">>, AuthData).

-spec get_user_email(auth_context()) -> binary() | undefined.
get_user_email(?authorized(AuthData)) ->
    token_keeper_auth_data:get_metadata(get_user_session_namespace(), <<"user_email">>, AuthData).

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

-spec get_consumer(auth_context()) -> consumer().
get_consumer(?authorized(AuthData)) ->
    case token_keeper_auth_data:get_metadata(get_api_key_namespace(), AuthData) of
        #{<<"cons">> := <<"merchant">>} -> merchant;
        #{<<"cons">> := <<"client">>} -> client;
        #{<<"cons">> := <<"provider">>} -> provider;
        _Default -> merchant
    end.

%%

-spec issue_access_token(
    TokenSpec :: token_spec(),
    WoodyContext :: woody_context:ctx()
) ->
    token_keeper_client:token().
issue_access_token(TokenSpec, WoodyContext) ->
    issue_access_token(TokenSpec, #{}, WoodyContext).

-spec issue_access_token(
    TokenSpec :: token_spec(),
    ExtraProperties :: token_keeper_auth_data:metadata_content(),
    WoodyContext :: woody_context:ctx()
) ->
    token_keeper_client:token().
issue_access_token(#{scope := Scope, party_id := PartyID} = TokenSpec, ExtraProperties, WoodyContext) ->
    ContextFragment = create_context_fragment(Scope, PartyID, maps:get(lifetime, TokenSpec, undefined)),
    Metadata = create_metadata(get_api_key_namespace(), PartyID, add_consumer(Scope, ExtraProperties)),
    %%TODO InvoiceTemplateAccessTokens are technically not ephemeral and should become so in the future
    AuthData = token_keeper_client:create_ephemeral(ContextFragment, Metadata, WoodyContext),
    token_keeper_auth_data:get_token(AuthData).

%%
%% Internal functions
%%

-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

create_context_fragment(Access, PartyID, undefined) ->
    create_context_fragment(Access, PartyID, get_default_token_lifetime(Access));
create_context_fragment(Access, PartyID, Lifetime) ->
    ok = verify_token_lifetime(Access, Lifetime),
    ContextFragment0 = bouncer_context_helpers:make_auth_fragment(resolve_bouncer_ctx(Access, PartyID, Lifetime)),
    {encoded_fragment, ContextFragment} = bouncer_client:bake_context_fragment(ContextFragment0),
    ContextFragment.

get_default_token_lifetime({invoice, _}) -> ?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME;
get_default_token_lifetime({invoice_tpl, _}) -> unlimited;
get_default_token_lifetime({customer, _}) -> ?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME.

%% Forbid creation of unlimited lifetime invoice and customer tokens
verify_token_lifetime({invoice, _}, Lifetime) when Lifetime =/= unlimited -> ok;
verify_token_lifetime({customer, _}, Lifetime) when Lifetime =/= unlimited -> ok;
verify_token_lifetime({invoice_tpl, _}, _Lifetime) -> ok;
verify_token_lifetime(_, _) -> error.

-spec resolve_bouncer_ctx(token_scope(), _PartyID :: binary(), token_lifetime()) ->
    bouncer_context_helpers:auth_params().
resolve_bouncer_ctx({invoice, InvoiceID}, PartyID, Lifetime) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICEACCESSTOKEN,
        expiration => make_auth_expiration(Lifetime),
        scope => [
            #{
                party => #{id => PartyID},
                invoice => #{id => InvoiceID}
            }
        ]
    };
resolve_bouncer_ctx({invoice_tpl, InvoiceTemplateID}, PartyID, Lifetime) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICETEMPLATEACCESSTOKEN,
        expiration => make_auth_expiration(Lifetime),
        scope => [
            #{
                party => #{id => PartyID},
                invoice_template => #{id => InvoiceTemplateID}
            }
        ]
    };
resolve_bouncer_ctx({customer, CustomerID}, PartyID, Lifetime) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_CUSTOMERACCESSTOKEN,
        expiration => make_auth_expiration(Lifetime),
        scope => [
            #{
                party => #{id => PartyID},
                customer => #{id => CustomerID}
            }
        ]
    }.

make_auth_expiration(Lifetime) when is_integer(Lifetime) ->
    genlib_rfc3339:format(lifetime_to_expiration(Lifetime), second);
make_auth_expiration(unlimited) ->
    undefined.

lifetime_to_expiration(Lt) when is_integer(Lt) ->
    genlib_time:unow() + Lt.

add_consumer({invoice, _}, ExtraProperties) ->
    ExtraProperties#{<<"cons">> => <<"client">>};
add_consumer(_, ExtraProperties) ->
    ExtraProperties.

create_metadata(Namespace, PartyID, AdditionalMeta) ->
    #{Namespace => AdditionalMeta#{<<"party_id">> => PartyID}}.

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

get_user_session_namespace() ->
    maps:get(user_session_token, get_meta_ns_conf()).

get_api_key_namespace() ->
    maps:get(api_key_token, get_meta_ns_conf()).

get_meta_ns_conf() ->
    genlib_app:env(capi, namespace_mappings, #{}).
