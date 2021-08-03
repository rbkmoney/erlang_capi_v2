-module(capi_auth).

%% API functions

-export([get_subject_id/1]).
-export([get_subject_email/1]).
-export([get_subject_name/1]).

-export([preauthorize_api_key/1]).
-export([authorize_api_key/3]).
-export([authorize_operation/2]).

-export([issue_access_token/3]).

%% Legacy compatability functions
-export([get_consumer/1]).
-export([get_legacy_claims/1]).

%% API types

-type token_type() :: bearer.

-type preauth_context() :: {unauthorized, {token_type(), token_keeper_client:token()}}.
-type auth_context() ::
    {authorized, #{
        legacy := capi_auth_legacy:context(),
        auth_data => token_keeper_auth_data:auth_data()
    }}.

-type resolution() :: allowed | forbidden.

-type consumer() :: capi_auth_legacy:consumer().

-export_type([preauth_context/0]).
-export_type([auth_context/0]).
-export_type([resolution/0]).
-export_type([consumer/0]).

%% Internal types

-define(authorized(Ctx), {authorized, Ctx}).
-define(unauthorized(Ctx), {unauthorized, Ctx}).

%%
%% API functions
%%

-spec get_subject_id(auth_context()) -> binary() | undefined.
get_subject_id(?authorized(#{auth_data := AuthData})) ->
    case token_keeper_auth_data:get_party_id(AuthData) of
        PartyId when is_binary(PartyId) ->
            PartyId;
        undefined ->
            token_keeper_auth_data:get_user_id(AuthData)
    end;
get_subject_id(?authorized(#{legacy := Context})) ->
    capi_auth_legacy:get_subject_id(Context).

-spec get_subject_email(auth_context()) -> binary() | undefined.
get_subject_email(?authorized(#{auth_data := AuthData})) ->
    token_keeper_auth_data:get_user_email(AuthData);
get_subject_email(?authorized(#{legacy := Context})) ->
    capi_auth_legacy:get_subject_email(Context).

-spec get_subject_name(auth_context()) -> binary() | undefined.
get_subject_name(?authorized(#{auth_data := _AuthData})) ->
    %% Subject names are no longer a thing for auth_data contexts
    undefined;
get_subject_name(?authorized(#{legacy := Context})) ->
    capi_auth_legacy:get_subject_name(Context).

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
    AuthData = get_auth_data(extract_auth_context(ProcessingContext)),
    #{swagger_context := SwagContext, woody_context := WoodyContext} = ProcessingContext,
    Fragments = capi_bouncer:gather_context_fragments(AuthData, SwagContext, WoodyContext),
    Fragments1 = capi_bouncer_context:build(Prototypes, Fragments, WoodyContext),
    capi_bouncer:judge(Fragments1, WoodyContext).

%%

-spec get_consumer(auth_context()) -> consumer().
get_consumer(?authorized(#{legacy := AuthContext})) ->
    %% TODO: Can we even get to these claims now?
    capi_auth_legacy:get_consumer(AuthContext).

-spec get_legacy_claims(auth_context()) -> capi_auth_legacy:claims().
get_legacy_claims(?authorized(#{legacy := AuthContext})) ->
    %% TODO: Seriously.
    %% This is needed here for the ip_replacement_allowed claim to still be accessible
    %% WE ALSO rely on email claim to be present for lazy party creation in capi_handler_parties!
    %% (which is dropped by tk for api tokens)
    capi_auth_legacy:get_claims(AuthContext).

%%

-define(METADATA_NAMESPACE, <<"com.rbkmoney.apikeymgmt">>).

-spec issue_access_token(PartyID :: binary(), token_spec(), woody_context:ctx()) -> token_keeper_client:token().
issue_access_token(PartyID, TokenSpec, WoodyContext) ->
    ContextFragment0 = bouncer_context_helpers:make_auth_fragment(resolve_bouncer_ctx(TokenSpec, PartyID)),
    {encoded_fragment, ContextFragment} = bouncer_client:bake_context_fragment(ContextFragment0),
    Metadata = make_metadata(?METADATA_NAMESPACE, PartyID),
    AuthData = token_keeper_client:create_ephemeral(ContextFragment, Metadata, WoodyContext),
    token_keeper_auth_data:get_token(AuthData).

%%
%% Internal functions
%%

%% TODO
%% Hardcode for now, should pass it here probably as an argument
-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type token_spec() ::
    {invoice, InvoiceID :: binary()}
    | {invoice_tpl, InvoiceTplID :: binary()}
    | {customer, CustomerID :: binary()}.

-spec resolve_bouncer_ctx(token_spec(), _PartyID :: binary()) -> bouncer_context_helpers:auth_params().
resolve_bouncer_ctx({invoice, InvoiceID}, PartyID) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICEACCESSTOKEN,
        expiration => make_auth_expiration(lifetime_to_expiration(?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME)),
        scope => [
            #{
                party => #{id => PartyID},
                invoice => #{id => InvoiceID}
            }
        ]
    };
resolve_bouncer_ctx({invoice_tpl, InvoiceTemplateID}, PartyID) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICETEMPLATEACCESSTOKEN,
        expiration => make_auth_expiration(unlimited),
        scope => [
            #{
                party => #{id => PartyID},
                invoice_template => #{id => InvoiceTemplateID}
            }
        ]
    };
resolve_bouncer_ctx({customer, CustomerID}, PartyID) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_CUSTOMERACCESSTOKEN,
        expiration => make_auth_expiration(lifetime_to_expiration(?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME)),
        scope => [
            #{
                party => #{id => PartyID},
                customer => #{id => CustomerID}
            }
        ]
    }.

lifetime_to_expiration(Lt) when is_integer(Lt) ->
    genlib_time:unow() + Lt.

make_auth_expiration(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
make_auth_expiration(unlimited) ->
    undefined.

make_metadata(Namespace, PartyID) ->
    #{Namespace => #{<<"party_id">> => PartyID}}.

extract_auth_context(#{swagger_context := #{auth_context := ?authorized(AuthContext)}}) ->
    AuthContext.

get_auth_data(AuthContext) ->
    maps:get(auth_data, AuthContext).

authorize_token_by_type(bearer = TokenType, Token, TokenContext, WoodyContext) ->
    %% NONE: For now legacy auth still takes precedence over
    %% bouncer-based auth, so we MUST have a legacy context
    case capi_auth_legacy:authorize_api_key(restore_api_key(TokenType, Token)) of
        {ok, LegacyContext} ->
            case token_keeper_client:get_by_token(Token, TokenContext, WoodyContext) of
                {ok, AuthData} ->
                    {ok, {authorized, make_context(AuthData, LegacyContext)}};
                {error, TokenKeeperError} ->
                    _ = logger:warning("Token keeper authorization failed: ~p", [TokenKeeperError]),
                    {error, {auth_failed, TokenKeeperError}}
            end;
        {error, LegacyError} ->
            {error, {legacy_auth_failed, LegacyError}}
    end.

parse_api_key(<<"Bearer ", Token/binary>>) ->
    {ok, {bearer, Token}};
parse_api_key(_) ->
    {error, unsupported_auth_scheme}.

restore_api_key(bearer, Token) ->
    %% Kind of a hack since legacy auth expects the full api key string, but
    %% token-keeper does not and we got rid of it at preauth stage
    <<"Bearer ", Token/binary>>.

make_context(AuthData, LegacyContext) ->
    genlib_map:compact(#{
        legacy => LegacyContext,
        auth_data => AuthData
    }).
