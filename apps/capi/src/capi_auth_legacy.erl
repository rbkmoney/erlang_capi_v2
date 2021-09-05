-module(capi_auth_legacy).

%% API functions

-export([get_subject_id/1]).
-export([get_subject_email/1]).
-export([get_subject_name/1]).
-export([get_claims/1]).

-export([authorize_api_key/1]).

-export([issue_access_token/3]).

-export([get_consumer/1]).
-export([get_extra_properties/0]).

-export([get_access_config/0]).

%% API types

-type context() :: uac:context().
-type claims() :: uac:claims().
-type consumer() :: client | merchant | provider.
-type resolution() ::
    allowed
    | forbidden
    | {forbidden, _Reason}.
-type realm() :: binary().
-type auth_method() ::
    user_session_token.
-type metadata() :: #{
    auth_method => auth_method(),
    user_realm => realm()
}.

-export_type([context/0]).
-export_type([claims/0]).
-export_type([consumer/0]).
-export_type([resolution/0]).
-export_type([realm/0]).
-export_type([auth_method/0]).
-export_type([metadata/0]).

%% Internal types

%%
%% API functions
%%

-spec get_subject_id(context()) -> binary() | undefined.
get_subject_id(Context) ->
    uac_authorizer_jwt:get_subject_id(Context).

-spec get_subject_email(context()) -> binary() | undefined.
get_subject_email(Context) ->
    uac_authorizer_jwt:get_claim(<<"email">>, Context, undefined).

-spec get_subject_name(context()) -> binary() | undefined.
get_subject_name(Context) ->
    uac_authorizer_jwt:get_claim(<<"name">>, Context, undefined).

-spec get_claims(context()) -> claims().
get_claims(Context) ->
    uac_authorizer_jwt:get_claims(Context).

%%

-spec authorize_api_key(ApiKey :: binary()) -> {ok, context()} | {error, {authorize_api_key_failed, _Reason}}.
authorize_api_key(ApiKey) ->
    case uac:authorize_api_key(ApiKey, #{}) of
        {ok, Context} ->
            {ok, Context};
        {error, Error} ->
            {error, {authorize_api_key_failed, Error}}
    end.

%%

-define(SIGNEE, capi).

%% TODO
%% Hardcode for now, should pass it here probably as an argument
-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type token_spec() :: #{
    invoice | invoice_tpl | customer | shop => binary()
}.

-spec issue_access_token(PartyID :: binary(), token_spec(), map()) -> uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, ExtraProperties) ->
    TokenID = capi_utils:get_unique_id(),
    AuthContext = resolve_bouncer_ctx(TokenSpec, PartyID),
    ContextFragment = bouncer_context_helpers:make_auth_fragment(
        AuthContext#{
            token => #{id => TokenID}
        }
    ),
    % Метаданные для привязки платежных токенов в createPaymentResource
    Claims0 = maps:merge(
        ExtraProperties,
        resolve_payment_token_metadata(TokenSpec)
    ),
    Claims1 = maps:merge(
        Claims0,
        resolve_token_spec(TokenSpec)
    ),
    Claims2 = capi_bouncer:set_claim(
        bouncer_client:bake_context_fragment(ContextFragment),
        Claims1
    ),
    capi_utils:unwrap(
        uac_authorizer_jwt:issue(TokenID, PartyID, Claims2, ?SIGNEE)
    ).

%%

-spec get_consumer(context()) -> consumer().
get_consumer(Context) ->
    case maps:get(<<"cons">>, uac_authorizer_jwt:get_claims(Context), <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider
    end.

-spec get_extra_properties() -> [binary()].
% Which claims are gonna make it to InvoiceAccessTokens
get_extra_properties() ->
    [
        <<"ip_replacement_allowed">>
    ].

-spec get_access_config() -> uac_conf:options().
get_access_config() ->
    #{
        domain_name => <<"common-api">>,
        resource_hierarchy => get_resource_hierarchy()
    }.

%%
%% Internal functions
%%

-spec resolve_token_spec(token_spec() | tuple()) -> claims().
resolve_token_spec(#{} = TokenSpec) ->
    maps:fold(
        fun
            (Entity, EntityID, undefined) ->
                resolve_token_spec({Entity, EntityID});
            (_Entity, _EntityID, Context) ->
                Context
        end,
        undefined,
        TokenSpec
    );
resolve_token_spec({invoice, InvoiceID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[{invoices, InvoiceID}], read},
            {[{invoices, InvoiceID}, payments], read},
            {[{invoices, InvoiceID}, payments], write},
            {[payment_resources], write}
        ])
    },
    Expiration = lifetime_to_expiration(?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME),
    #{
        <<"exp">> => Expiration,
        <<"resource_access">> => DomainRoles,
        % token consumer
        <<"cons">> => <<"client">>
    };
resolve_token_spec({invoice_tpl, InvoiceTplID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[party, {invoice_templates, InvoiceTplID}], read},
            {[party, {invoice_templates, InvoiceTplID}, invoice_template_invoices], write}
        ])
    },
    #{
        <<"exp">> => unlimited,
        <<"resource_access">> => DomainRoles
    };
resolve_token_spec({customer, CustomerID}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[{customers, CustomerID}], read},
            {[{customers, CustomerID}, bindings], read},
            {[{customers, CustomerID}, bindings], write},
            {[payment_resources], write}
        ])
    },
    Expiration = lifetime_to_expiration(?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME),
    #{
        <<"exp">> => Expiration,
        <<"resource_access">> => DomainRoles
    };
resolve_token_spec({_Entity, _EntityID}) ->
    % ED-123:  для ролей другие сущности не используются
    undefined.

-spec resolve_payment_token_metadata(token_spec()) -> claims().
resolve_payment_token_metadata(TokenSpec) ->
    maps:fold(
        fun
            (invoice, EntityID, Meta) -> Meta#{<<"invoice_link">> => EntityID};
            (customer, EntityID, Meta) -> Meta#{<<"customer_link">> => EntityID};
            (_Entity, _EntityID, Meta) -> Meta
        end,
        #{},
        TokenSpec
    ).

-spec resolve_bouncer_ctx(token_spec(), _PartyID :: binary()) -> bouncer_context_helpers:auth_params().
resolve_bouncer_ctx(TokenSpec, PartyID) ->
    Scope = maps:map(
        fun(_Entity, EntityID) ->
            #{id => EntityID}
        end,
        TokenSpec#{party => PartyID}
    ),

    AuthContext = maps:fold(
        fun
            (Entity, _EntityID, undefined) ->
                resolve_bouncer_ctx(Entity);
            (_Entity, _EntityID, Context) ->
                Context
        end,
        undefined,
        TokenSpec
    ),
    AuthContext#{
        scope => [Scope]
    }.

resolve_bouncer_ctx(invoice) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICEACCESSTOKEN,
        expiration => make_auth_expiration(lifetime_to_expiration(?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME))
    };
resolve_bouncer_ctx(invoice_tpl) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_INVOICETEMPLATEACCESSTOKEN,
        expiration => make_auth_expiration(unlimited)
    };
resolve_bouncer_ctx(customer) ->
    #{
        method => ?BCTX_V1_AUTHMETHOD_CUSTOMERACCESSTOKEN,
        expiration => make_auth_expiration(lifetime_to_expiration(?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME))
    };
resolve_bouncer_ctx(_) ->
    undefined.

lifetime_to_expiration(Lt) when is_integer(Lt) ->
    genlib_time:unow() + Lt.

make_auth_expiration(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
make_auth_expiration(unlimited) ->
    undefined.

%%

-spec get_resource_hierarchy() -> uac_conf:resource_hierarchy().
get_resource_hierarchy() ->
    #{
        party => #{invoice_templates => #{invoice_template_invoices => #{}}},
        customers => #{bindings => #{}},
        invoices => #{payments => #{}},
        payment_resources => #{},
        payouts => #{},
        card_bins => #{}
    }.
