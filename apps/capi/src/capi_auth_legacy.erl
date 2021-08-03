-module(capi_auth_legacy).

%% API functions

-export([get_subject_id/1]).
-export([get_subject_email/1]).
-export([get_subject_name/1]).
-export([get_claims/1]).

-export([authorize_api_key/1]).

-export([get_consumer/1]).

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

-spec authorize_api_key(ApiKey :: binary()) ->
    {ok, context()} | {error, {authorize_api_key_failed, _Reason}}.
authorize_api_key(ApiKey) ->
    case uac:authorize_api_key(ApiKey, #{}) of
        {ok, Context} ->
            {ok, Context};
        {error, Error} ->
            {error, {authorize_api_key_failed, Error}}
    end.

%%

-spec get_consumer(context()) -> consumer().
get_consumer(Context) ->
    case maps:get(<<"cons">>, uac_authorizer_jwt:get_claims(Context), <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider
    end.

-spec get_access_config() -> uac_conf:options().
get_access_config() ->
    #{
        domain_name => <<"common-api">>,
        resource_hierarchy => get_resource_hierarchy()
    }.

%%
%% Internal functions
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
