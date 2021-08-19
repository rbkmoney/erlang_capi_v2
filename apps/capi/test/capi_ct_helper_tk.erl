-module(capi_ct_helper_tk).

-include_lib("capi_dummy_data.hrl").
-include_lib("token_keeper_proto/include/tk_token_keeper_thrift.hrl").
-include_lib("token_keeper_proto/include/tk_context_thrift.hrl").

-include_lib("capi_tk_data.hrl").

-define(PARTY_ID, ?STRING).
-define(USER_ID, ?STRING).
-define(USER_EMAIL, <<"bla@bla.ru">>).

-export([mock_service/2]).

-export([not_found_handler/0]).

-export([user_session_handler/0]).
-export([api_key_handler/1]).
-export([invoice_access_token/2]).
-export([invoice_template_access_token/2]).
-export([customer_access_token/2]).

-export([make_handler_fun/3]).

-dialyzer({no_return, not_found_handler/0}).
-spec not_found_handler() -> handler_fun().
not_found_handler() ->
    fun('GetByToken', {_, _}) ->
        woody_error:raise(business, #token_keeper_AuthDataNotFound{})
    end.

-spec user_session_handler() -> handler_fun().
user_session_handler() ->
    make_handler_fun(
        ?TK_AUTHORITY_KEYCLOAK,
        [
            {user, [id, email, realm]},
            {auth, [{method, <<"SessionToken">>}, expiration, token]}
        ],
        [user_session_meta]
    ).

-spec invoice_access_token(PartyID :: binary(), InvoiceID :: binary()) -> handler_fun().
invoice_access_token(PartyID, InvoiceID) ->
    make_handler_fun(
        <<"com.rbkmoney.capi">>,
        [
            {auth, [
                {method, <<"InvoiceAccessToken">>},
                expiration,
                token,
                {scope, [[{party, PartyID}, {invoice, InvoiceID}]]}
            ]}
        ],
        [api_key_meta, {consumer_meta, <<"client">>}]
    ).

-spec invoice_template_access_token(PartyID :: binary(), InvoiceTmeplateID :: binary()) -> handler_fun().
invoice_template_access_token(PartyID, InvoiceTmeplateID) ->
    make_handler_fun(
        <<"com.rbkmoney.capi">>,
        [
            {auth, [
                {method, <<"InvoiceTemplateAccessToken">>},
                expiration,
                token,
                {scope, [[{party, PartyID}, {invoice_template, InvoiceTmeplateID}]]}
            ]}
        ],
        [api_key_meta]
    ).

-spec customer_access_token(PartyID :: binary(), CustomerID :: binary()) -> handler_fun().
customer_access_token(PartyID, CustomerID) ->
    make_handler_fun(
        <<"com.rbkmoney.capi">>,
        [
            {auth, [
                {method, <<"CustomerAccessToken">>},
                expiration,
                token,
                {scope, [[{party, PartyID}, {customer, CustomerID}]]}
            ]}
        ],
        [api_key_meta]
    ).

-spec api_key_handler(PartyID :: binary()) -> handler_fun().
api_key_handler(PartyID) ->
    make_handler_fun(
        ?TK_AUTHORITY_APIKEYMGMT,
        [
            {auth, [
                {method, <<"ApiKeyToken">>},
                token,
                {scope, [[{party, PartyID}]]}
            ]}
        ],
        [api_key_meta]
    ).

-type operation_id() :: 'GetByToken'.
-type args() :: tuple().
-type handler_return() :: term() | no_return().
-type handler_fun() :: fun((operation_id(), args()) -> handler_return()).

-spec make_handler_fun(Authority :: binary(), ContextSpec :: any(), MetadataSpec :: any()) -> handler_fun().
make_handler_fun(Authority, ContextSpec, MetadataSpec) ->
    fun
        ('GetByToken', {Token, _}) ->
            AuthData = #token_keeper_AuthData{
                token = Token,
                status = active,
                context = encode_context(get_context(ContextSpec)),
                authority = Authority,
                metadata = get_metadata(MetadataSpec)
            },
            {ok, AuthData};
        ('CreateEphemeral', {ContextFragment, Metadata}) ->
            AuthData = #token_keeper_AuthData{
                token = ?API_TOKEN,
                status = active,
                context = ContextFragment,
                authority = Authority,
                metadata = Metadata
            },
            {ok, AuthData}
    end.

%%

-spec mock_service(_, _) -> _.
mock_service(HandlerFun, SupOrConfig) ->
    start_client(
        capi_ct_helper:mock_services_(
            [
                {
                    token_keeper,
                    {tk_token_keeper_thrift, 'TokenKeeper'},
                    HandlerFun
                }
            ],
            SupOrConfig
        )
    ).

start_client(ServiceURLs) ->
    capi_ct_helper:start_app(token_keeper_client, [
        {service_client, #{
            url => maps:get(token_keeper, ServiceURLs)
        }}
    ]).

%%

get_context(Spec) ->
    Acc0 = bouncer_context_helpers:empty(),
    add_by_spec(Acc0, Spec).

add_by_spec(Acc0, []) ->
    Acc0;
add_by_spec(Acc0, [{user, UserSpec} | Rest]) ->
    add_by_spec(add_user_spec(Acc0, UserSpec), Rest);
add_by_spec(Acc0, [{auth, AuthSpec} | Rest]) ->
    add_by_spec(add_auth_spec(Acc0, AuthSpec), Rest).

add_user_spec(Acc0, UserSpec) ->
    bouncer_context_helpers:add_user(
        assemble_user_fragment(UserSpec),
        Acc0
    ).

add_auth_spec(Acc0, AuthSpec) ->
    bouncer_context_helpers:add_auth(
        assemble_auth_fragment(AuthSpec),
        Acc0
    ).

assemble_user_fragment(UserSpec) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            FragName = get_user_fragment_name(SpecFragment),
            Acc0#{FragName => get_user_fragment_value(SpecFragment)}
        end,
        #{},
        UserSpec
    ).

get_user_fragment_name(Atom) when is_atom(Atom) ->
    Atom;
get_user_fragment_name({Atom, _Spec}) when is_atom(Atom) ->
    Atom.

get_user_fragment_value(id) ->
    ?USER_ID;
get_user_fragment_value({id, ID}) ->
    ID;
get_user_fragment_value(email) ->
    ?USER_EMAIL;
get_user_fragment_value({email, Email}) ->
    Email;
get_user_fragment_value(realm) ->
    #{id => <<"external">>};
get_user_fragment_value({realm, RealmID}) ->
    #{id => RealmID}.

assemble_auth_fragment(AuthSpec) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            FragName = get_auth_fragment_name(SpecFragment),
            Acc0#{FragName => get_auth_fragment_value(SpecFragment)}
        end,
        #{},
        AuthSpec
    ).

get_auth_fragment_name(Atom) when is_atom(Atom) ->
    Atom;
get_auth_fragment_name({Atom, _Spec}) when is_atom(Atom) ->
    Atom.

get_auth_fragment_value(method) ->
    <<"SessionToken">>;
get_auth_fragment_value({method, Method}) ->
    Method;
get_auth_fragment_value(expiration) ->
    make_auth_expiration(unlimited);
get_auth_fragment_value({expiration, Expiration}) ->
    make_auth_expiration(Expiration);
get_auth_fragment_value(token) ->
    #{id => ?STRING};
get_auth_fragment_value({token, ID}) ->
    #{id => ID};
get_auth_fragment_value(scope) ->
    [#{party => #{id => ?PARTY_ID}}];
get_auth_fragment_value({scope, ScopeSpecs}) ->
    lists:foldl(
        fun(ScopeSpec, Acc0) ->
            [assemble_auth_scope_fragment(ScopeSpec) | Acc0]
        end,
        [],
        ScopeSpecs
    ).

assemble_auth_scope_fragment(ScopeSpec) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            FragName = get_auth_scope_fragment_name(SpecFragment),
            Acc0#{FragName => get_auth_scope_fragment_value(SpecFragment)}
        end,
        #{},
        ScopeSpec
    ).

get_auth_scope_fragment_name(Atom) when is_atom(Atom) ->
    Atom;
get_auth_scope_fragment_name({Atom, _Spec}) when is_atom(Atom) ->
    Atom.

get_auth_scope_fragment_value(party) ->
    #{id => ?PARTY_ID};
get_auth_scope_fragment_value({Name, EntityID}) when is_atom(Name) ->
    #{id => EntityID}.

get_metadata(MetadataSpec) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            deep_merge(Acc0, make_metadata_by_spec(SpecFragment))
        end,
        #{},
        MetadataSpec
    ).

make_metadata_by_spec(user_session_meta) ->
    #{
        ?TK_META_USER_ID => ?USER_ID,
        ?TK_META_USER_EMAIL => ?USER_EMAIL
    };
make_metadata_by_spec(api_key_meta) ->
    #{
        ?TK_META_PARTY_ID => ?PARTY_ID
    };
make_metadata_by_spec({consumer_meta, Cons}) ->
    #{
        ?TK_META_TOKEN_CONSUMER => Cons
    }.

encode_context(Context) ->
    #bctx_ContextFragment{
        type = v1_thrift_binary,
        content = encode_context_content(Context)
    }.

encode_context_content(Context) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Codec = thrift_strict_binary_codec:new(),
    case thrift_strict_binary_codec:write(Codec, Type, Context) of
        {ok, Codec1} ->
            thrift_strict_binary_codec:close(Codec1)
    end.

%% Internal functions

make_auth_expiration(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
make_auth_expiration(unlimited) ->
    undefined.

deep_merge(L, R) ->
    maps:fold(
        fun(K, V0, Acc) ->
            case Acc of
                #{K := V1} ->
                    Acc#{K => deep_merge(V1, V0)};
                _ ->
                    Acc#{K => V0}
            end
        end,
        L,
        R
    ).
