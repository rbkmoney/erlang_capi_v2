-module(capi_ct_helper_tk).

-include_lib("token_keeper_proto/include/tk_token_keeper_thrift.hrl").
-include_lib("token_keeper_proto/include/tk_context_thrift.hrl").

-define(TK_META_NS_KEYCLOAK, <<"com.rbkmoney.keycloak">>).
-define(TK_META_NS_APIKEYMGMT, <<"com.rbkmoney.apikeymgmt">>).

-export([mock_service/2]).

-export([not_found_handler/0]).

-export([user_session_handler/0]).
-export([api_key_handler/1]).
-export([invoice_access_token/2]).
-export([invoice_template_access_token/2]).
-export([customer_access_token/2]).

-export([make_handler_fun/3]).

-spec not_found_handler() -> fun().
not_found_handler() ->
    fun('GetByToken', {_, _}) ->
        woody_error:raise(business, #token_keeper_AuthDataNotFound{})
    end.

-spec user_session_handler() -> fun().
user_session_handler() ->
    make_handler_fun(
        ?TK_META_NS_KEYCLOAK,
        [
            {user, [id, email, realm]},
            {auth, [{method, <<"SessionToken">>}, expiration, token]}
        ],
        [user_session_meta]
    ).

-spec invoice_access_token(PartyID :: binary(), InvoiceID :: binary()) -> fun().
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
        [api_key_meta]
    ).

-spec invoice_template_access_token(PartyID :: binary(), InvoiceTmeplateID :: binary()) -> fun().
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

-spec customer_access_token(PartyID :: binary(), CustomerID :: binary()) -> fun().
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

-spec api_key_handler(PartyID :: binary()) -> fun().
api_key_handler(PartyID) ->
    make_handler_fun(
        ?TK_META_NS_APIKEYMGMT,
        [
            {auth, [
                {method, <<"ApiKeyToken">>},
                token,
                {scope, [[{party, PartyID}]]}
            ]}
        ],
        [api_key_meta]
    ).

-spec make_handler_fun(Authority :: binary(), ContextSpec :: any(), MetadataSpec :: any()) -> fun().
make_handler_fun(Authority, ContextSpec, MetadataSpec) ->
    fun('GetByToken', {Token, _}) ->
        case uac_authorizer_jwt:verify(Token, #{}) of
            {ok, TokenInfo} ->
                AuthData = #token_keeper_AuthData{
                    token = Token,
                    status = active,
                    context = encode_context(get_context(TokenInfo, ContextSpec)),
                    authority = Authority,
                    metadata = get_metadata(TokenInfo, MetadataSpec)
                },
                {ok, AuthData};
            {error, Error} ->
                _ = logger:warning("Token-keeper ct-helper could not verify the token: ~p", [Error]),
                woody_error:raise(business, #token_keeper_AuthDataNotFound{})
        end
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
        }},
        {namespace_mappings, #{
            user_session => ?TK_META_NS_KEYCLOAK,
            api_key => ?TK_META_NS_APIKEYMGMT
        }}
    ]).

%%

get_context(TokenInfo, Spec) ->
    Acc0 = bouncer_context_helpers:empty(),
    add_by_spec(Acc0, TokenInfo, Spec).

add_by_spec(Acc0, _TokenInfo, []) ->
    Acc0;
add_by_spec(Acc0, TokenInfo, [{user, UserSpec} | Rest]) ->
    add_by_spec(add_user_spec(Acc0, UserSpec, TokenInfo), TokenInfo, Rest);
add_by_spec(Acc0, TokenInfo, [{auth, AuthSpec} | Rest]) ->
    add_by_spec(add_auth_spec(Acc0, AuthSpec, TokenInfo), TokenInfo, Rest).

add_user_spec(Acc0, UserSpec, TokenInfo) ->
    bouncer_context_helpers:add_user(
        assemble_user_fragment(UserSpec, TokenInfo),
        Acc0
    ).

add_auth_spec(Acc0, AuthSpec, TokenInfo) ->
    bouncer_context_helpers:add_auth(
        assemble_auth_fragment(AuthSpec, TokenInfo),
        Acc0
    ).

assemble_user_fragment(UserSpec, TokenInfo) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            FragName = get_user_fragment_name(SpecFragment, TokenInfo),
            Acc0#{FragName => get_user_fragment_value(SpecFragment, TokenInfo)}
        end,
        #{},
        UserSpec
    ).

get_user_fragment_name(Atom, _TokenInfo) when is_atom(Atom) ->
    Atom;
get_user_fragment_name({Atom, _Spec}, _TokenInfo) when is_atom(Atom) ->
    Atom.

get_user_fragment_value(id, TokenInfo) ->
    uac_authorizer_jwt:get_subject_id(TokenInfo);
get_user_fragment_value({id, ID}, _TokenInfo) ->
    ID;
get_user_fragment_value(email, TokenInfo) ->
    uac_authorizer_jwt:get_subject_email(TokenInfo);
get_user_fragment_value({email, Email}, _TokenInfo) ->
    Email;
get_user_fragment_value(realm, _TokenInfo) ->
    #{id => <<"external">>};
get_user_fragment_value({realm, RealmID}, _TokenInfo) ->
    #{id => RealmID}.

assemble_auth_fragment(AuthSpec, TokenInfo) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            FragName = get_auth_fragment_name(SpecFragment, TokenInfo),
            Acc0#{FragName => get_auth_fragment_value(SpecFragment, TokenInfo)}
        end,
        #{},
        AuthSpec
    ).

get_auth_fragment_name(Atom, _TokenInfo) when is_atom(Atom) ->
    Atom;
get_auth_fragment_name({Atom, _Spec}, _TokenInfo) when is_atom(Atom) ->
    Atom.

get_auth_fragment_value(method, _TokenInfo) ->
    <<"SessionToken">>;
get_auth_fragment_value({method, Method}, _TokenInfo) ->
    Method;
get_auth_fragment_value(expiration, TokenInfo) ->
    Expiration = uac_authorizer_jwt:get_expires_at(TokenInfo),
    make_auth_expiration(Expiration);
get_auth_fragment_value({expiration, Expiration}, _TokenInfo) ->
    make_auth_expiration(Expiration);
get_auth_fragment_value(token, TokenInfo) ->
    #{id => uac_authorizer_jwt:get_token_id(TokenInfo)};
get_auth_fragment_value({token, ID}, _TokenInfo) ->
    #{id => ID};
get_auth_fragment_value(scope, TokenInfo) ->
    [#{party => #{id => uac_authorizer_jwt:get_subject_id(TokenInfo)}}];
get_auth_fragment_value({scope, ScopeSpecs}, TokenInfo) ->
    lists:foldl(
        fun(ScopeSpec, Acc0) ->
            [assemble_auth_scope_fragment(ScopeSpec, TokenInfo) | Acc0]
        end,
        [],
        ScopeSpecs
    ).

assemble_auth_scope_fragment(ScopeSpec, TokenInfo) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            FragName = get_auth_scope_fragment_name(SpecFragment, TokenInfo),
            Acc0#{FragName => get_auth_scope_fragment_value(SpecFragment, TokenInfo)}
        end,
        #{},
        ScopeSpec
    ).

get_auth_scope_fragment_name(Atom, _TokenInfo) when is_atom(Atom) ->
    Atom;
get_auth_scope_fragment_name({Atom, _Spec}, _TokenInfo) when is_atom(Atom) ->
    Atom.

get_auth_scope_fragment_value(party, TokenInfo) ->
    #{id => uac_authorizer_jwt:get_subject_id(TokenInfo)};
get_auth_scope_fragment_value({Name, EntityID}, _TokenInfo) when is_atom(Name) ->
    #{id => EntityID}.

get_metadata(TokenInfo, MetadataSpec) ->
    lists:foldl(
        fun(SpecFragment, Acc0) ->
            maps:merge(Acc0, get_metadata_by_spec(SpecFragment, TokenInfo))
        end,
        #{},
        MetadataSpec
    ).

get_metadata_by_spec(user_session_meta, TokenInfo) ->
    #{
        ?TK_META_NS_KEYCLOAK => genlib_map:compact(#{
            <<"user_id">> => uac_authorizer_jwt:get_subject_id(TokenInfo),
            <<"user_email">> => uac_authorizer_jwt:get_subject_email(TokenInfo)
        })
    };
get_metadata_by_spec(api_key_meta, TokenInfo) ->
    #{
        ?TK_META_NS_APIKEYMGMT => #{
            <<"party_id">> => uac_authorizer_jwt:get_subject_id(TokenInfo)
        }
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
