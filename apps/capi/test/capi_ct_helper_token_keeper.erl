-module(capi_ct_helper_token_keeper).

-include_lib("token_keeper_proto/include/tk_token_keeper_thrift.hrl").
-include_lib("token_keeper_proto/include/tk_context_thrift.hrl").
-include_lib("capi_token_keeper_data.hrl").

-type sup_or_config() :: capi_ct_helper:sup_or_config().
-type app_name() :: capi_ct_helper:app_name().
-type token_handler() :: fun(('GetByToken', tuple()) -> term() | no_return()).

-export([mock_token/2]).
-export([mock_not_found/1]).
-export([mock_user_session_token/1]).
-export([mock_api_key_token/2]).
-export([mock_invoice_access_token/3]).
-export([mock_invoice_template_access_token/3]).
-export([mock_customer_access_token/3]).
-export([make_token_handler/1]).

-spec mock_token(token_handler(), sup_or_config()) -> list(app_name()).
mock_token(HandlerFun, SupOrConfig) ->
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

-spec mock_not_found(sup_or_config()) -> list(app_name()).
mock_not_found(SupOrConfig) ->
    mock_token(fun('GetByToken', {_, _}) -> {throwing, #token_keeper_AuthDataNotFound{}} end, SupOrConfig).

-spec mock_user_session_token(sup_or_config()) -> list(app_name()).
mock_user_session_token(SupOrConfig) ->
    Handler = make_token_handler(fun(TokenInfo, Token) ->
        UserParams = #{
            id => uac_authorizer_jwt:get_subject_id(TokenInfo),
            realm => #{id => <<"external">>},
            email => uac_authorizer_jwt:get_subject_email(TokenInfo)
        },
        AuthParams = #{
            method => <<"SessionToken">>,
            expiration => posix_to_rfc3339(uac_authorizer_jwt:get_expires_at(TokenInfo)),
            token => #{id => uac_authorizer_jwt:get_token_id(TokenInfo)}
        },
        {ok, #token_keeper_AuthData{
            token = Token,
            status = active,
            context = create_bouncer_context(AuthParams, UserParams),
            authority = ?TK_AUTHORITY_KEYCLOAK,
            metadata = user_session_metadata(TokenInfo)
        }}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_api_key_token(binary(), sup_or_config()) -> list(app_name()).
mock_api_key_token(PartyID, SupOrConfig) ->
    Handler = make_token_handler(fun(TokenInfo, Token) ->
        AuthParams = #{
            method => <<"ApiKeyToken">>,
            token => #{id => uac_authorizer_jwt:get_token_id(TokenInfo)},
            scope => [#{party => #{id => PartyID}}]
        },
        {ok, #token_keeper_AuthData{
            token = Token,
            status = active,
            context = create_bouncer_context(AuthParams),
            authority = ?TK_AUTHORITY_APIKEYMGMT,
            metadata = api_key_metadata(TokenInfo)
        }}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_invoice_access_token(binary(), binary(), sup_or_config()) -> list(app_name()).
mock_invoice_access_token(PartyID, InvoiceID, SupOrConfig) ->
    Handler = make_token_handler(fun(TokenInfo, Token) ->
        AuthParams = #{
            method => <<"InvoiceAccessToken">>,
            expiration => posix_to_rfc3339(uac_authorizer_jwt:get_expires_at(TokenInfo)),
            token => #{id => uac_authorizer_jwt:get_token_id(TokenInfo)},
            scope => [#{party => #{id => PartyID}, invoice => #{id => InvoiceID}}]
        },
        {ok, #token_keeper_AuthData{
            token = Token,
            status = active,
            context = create_bouncer_context(AuthParams),
            authority = <<"com.rbkmoney.capi">>,
            metadata = combine_metadata(TokenInfo, [fun api_key_metadata/1, fun consumer_metadata/1])
        }}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_invoice_template_access_token(binary(), binary(), sup_or_config()) -> list(app_name()).
mock_invoice_template_access_token(PartyID, InvoiceTemplateID, SupOrConfig) ->
    Handler = make_token_handler(fun(TokenInfo, Token) ->
        AuthParams = #{
            method => <<"InvoiceAccessToken">>,
            expiration => posix_to_rfc3339(uac_authorizer_jwt:get_expires_at(TokenInfo)),
            token => #{id => uac_authorizer_jwt:get_token_id(TokenInfo)},
            scope => [#{party => #{id => PartyID}, invoice_template => #{id => InvoiceTemplateID}}]
        },
        {ok, #token_keeper_AuthData{
            token = Token,
            status = active,
            context = create_bouncer_context(AuthParams),
            authority = <<"com.rbkmoney.capi">>,
            metadata = api_key_metadata(TokenInfo)
        }}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_customer_access_token(binary(), binary(), sup_or_config()) -> list(app_name()).
mock_customer_access_token(PartyID, CustomerID, SupOrConfig) ->
    Handler = make_token_handler(fun(TokenInfo, Token) ->
        AuthParams = #{
            method => <<"CustomerAccessToken">>,
            expiration => posix_to_rfc3339(uac_authorizer_jwt:get_expires_at(TokenInfo)),
            token => #{id => uac_authorizer_jwt:get_token_id(TokenInfo)},
            scope => [#{party => #{id => PartyID}, customer => #{id => CustomerID}}]
        },
        {ok, #token_keeper_AuthData{
            token = Token,
            status = active,
            context = create_bouncer_context(AuthParams),
            authority = <<"com.rbkmoney.capi">>,
            metadata = api_key_metadata(TokenInfo)
        }}
    end),
    mock_token(Handler, SupOrConfig).

%%

-spec make_token_handler(function()) -> token_handler().
make_token_handler(Handler) ->
    fun('GetByToken', {Token, _}) ->
        case uac_authorizer_jwt:verify(Token, #{}) of
            {ok, TokenInfo} ->
                Handler(TokenInfo, Token);
            {error, Error} ->
                _ = logger:warning("Token-keeper ct-helper could not verify the token: ~p", [Error]),
                {throwing, #token_keeper_AuthDataNotFound{}}
        end
    end.

%%

combine_metadata(TokenInfo, Selectors) ->
    lists:foldl(fun(Selector, Meta) -> maps:merge(Meta, Selector(TokenInfo)) end, #{}, Selectors).

user_session_metadata(TokenInfo) ->
    genlib_map:compact(#{
        ?TK_META_USER_ID => uac_authorizer_jwt:get_subject_id(TokenInfo),
        ?TK_META_USER_EMAIL => uac_authorizer_jwt:get_subject_email(TokenInfo)
    }).

api_key_metadata(TokenInfo) ->
    genlib_map:compact(#{
        ?TK_META_PARTY_ID => uac_authorizer_jwt:get_subject_id(TokenInfo)
    }).

consumer_metadata(TokenInfo) ->
    genlib_map:compact(#{
        ?TK_META_TOKEN_CONSUMER => uac_authorizer_jwt:get_claim(<<"cons">>, TokenInfo, undefined)
    }).

%%

create_bouncer_context(AuthParams) ->
    Fragment0 = bouncer_context_helpers:make_auth_fragment(AuthParams),
    encode_context(Fragment0).

create_bouncer_context(AuthParams, UserParams) ->
    Fragment0 = bouncer_context_helpers:make_auth_fragment(AuthParams),
    Fragment1 = bouncer_context_helpers:add_user(UserParams, Fragment0),
    encode_context(Fragment1).
%%

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

%%

posix_to_rfc3339(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
posix_to_rfc3339(unlimited) ->
    undefined.
