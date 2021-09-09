-module(capi_ct_helper_token_keeper).

-include_lib("capi_dummy_data.hrl").
-include_lib("token_keeper_proto/include/tk_token_keeper_thrift.hrl").
-include_lib("token_keeper_proto/include/tk_context_thrift.hrl").
-include_lib("capi_token_keeper_data.hrl").

-define(PARTY_ID, ?STRING).
-define(USER_ID, ?STRING).
-define(USER_EMAIL, <<"bla@bla.ru">>).
-define(TOKEN_LIFETIME, 259200).

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
    Handler = make_token_handler(fun() ->
        UserParams = #{
            id => ?USER_ID,
            realm => #{id => <<"external">>},
            email => ?USER_EMAIL
        },
        AuthParams = #{
            method => <<"SessionToken">>,
            expiration => posix_to_rfc3339(lifetime_to_expiration(?TOKEN_LIFETIME)),
            token => #{id => ?STRING}
        },
        {?TK_AUTHORITY_KEYCLOAK, create_bouncer_context(AuthParams, UserParams), user_session_metadata()}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_api_key_token(binary(), sup_or_config()) -> list(app_name()).
mock_api_key_token(PartyID, SupOrConfig) ->
    Handler = make_token_handler(fun() ->
        AuthParams = #{
            method => <<"ApiKeyToken">>,
            token => #{id => ?STRING},
            scope => [#{party => #{id => PartyID}}]
        },
        {?TK_AUTHORITY_APIKEYMGMT, create_bouncer_context(AuthParams), api_key_metadata()}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_invoice_access_token(binary(), binary(), sup_or_config()) -> list(app_name()).
mock_invoice_access_token(PartyID, InvoiceID, SupOrConfig) ->
    Handler = make_token_handler(fun() ->
        AuthParams = #{
            method => <<"InvoiceAccessToken">>,
            expiration => posix_to_rfc3339(lifetime_to_expiration(?TOKEN_LIFETIME)),
            token => #{id => ?STRING},
            scope => [#{party => #{id => PartyID}, invoice => #{id => InvoiceID}}]
        },
        {<<"com.rbkmoney.capi">>, create_bouncer_context(AuthParams), [api_key_metadata(), consumer_metadata(<<"client">>)]}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_invoice_template_access_token(binary(), binary(), sup_or_config()) -> list(app_name()).
mock_invoice_template_access_token(PartyID, InvoiceTemplateID, SupOrConfig) ->
    Handler = make_token_handler(fun() ->
        AuthParams = #{
            method => <<"InvoiceAccessToken">>,
            expiration => posix_to_rfc3339(unlimited),
            token => #{id => ?STRING},
            scope => [#{party => #{id => PartyID}, invoice_template => #{id => InvoiceTemplateID}}]
        },
        {<<"com.rbkmoney.capi">>, create_bouncer_context(AuthParams), api_key_metadata()}
    end),
    mock_token(Handler, SupOrConfig).

-spec mock_customer_access_token(binary(), binary(), sup_or_config()) -> list(app_name()).
mock_customer_access_token(PartyID, CustomerID, SupOrConfig) ->
    Handler = make_token_handler(fun() ->
        AuthParams = #{
            method => <<"CustomerAccessToken">>,
            expiration => posix_to_rfc3339(lifetime_to_expiration(?TOKEN_LIFETIME)),
            token => #{id => ?STRING},
            scope => [#{party => #{id => PartyID}, customer => #{id => CustomerID}}]
        },
        {<<"com.rbkmoney.capi">>, create_bouncer_context(AuthParams), api_key_metadata()}
    end),
    mock_token(Handler, SupOrConfig).

%%

-spec make_token_handler(function()) -> token_handler().
make_token_handler(Handler) ->
    fun
        ('GetByToken', {Token, _}) ->
            {Authority, ContextFragment, Metadata} = Handler(),
            AuthData = #token_keeper_AuthData{
                token = Token,
                status = active,
                context = ContextFragment,
                authority = Authority,
                metadata = combine_metadata(Metadata)
            },
            {ok, AuthData};
        ('CreateEphemeral', {ContextFragment, Metadata}) ->
            AuthData = #token_keeper_AuthData{
                token = ?API_TOKEN,
                status = active,
                context = ContextFragment,
                authority = ?TK_AUTHORITY_APIKEYMGMT,
                metadata = Metadata
            },
            {ok, AuthData}
    end.

%%

combine_metadata(MetadataParts) when is_list(MetadataParts) ->
    lists:foldl(fun(Part, Acc) -> maps:merge(Acc, Part) end, #{}, MetadataParts);
combine_metadata(#{} = FullMetadata) ->
    FullMetadata.

user_session_metadata() ->
    genlib_map:compact(#{
        ?TK_META_USER_ID => ?USER_ID,
        ?TK_META_USER_EMAIL => ?USER_EMAIL
    }).

api_key_metadata() ->
    genlib_map:compact(#{
        ?TK_META_PARTY_ID => ?PARTY_ID
    }).

consumer_metadata(Consumer) ->
    genlib_map:compact(#{
        ?TK_META_TOKEN_CONSUMER => Consumer
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

lifetime_to_expiration(Lt) when is_integer(Lt) ->
    genlib_time:unow() + Lt.

posix_to_rfc3339(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second);
posix_to_rfc3339(unlimited) ->
    undefined.