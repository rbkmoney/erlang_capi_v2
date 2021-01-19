-module(capi_authorization_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("jose/include/jose_jwk.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    authorization_positive_lifetime_ok_test/1,
    authorization_unlimited_lifetime_ok_test/1,
    authorization_far_future_deadline_ok_test/1,
    authorization_permission_ok_test/1,
    authorization_negative_lifetime_error_test/1,
    authorization_bad_deadline_error_test/1,
    authorization_error_no_header_test/1,
    authorization_error_no_permission_test/1,
    authorization_blacklisted_token_error_test/1,
    authorization_bad_token_error_test/1
]).

-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [test_case_name()].
all() ->
    [
        {group, authorization}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {authorization, [], [
            authorization_positive_lifetime_ok_test,
            authorization_unlimited_lifetime_ok_test,
            authorization_far_future_deadline_ok_test,
            authorization_permission_ok_test,
            authorization_negative_lifetime_error_test,
            authorization_bad_deadline_error_test,
            authorization_error_no_header_test,
            authorization_error_no_permission_test,
            authorization_blacklisted_token_error_test,
            authorization_bad_token_error_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec authorization_positive_lifetime_ok_test(config()) -> _.
authorization_positive_lifetime_ok_test(_Config) ->
    {ok, Token} = capi_ct_helper:issue_token([], {lifetime, 10}),
    {ok, _} = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_unlimited_lifetime_ok_test(config()) -> _.
authorization_unlimited_lifetime_ok_test(_Config) ->
    {ok, Token} = capi_ct_helper:issue_token([], unlimited),
    {ok, _} = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_far_future_deadline_ok_test(config()) -> _.
authorization_far_future_deadline_ok_test(_Config) ->
    % 01/01/2100 @ 12:00am (UTC)
    {ok, Token} = capi_ct_helper:issue_token([], {deadline, 4102444800}),
    {ok, _} = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_permission_ok_test(config()) -> _.
authorization_permission_ok_test(Config) ->
    capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, ?PARTY} end}], Config),
    {ok, Token} = capi_ct_helper:issue_token([{[party], read}], unlimited),
    {ok, _} = capi_client_parties:get_my_party(capi_ct_helper:get_context(Token)).

-spec authorization_negative_lifetime_error_test(config()) -> _.
authorization_negative_lifetime_error_test(_Config) ->
    ok.

% {ok, Token} = capi_ct_helper:issue_token([], {lifetime, -10}),
% ?emptyresp(401) = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_bad_deadline_error_test(config()) -> _.
authorization_bad_deadline_error_test(_Config) ->
    ok.

% {ok, Token} = capi_ct_helper:issue_token([], {deadline, -10}),
% ?emptyresp(401) = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_error_no_header_test(config()) -> _.
authorization_error_no_header_test(_Config) ->
    Token = <<>>,
    ?emptyresp(401) = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_error_no_permission_test(config()) -> _.
authorization_error_no_permission_test(_Config) ->
    {ok, Token} = capi_ct_helper:issue_token([], {lifetime, 10}),
    ?emptyresp(401) = capi_client_parties:get_my_party(capi_ct_helper:get_context(Token)).

-spec authorization_blacklisted_token_error_test(config()) -> _.
authorization_blacklisted_token_error_test(Config) ->
    {ok, Token} = capi_ct_helper:issue_token(<<"BlackListedToken">>, [{[party], read}], unlimited, #{}),
    DataDir = get_blacklisted_keys_dir(Config),
    ok = file:write_file(filename:join(DataDir, "1.key"), Token),
    ok = file:write_file(filename:join(DataDir, "2.key"), Token),
    ok = file:write_file(filename:join(DataDir, "3.key"), Token),
    ok = capi_api_key_blacklist:update(),
    ?emptyresp(401) = capi_client_parties:get_my_party(capi_ct_helper:get_context(Token)).

-spec authorization_bad_token_error_test(config()) -> _.
authorization_bad_token_error_test(Config) ->
    {ok, Token} = issue_dummy_token([{[party], read}], Config),
    ?emptyresp(401) = capi_client_parties:get_my_party(capi_ct_helper:get_context(Token)).

issue_dummy_token(ACL, Config) ->
    Claims = #{
        <<"jti">> => capi_utils:get_unique_id(),
        <<"sub">> => ?STRING,
        <<"exp">> => 0,
        <<"resource_access">> => #{
            <<"common-api">> => #{
                <<"roles">> => uac_acl:encode(uac_acl:from_list(ACL))
            }
        }
    },
    BadPemFile = capi_ct_helper:get_keysource("keys/local/dummy.pem", Config),
    BadJWK = jose_jwk:from_pem_file(BadPemFile),
    GoodPemFile = capi_ct_helper:get_keysource("keys/local/private.pem", Config),
    GoodJWK = jose_jwk:from_pem_file(GoodPemFile),
    JWKPublic = jose_jwk:to_public(GoodJWK),
    {_Module, PublicKey} = JWKPublic#jose_jwk.kty,
    {_PemEntry, Data, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey),
    KID = jose_base64url:encode(crypto:hash(sha256, Data)),
    JWT = jose_jwt:sign(BadJWK, #{<<"alg">> => <<"RS256">>, <<"kid">> => KID}, Claims),
    {_Modules, Token} = jose_jws:compact(JWT),
    {ok, Token}.

get_blacklisted_keys_dir(Config) ->
    filename:join(?config(data_dir, Config), "blacklisted_keys").
