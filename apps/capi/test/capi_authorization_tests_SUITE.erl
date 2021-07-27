-module(capi_authorization_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

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
    authorization_error_no_header_test/1,
    authorization_error_no_permission_test/1,
    authorization_bad_token_error_test/1
]).

-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, authorization}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {authorization, [], [
            authorization_error_no_header_test,
            authorization_error_no_permission_test,
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
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_tk:mock_service(capi_ct_helper_tk:user_session_handler(), SupPid),
    [{group_apps, Apps1}, {group_test_sup, SupPid} | Config].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = capi_utils:maybe(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(authorization_error_no_permission_test, C) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps0 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_forbidden(), SupPid),
    [{test_apps, Apps0}, {test_sup, SupPid} | C];
init_per_testcase(_Name, C) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps0 = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{test_apps, Apps0}, {test_sup, SupPid} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec authorization_error_no_header_test(config()) -> _.
authorization_error_no_header_test(_Config) ->
    Token = <<>>,
    ?emptyresp(401) = capi_client_categories:get_categories(capi_ct_helper:get_context(Token)).

-spec authorization_error_no_permission_test(config()) -> _.
authorization_error_no_permission_test(_Config) ->
    Token = capi_ct_helper:issue_token(unlimited),
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
