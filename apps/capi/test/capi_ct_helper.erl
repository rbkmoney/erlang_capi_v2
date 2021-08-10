-module(capi_ct_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([init_suite/2]).
-export([init_suite/3]).
-export([start_app/1]).
-export([start_app/2]).
-export([start_capi/1]).
-export([start_capi/2]).
-export([get_context/1]).
-export([get_context/2]).
-export([get_keysource/2]).
-export([start_mocked_service_sup/1]).
-export([stop_mocked_service_sup/1]).
-export([mock_services/2]).
-export([mock_services_/2]).
-export([get_lifetime/0]).
-export([map_to_flat/1]).

-define(CAPI_IP, "::").
-define(CAPI_PORT, 8080).
-define(CAPI_HOST_NAME, "localhost").
-define(CAPI_URL, ?CAPI_HOST_NAME ++ ":" ++ integer_to_list(?CAPI_PORT)).

-define(TK_META_NS_KEYCLOAK, <<"test.rbkmoney.keycloak">>).
-define(TK_META_NS_APIKEYMGMT, <<"test.rbkmoney.apikeymgmt">>).

%%
-type config() :: [{atom(), any()}].
-type app_name() :: atom().

-spec init_suite(module(), config()) -> config().
init_suite(Module, Config) ->
    init_suite(Module, Config, []).

-spec init_suite(module(), config(), any()) -> config().
init_suite(Module, Config, CapiEnv) ->
    SupPid = start_mocked_service_sup(Module),
    WoodyApp = start_app(woody),
    ServiceURLs = mock_services_(
        [
            {
                'Repository',
                {dmsl_domain_config_thrift, 'Repository'},
                fun
                    ('Checkout', _) -> {ok, ?SNAPSHOT};
                    ('PullRange', _) -> {ok, #{}}
                end
            }
        ],
        SupPid
    ),
    DmtApp = start_app(dmt_client, [
        {max_cache_size, #{}},
        {service_urls, ServiceURLs},
        {cache_update_interval, 50000}
    ]),
    CapiApp = start_capi(Config, CapiEnv),
    BouncerApp = capi_ct_helper_bouncer:mock_client(SupPid),
    Apps = lists:reverse([WoodyApp, DmtApp, CapiApp, BouncerApp]),
    [{apps, Apps}, {suite_test_sup, SupPid} | Config].

-spec start_app(app_name()) -> [app_name()].
start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);
start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) -> [app_name()].
start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_capi(config()) -> [app_name()].
start_capi(Config) ->
    start_capi(Config, []).

-spec start_capi(config(), list()) -> [app_name()].
start_capi(Config, ExtraEnv) ->
    JwkPublSource = {json, {file, get_keysource("keys/local/jwk.publ.json", Config)}},
    JwkPrivSource = {json, {file, get_keysource("keys/local/jwk.priv.json", Config)}},
    BlacklistedKeysDir = get_blacklisted_keys_dir(Config),
    [_] = lists:delete(file:make_dir(BlacklistedKeysDir), [ok, {error, eexist}]),
    CapiEnv =
        ExtraEnv ++
            [
                {ip, ?CAPI_IP},
                {port, ?CAPI_PORT},
                {service_type, real},
                {bouncer_ruleset_id, ?TEST_RULESET_ID},
                {access_conf, #{
                    jwt => #{
                        keyset => #{
                            capi => #{
                                source => {pem_file, get_keysource("keys/local/private.pem", Config)},
                                metadata => #{
                                    auth_method => user_session_token,
                                    user_realm => <<"external">>
                                }
                            }
                        }
                    }
                }},
                {api_key_blacklist, #{
                    % milliseconds
                    update_interval => 50000,
                    blacklisted_keys_dir => BlacklistedKeysDir
                }},
                {lechiffre_opts, #{
                    encryption_source => JwkPublSource,
                    decryption_sources => [JwkPrivSource]
                }},
                {auth_config, #{
                    metadata_namespaces => #{
                        user_namespace => ?TK_META_NS_KEYCLOAK,
                        party_namespace => ?TK_META_NS_APIKEYMGMT
                    }
                }}
            ],
    start_app(capi, CapiEnv).

-spec get_keysource(_, config()) -> _.
get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).

-spec get_context(binary()) -> capi_client_lib:context().
get_context(Token) ->
    get_context(Token, undefined).

-spec get_context(binary(), capi_client_lib:deadline()) -> capi_client_lib:context().
get_context(Token, Deadline) ->
    capi_client_lib:get_context(
        ?CAPI_URL,
        Token,
        10000,
        ipv4,
        capi_client_lib:default_event_handler(),
        Deadline
    ).

% TODO move it to `capi_dummy_service`, looks more appropriate

-spec start_mocked_service_sup(module()) -> pid().
start_mocked_service_sup(Module) ->
    {ok, SupPid} = supervisor:start_link(Module, []),
    _ = unlink(SupPid),
    SupPid.

-spec stop_mocked_service_sup(pid()) -> _.
stop_mocked_service_sup(SupPid) ->
    proc_lib:stop(SupPid, shutdown, 5000).

-spec mock_services(_, _) -> _.
mock_services(Services, SupOrConfig) ->
    {PartyClientServices, Other} = lists:partition(
        fun
            ({party_management, _}) -> true;
            (_) -> false
        end,
        Services
    ),
    {BenderClientServices, WoodyServices} = lists:partition(
        fun
            ({generator, _}) -> true;
            (_) -> false
        end,
        Other
    ),
    _ = start_party_client(mock_services_(PartyClientServices, SupOrConfig)),
    _ = start_bender_client(mock_services_(BenderClientServices, SupOrConfig)),
    start_woody_client(mock_services_(WoodyServices, SupOrConfig)).

start_party_client(Services) ->
    start_app(party_client, [{services, Services}]).

start_bender_client(Services) ->
    start_app(bender_client, [{services, Services}]).

start_woody_client(Services) ->
    start_app(capi_woody_client, [{services, Services}]).

-spec mock_services_(_, _) -> _.
% TODO need a better name
mock_services_([], _Config) ->
    #{};
mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));
mock_services_(Services, SupPid) when is_pid(SupPid) ->
    {ok, IP} = inet:parse_address(?CAPI_IP),
    lists:foldl(
        fun(Service, Acc) ->
            Name = get_service_name(Service),
            ServerID = {dummy, Name},
            WoodyOpts = #{
                ip => IP,
                port => 0,
                event_handler => scoper_woody_event_handler,
                handlers => [mock_service_handler(Service)]
            },
            ChildSpec = woody_server:child_spec(ServerID, WoodyOpts),
            {ok, _} = supervisor:start_child(SupPid, ChildSpec),
            {_IP, Port} = woody_server:get_addr(ServerID, WoodyOpts),
            Acc#{Name => make_url(Name, Port)}
        end,
        #{},
        Services
    ).

get_service_name({generator, _}) ->
    'Generator';
get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({generator, Fun}) ->
    mock_service_handler('Generator', {bender_thrift, 'Generator'}, Fun);
mock_service_handler({party_management, Fun}) ->
    mock_service_handler(party_management, {dmsl_payment_processing_thrift, 'PartyManagement'}, Fun);
mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, capi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {capi_dummy_service, #{function => Fun}}}}.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?CAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

-spec get_lifetime() -> map().
get_lifetime() ->
    get_lifetime(0, 0, 7).

get_lifetime(YY, MM, DD) ->
    #{
        <<"years">> => YY,
        <<"months">> => MM,
        <<"days">> => DD
    }.

-spec map_to_flat(#{}) -> FlatRepresentation :: #{}.
map_to_flat(Value) ->
    Prefix = [],
    Acc = #{},
    {_, FlatMap} = maps:fold(fun to_flat/3, {Prefix, Acc}, Value),
    FlatMap.

to_flat(Key, #{} = Value, {Prefix, Acc}) ->
    {_Prefix2, AccOut} = maps:fold(fun to_flat/3, {[Key | Prefix], Acc}, Value),
    {Prefix, AccOut};
to_flat(Key, Value, {Prefix, Acc}) ->
    add_prefix(Key, Value, {Prefix, Acc}).

add_prefix(Key, Value, {Prefix, Acc}) ->
    FlatKey = lists:reverse([Key | Prefix]),
    {Prefix, Acc#{FlatKey => Value}}.

get_blacklisted_keys_dir(Config) ->
    filename:join(?config(data_dir, Config), "blacklisted_keys").
