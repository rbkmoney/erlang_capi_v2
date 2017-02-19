-module(capi_authorizer_jwt).

%%

-export([get_child_spec/1]).
-export([init/1]).

-export([store_key/2]).

-export([issue/3]).
-export([verify/1]).

%%

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jwt.hrl").

-type keyname()    :: term().
-type kid()        :: binary().
-type key()        :: #jose_jwk{}.
-type token()      :: binary().
-type claims()     :: #{binary() => term()}.
-type subject()    :: {subject_id(), capi_acl:t()}.
-type subject_id() :: binary().
-type t()          :: {subject(), claims()}.

-export_type([t/0]).
-export_type([subject/0]).
-export_type([claims/0]).

%%

-type keyset() :: #{keyname() => keysource()}.
-type options() :: #{
    keyset => keyset()
}.

-type keysource() ::
    {pem_file, file:filename()}.

-spec get_child_spec(options()) ->
    supervisor:child_spec() | no_return().

get_child_spec(Options) ->
    #{
        id => ?MODULE,
        start => {supervisor, start_link, [?MODULE, parse_options(Options)]},
        type => supervisor
    }.

parse_options(Options) ->
    Keyset = maps:get(keyset, Options, #{}),
    _ = is_map(Keyset) orelse exit({invalid_option, keyset, Keyset}),
    _ = genlib_map:foreach(
        fun (K, V) ->
            is_keysource(V) orelse exit({invalid_option, K, V})
        end,
        Keyset
    ),
    Keyset.

is_keysource({pem_file, Fn}) ->
    is_list(Fn) orelse is_binary(Fn);
is_keysource(_) ->
    false.

%%

-spec init(keyset()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(Keyset) ->
    ok = create_table(),
    ok = genlib_map:foreach(
        fun (KID, Source) ->
            case store_key(KID, Source) of
                ok ->
                    ok;
                {error, Reason} ->
                    _ = lager:error("Error importing key ~s: ~p", [KID, Reason]),
                    exit({KID, Source, Reason})
            end
        end,
        Keyset
    ),
    {ok, {#{}, []}}.

%%

-spec store_key(keyname(), {pem_file, file:filename()}) ->
    ok | {error, file:posix() | {unknown_key, _}}.

store_key(Keyname, {pem_file, Filename}) ->
    store_key(Keyname, {pem_file, Filename}, #{
        kid => fun derive_kid_from_public_key_pem_entry/1
    }).

derive_kid_from_public_key_pem_entry(JWK) ->
    JWKPublic = jose_jwk:to_public(JWK),
    {_Module, PublicKey} = JWKPublic#jose_jwk.kty,
    {_PemEntry, Data, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey),
    base64url:encode(crypto:hash(sha256, Data)).

-type store_opts() :: #{
    kid => kid() | fun ((key()) -> kid())
}.

-spec store_key(keyname(), {pem_file, file:filename()}, store_opts()) ->
    ok | {error, file:posix() | {unknown_key, _}}.

store_key(Keyname, {pem_file, Filename}, Opts) ->
    case jose_jwk:from_pem_file(Filename) of
        JWK = #jose_jwk{} ->
            insert_key(Keyname, derive_kid(JWK, Opts), JWK);
        Error = {error, _} ->
            Error
    end.

derive_kid(_JWK, #{kid := Value}) when is_binary(Value) ->
    Value;
derive_kid(JWK, #{kid := DeriveFun}) when is_function(DeriveFun, 1) ->
    DeriveFun(JWK).

insert_key(Keyname, KID, JWK) ->
    KeyInfo = #{
        jwk      => JWK,
        kid      => KID,
        signer   => try jose_jwk:signer(JWK)   catch error:_ -> undefined end,
        verifier => try jose_jwk:verifier(JWK) catch error:_ -> undefined end
    },
    insert_values(#{
        {keyname, Keyname} => KeyInfo,
        {kid, KID}         => KeyInfo
    }).

%%

-type expiration() ::
    {lifetime, Seconds :: pos_integer()} |
    {deadline, UnixTs :: pos_integer()}.

-spec issue(keyname(), t(), expiration()) ->
    {ok, token()} |
    {error,
        nonexistent_key |
        invalid_operation
    }.

issue(Keyname, Auth, Expiration) ->
    Claims = construct_final_claims(Auth, Expiration),
    sign(Keyname, Claims).

construct_final_claims({{Subject, ACL}, Claims}, Expiration) ->
    maps:merge(
        Claims#{
            <<"jti">> => unique_id(),
            <<"sub">> => Subject,
            <<"exp">> => get_expires_at(Expiration)
        },
        encode_roles(capi_acl:encode(ACL))
    ).

get_expires_at({lifetime, Lt}) ->
    genlib_time:unow() + Lt;
get_expires_at({deadline, Dl}) ->
    Dl.

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

sign(Keyname, Claims) ->
    case lookup_value({keyname, Keyname}) of
        #{kid := KID, jwk := JWK, signer := #{} = JWS} ->
            sign(KID, JWK, JWS, Claims);
        #{signer := undefined} ->
            {error, invalid_operation};
        undefined ->
            {error, nonexistent_key}
    end.

sign(KID, JWK, JWS, Claims) ->
    JWT = jose_jwt:sign(JWK, JWS#{<<"kid">> => KID}, Claims),
    {_Modules, Token} = jose_jws:compact(JWT),
    {ok, Token}.

%%

-spec verify(token()) ->
    {ok, t()} |
    {error,
        {invalid_token,
            badarg |
            {badarg, term()} |
            {missing, atom()} |
            expired |
            {malformed_acl, term()}
        } |
        {nonexistent_key, kid()} |
        invalid_operation |
        invalid_signature
    }.

verify(Token) ->
    try
        {_, ExpandedToken} = jose_jws:expand(Token),
        #{<<"protected">> := ProtectedHeader} = ExpandedToken,
        Header = decode(ProtectedHeader),
        Alg = get_alg(Header),
        KID = get_kid(Header),
        verify(KID, Alg, ExpandedToken)
    catch
        throw:Reason ->
            {error, Reason};
        %% TODO we're losing error information here, e.g. stacktrace
        error:badarg = Reason ->
            {error, {invalid_token, Reason}};
        error:{badarg, _} = Reason ->
            {error, {invalid_token, Reason}}
    end.

verify(KID, Alg, ExpandedToken) ->
    case lookup_value({kid, KID}) of
        #{jwk := JWK, verifier := Algs} ->
            _ = lists:member(Alg, Algs) orelse throw(invalid_operation),
            verify(JWK, ExpandedToken);
        undefined ->
            {error, {nonexistent_key, KID}}
    end.

verify(JWK, ExpandedToken) ->
    case jose_jwt:verify(JWK, ExpandedToken) of
        {true, #jose_jwt{fields = Claims}, _JWS} ->
            {#{subject_id := SubjectID}, Claims1} = validate_claims(Claims),
            get_result(SubjectID, decode_roles(Claims1));
        {false, _JWT, _JWS} ->
            {error, invalid_signature}
    end.

validate_claims(Claims) ->
    validate_claims(Claims, get_validators(), #{}).

validate_claims(Claims, [{Name, Claim, Validator} | Rest], Acc) ->
    V = Validator(Name, maps:get(Claim, Claims, undefined)),
    validate_claims(maps:without([Claim], Claims), Rest, Acc#{Name => V});
validate_claims(Claims, [], Acc) ->
    {Acc, Claims}.

get_result(SubjectID, {Roles, Claims}) ->
    try
        Subject = {SubjectID, capi_acl:decode(Roles)},
        {ok, {Subject, Claims}}
    catch
        error:{badarg, _} = Reason ->
            throw({invalid_token, {malformed_acl, Reason}})
    end.

get_kid(#{<<"kid">> := KID}) when is_binary(KID) ->
    KID;
get_kid(#{}) ->
    throw({invalid_token, {missing, kid}}).

get_alg(#{<<"alg">> := Alg}) when is_binary(Alg) ->
    Alg;
get_alg(#{}) ->
    throw({invalid_token, {missing, alg}}).

decode(Bin) ->
    jsx:decode(base64url:decode(Bin), [return_maps]).

%%

get_validators() ->
    [
        {token_id   , <<"jti">> , fun check_presence/2},
        {subject_id , <<"sub">> , fun check_presence/2},
        {expires_at , <<"exp">> , fun check_expiration/2}
    ].

check_presence(_, V) when is_binary(V) ->
    V;
check_presence(C, undefined) ->
    throw({invalid_token, {missing, C}}).

check_expiration(_, Exp) when is_integer(Exp) ->
    case genlib_time:unow() of
        Now when Exp > Now ->
            Exp;
        _ ->
            throw({invalid_token, expired})
    end;
check_expiration(C, undefined) ->
    throw({invalid_token, {missing, C}});
check_expiration(C, V) ->
    throw({invalid_token, {badarg, {C, V}}}).

%%

encode_roles(Roles) ->
    #{
        <<"resource_access">> => #{
            <<"common-api">> => #{
                <<"roles">> => Roles
            }
        }
    }.

decode_roles(Claims = #{
    <<"resource_access">> := #{
        <<"common-api">> := #{
            <<"roles">> := Roles
        }
    }
}) when is_list(Roles) ->
    {Roles, maps:remove(<<"resource_access">>, Claims)};
decode_roles(_) ->
    throw({invalid_token, {missing, acl}}).

%%

-define(TABLE, ?MODULE).

create_table() ->
    _ = ets:new(?TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

insert_values(Values) ->
    true = ets:insert(?TABLE, maps:to_list(Values)),
    ok.

lookup_value(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            undefined
    end.
