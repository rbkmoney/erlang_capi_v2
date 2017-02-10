-module(capi_keystore).

%%

-export([init/0]).
-export([get/1]).
-export([store/2]).

%%

-include_lib("jose/include/jose_jwk.hrl").

-type name() :: term().
-type key() :: #jose_jwk{}.

-spec init() ->
    ok.

init() ->
    create_table().

-spec get(name()) ->
    {ok, key()} | {error, notfound}.

get(Name) ->
    case lookup_value(Name) of
        Key = #jose_jwk{} ->
            {ok, Key};
        _ ->
            {error, notfound}
    end.

-spec store(name(), file:filename()) ->
    ok | {error, file:posix() | {unknown_key, _}}.

store(Name, KeyFilename) ->
    case jose_jwk:from_pem_file(KeyFilename) of
        Key = #jose_jwk{} ->
            insert_value(Name, Key);
        Error = {error, _} ->
            Error
    end.

%%

-define(TABLE, ?MODULE).

create_table() ->
    _ = ets:new(?TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

insert_value(Name, Value) ->
    true = ets:insert(?TABLE, [{Name, Value}]),
    ok.

lookup_value(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{Name, Value}] ->
            Value;
        [] ->
            undefined
    end.
