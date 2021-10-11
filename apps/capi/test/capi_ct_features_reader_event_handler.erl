-module(capi_ct_features_reader_event_handler).

-export([handle_event/2]).
-export([create_storage/0]).
-export([delete_storage/0]).
-export([get_unused_params/0]).

%%

-spec create_storage() -> feat:event_handler().
create_storage() ->
    %% TODO delete named_table. Make opportunity for concurrent tests.
    ets:new(?MODULE, [set, public, named_table]).

-spec delete_storage() -> _.
delete_storage() ->
    ets:delete(?MODULE).

-spec get_unused_params() -> _.
get_unused_params() ->
    get_req_paths().

-spec handle_event(feat:event(), feat:options()) -> ok.
handle_event({request_visited, Req}, _Opts) ->
    save_req_paths(unroll_request_to_paths(Req));
handle_event({request_key_visit, Key, _Value}, _Opts) ->
    push_path(Key);
handle_event({request_key_visited, _Key, _Value}, _Opts) ->
    delete_subpath(pop_path());
handle_event({request_index_visit, N, _Value}, _Opts) ->
    push_path(N);
handle_event({request_index_visited, _N, _Value}, _Opts) ->
    delete_subpath(pop_path());
handle_event({request_variant_visit, _FeatureName, _Variant, _Value}, _Opts) ->
    ok;
handle_event({request_variant_visited, _FeatureName, _Variant, _Value}, _Opts) ->
    ok;
handle_event(Error, _Opts) ->
    throw(Error).

delete_subpath(Path) ->
    save_req_paths(
        lists:delete(
            lists:reverse(Path),
            get_req_paths()
        )
    ).

unroll_request_to_paths(Req) when is_map(Req); is_list(Req) ->
    lists:flatmap(
        fun({Key, Nested}) ->
            lists:map(
                fun(Rest) -> [Key | Rest] end,
                unroll_request_to_paths(Nested)
            )
        end,
        req_to_list(Req)
    );
unroll_request_to_paths(_req) ->
    [[]].

req_to_list(Map) when is_map(Map) ->
    maps:to_list(Map);
req_to_list(List) when is_list(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).

save_req_paths(Paths) ->
    insert(paths, Paths).

get_req_paths() ->
    get(paths, []).

put_path(Path) ->
    insert(path, Path).

get_path() ->
    get(path, []).

push_path(Item) ->
    put_path([Item | get_path()]).

pop_path() ->
    Path = get_path(),
    put_path(tl(Path)),
    Path.

insert(Key, Value) ->
    ets:insert(?MODULE, {Key, Value}),
    ok.

get(Key, Default) ->
    case ets:lookup(?MODULE, Key) of
        [] -> Default;
        [{Key, Value}] -> Value
    end.
