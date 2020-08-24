-module(capi_ct_handler_event).

-export([handle_event/1]).
-export([create_acc/0]).
-export([del_storage/0]).
-export([get_unused_params/0]).
%%

-spec create_acc() -> _.

create_acc() ->
    ets:new(?MODULE, [set, public, named_table]).

-spec del_storage() -> _.

del_storage() ->
    ets:delete(?MODULE).

-spec get_unused_params() -> _.

get_unused_params() ->
    get_request().

-spec handle_event(capi_idemp_features:event()) ->
    ok.

handle_event({invalid_schema_fragment, Key, Request}) ->
    throw({extact_idemp_feature, Key, Request});
handle_event({request_visited, {request, Req}}) ->
    save_request(Req),
    ok;
handle_event({request_key_index_visit, N}) ->
    push_path({key_index, N}),
    ok;
handle_event({request_key_index_visited, _N}) ->
    %% delete  key_index from stack
    pop_path(),
    {Key, List} = pop_path(),
    %% delete empty map from set
    List2 = lists:foldl(fun
        (M, AccIn) when map_size(M) =:= 0 -> AccIn;
        (M, AccIn) -> [M | AccIn]
    end, [], List),
    push_path({Key, List2}),
    ok;
handle_event({request_key_visit, {key, Key, SubReq}}) ->
    push_path({Key, SubReq}),
    ok;
handle_event({request_key_visited, {key, Key}}) ->
    Path = get_path(),
    [{Key, SubReq} | Tail] = Path,
    delete_subpath(Key, SubReq, Tail),
    ok.

delete_subpath(Key, SubReq, []) when
    is_map(SubReq), map_size(SubReq) > 0;
    is_list(SubReq), length(SubReq) > 0 ->
    Request = get_request(),
    Request2 = Request#{Key => SubReq},
    save_request(Request2),
    ok;
delete_subpath(Key, SubReq, [{K, Req} | T]) when
    is_map(SubReq), map_size(SubReq) > 0;
    is_list(SubReq), length(SubReq) > 0 ->
        Req2 = Req#{Key => SubReq},
        update_path([{K, Req2}| T]),
    ok;
delete_subpath(Key, _SubReq, []) ->
    Request = get_request(),
    Request2 = maps:remove(Key, Request),
    insert(path, []),
    save_request(Request2);
delete_subpath(Key, _SubReq, [{key_index, Index} = KeyIndex | Tail]) ->
    [{KeyList, SubReqList} | T] = Tail,
    {_, SubReqList2} = lists:foldl(fun
        (SubReq, {I, AccIn}) when I == Index ->
            SubReq2 = maps:remove(Key, SubReq),
            {I + 1, [SubReq2 | AccIn]};
        (_, {N, AccIn}) -> {N + 1, AccIn}
    end, {0, []}, SubReqList),
    Path = [KeyIndex, {KeyList, lists:reverse(SubReqList2)}] ++ T,
    update_path(Path),
    ok;
delete_subpath(Key, _SubReq, [{K, Req} | T]) ->
    Req2 = maps:remove(Key, Req),
    update_path([{K, Req2} | T]),
    ok.

save_request(Req) ->
    insert(request, Req).

get_request() ->
    case ets:lookup(?MODULE, request) of
        [] -> #{};
        [{request, Req}] -> Req
    end.

update_path(Path) ->
    insert(path, Path).

get_path() ->
    case ets:lookup(?MODULE, path) of
        [] -> [];
        [{path, Path}] -> Path
    end.

push_path({Key, Req}) ->
    Path = get_path(),
    insert(path, [{Key, Req} | Path]).

pop_path() ->
    [Result | Path] = get_path(),
    insert(path, Path),
    Result.

insert(Key, Value) ->
    ets:insert(?MODULE, {Key, Value}).
