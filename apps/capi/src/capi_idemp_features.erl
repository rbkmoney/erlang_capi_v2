-module(capi_idemp_features).

-define(DIFFERENCE, -1).

-type request()         :: #{binary() := request_value()}.
-type request_value()   :: integer() | binary() | request() | [request()] | undefined.
-type difference()      :: features().
-type feature_name()    :: binary().
-type feature_value()   :: integer() | features() | [features()] | undefined.
-type features()        :: #{feature_name() := feature_value()}.
-type schema()          :: #{binary() := [binary() | schema() | {set, schema()}]}.

-type event()           :: {invalid_schema_fragment, feature_name(), request()} |
                           {request_visited, {request, request()}} |
                           {request_key_index_visit, integer()} |
                           {request_key_index_visited, integer()} |
                           {request_key_visit, {key, integer(), request()}} |
                           {request_key_visited, {key, integer()}}.

-type options()         :: term().
-type handler()         :: {module(), options()} | undefined.

-export_type([handler/0]).
-export_type([event/0]).
-export_type([schema/0]).
-export_type([request/0]).
-export_type([difference/0]).
-export_type([features/0]).
-export_type([feature_name/0]).
-export_type([feature_value/0]).

-export([read/2, read/3]).
-export([compare/2]).
-export([list_diff_fields/2]).

-callback handle_event(event(), options()) -> ok | feature_value().

-spec read(schema(), request()) ->
    features().

read(Schema, Request) ->
    read(undefined, Schema, Request).

-spec read(handler(), schema(), request()) ->
    features().

read(Handler, Schema, Request) ->
    handle_event(get_event_handler(Handler), {request_visited, {request, Request}}),
    read_(Schema, Request, Handler).

read_(Schema, Request, Handler) ->
    Result = maps:fold(
        fun
            (Name, Fs, Acc) when is_map(Fs) ->
                Value = read_(Fs, Request, Handler),
                Acc#{Name => Value};
            (Name, Accessor, Acc) when is_list(Accessor) ->
                FeatureValue = read_request_value(Accessor, Request, Handler),
                Acc#{Name => FeatureValue};
            (_Name, 'reserved', Acc) ->
                Acc
        end,
        #{},
        Schema
    ),
    Result.

read_request_value([], undefined, _) ->
    undefined;
read_request_value([], Value, _) ->
    hash(Value);
read_request_value([Schema = #{}], Request = #{}, Handler) ->
    read_(Schema, Request, Handler);
read_request_value([{set, Schema = #{}}], List, Handler) when is_list(List) ->
    {_, ListIndex} = lists:foldl(fun(Item, {N, Acc}) -> {N + 1, [{N, Item} | Acc]} end, {0, []}, List),
    ListSorted = lists:keysort(2, ListIndex),
    {_, Result} = lists:foldl(fun({Index, Req}, {N, Acc}) ->
        handle_event(get_event_handler(Handler), {request_key_index_visit, Index}),
        Value = read_(Schema, Req, Handler),
        handle_event(get_event_handler(Handler), {request_key_index_visited, Index}),
        {N + 1, Acc#{N => [Index, Value]}}
    end, {0, #{}}, ListSorted),
    Result;
read_request_value([Key | Rest], Request = #{}, Handler) when is_binary(Key) ->
    SubRequest = maps:get(Key, Request, undefined),
    handle_event(get_event_handler(Handler), {request_key_visit, {key, Key, SubRequest}}),
    Value = read_request_value(Rest, SubRequest, Handler),
    handle_event(get_event_handler(Handler), {request_key_visited, {key, Key}}),
    Value;
read_request_value(_, undefined, _) ->
    undefined;
read_request_value(Key, Request, Handler) ->
    handle_event(get_event_handler(Handler), {invalid_schema_fragment, Key, Request}).

handle_event(undefined, {invalid_schema_fragment, Key, Request}) ->
    logger:warning("Unable to extract idemp feature with schema: ~p from client request subset: ~p", [Key, Request]),
    undefined;
handle_event(undefined, _Event) ->
    ok;
handle_event({Mod, Opts}, Event) ->
    Mod:handle_event(Event, Opts).

get_event_handler({Mod, Options}) ->
    {Mod, Options};
get_event_handler(undefined) ->
    undefined.

hash(V) ->
    erlang:phash2(V).

-spec list_diff_fields(schema(), difference()) ->
    [binary()].

list_diff_fields(Schema, Diff) ->
    ConvertedDiff = list_diff_fields_(Diff, Schema),
    lists:foldl(fun(Keys, AccIn) ->
        KeysBin = lists:map(fun genlib:to_binary/1, Keys),
        Item = list_to_binary(lists:join(<<".">>, KeysBin)),
        case lists:member(Item, AccIn) of
            false ->
               [Item | AccIn];
            _ ->
                AccIn
        end
    end, [], ConvertedDiff).

list_diff_fields_(Diff, Schema) ->
    zipfold(
        fun
            (_Feature, ?DIFFERENCE, [Key, _SchemaPart], AccIn)  ->
                [[Key] | AccIn];
            (_Feature, Value, [Key | SchemaPart], AccIn) when is_list(SchemaPart) and is_map(Value) ->
                list_diff_fields_(Value, SchemaPart, {[Key], AccIn});
            (_Feature, _Value, [Key], AccIn) when is_binary(Key) ->
                [[Key] | AccIn];
            (_Feature, Value, SchemaPart, AccIn) when is_map(SchemaPart) ->
                Result = list_diff_fields_(Value, SchemaPart),
                Result ++ AccIn
        end,
        [],
        Diff,
        Schema).

list_diff_fields_(Value, [SchemaPart], {PrefixIn, AccIn}) when is_map(SchemaPart) ->
    Prefix = lists:reverse(PrefixIn),
    Result = [Prefix ++ List || List <- list_diff_fields_(Value, SchemaPart)],
    Result ++ AccIn;
list_diff_fields_(Values, [{set, SchemaPart}], {PrefixIn, AccIn}) when is_map(SchemaPart) and is_map(Values) ->
    Result = maps:fold(fun(Index, Value, Acc) ->
        Prefix = lists:reverse([Index | PrefixIn]),
        List = [Prefix ++ L || L <- list_diff_fields_(Value, SchemaPart)],
        List ++ Acc
    end, [], Values),
    Result ++ AccIn;
list_diff_fields_(Value, [Key | SchemaPart], {Prefix, Acc}) ->
    list_diff_fields_(Value, SchemaPart, {[Key | Prefix], Acc}).

-spec compare(features(), features()) ->
    true | {false, difference()}.

compare(Features, FeaturesWith) ->
    case compare_features(Features, FeaturesWith) of
        Diff when map_size(Diff) > 0 ->
            {false, Diff};
        _ ->
            true
    end.

compare_features(Fs, FsWith) ->
    zipfold(
        fun
            (Key, [Index1, Value], [_, ValueWith], Diff) when
            is_integer(Key), is_map(ValueWith), is_map(Value) ->
                compare_features_(Index1, Value, ValueWith, Diff);
            (Key, Value, ValueWith, Diff) when
            is_map(ValueWith) and is_map(Value) ->
                compare_features_(Key, Value, ValueWith, Diff);
            %% We expect that clients may _at any time_ change their implementation and start
            %% sending information they were not sending beforehand, so this is not considered a
            %% conflict. Yet, we DO NOT expect them to do the opposite, to stop sending
            %% information they were sending, this is still a conflict.
            (_Key, _Value, undefined, Diff) ->
                Diff;
            (_Key, Value, Value, Diff) ->
                Diff;
            (Key, Value, ValueWith, Diff) when Value =/= ValueWith ->
                Diff#{Key => ?DIFFERENCE}
        end,
        #{},
        Fs,
        FsWith
    ).

compare_features_(Key, Value, ValueWith, Diff) ->
    case compare_features(Value, ValueWith) of
        ValueWith ->
            Diff#{Key => ?DIFFERENCE}; % different everywhere
        #{<<"$type">> := _} ->
            % Different with regard to _type_, semantically same as different everywhere.
            Diff#{Key => ?DIFFERENCE};
        Diff1 when map_size(Diff1) > 0 ->
            Diff#{Key => Diff1};
        #{} ->
            Diff % no notable differences
    end.

zipfold(Fun, Acc, M1, M2) ->
    maps:fold(
        fun (Key, V1, AccIn) ->
            case maps:find(Key, M2) of
                {ok, V2} ->
                    Fun(Key, V1, V2, AccIn);
                error ->
                    AccIn
            end
        end,
        Acc,
        M1
    ).
