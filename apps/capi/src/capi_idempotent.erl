-module(capi_idempotent).

-export([compare_features/2]).
-export([payment_features/1]).
-export([invoice_features/1]).
-export([refund_features/1]).

-type feature_name()  :: [binary()].
-type feature_value() :: any().
-type features()      :: [{feature_name(), feature_value()}].
-type difference() :: #{
    unequal   => [feature_name()],
    excess  => [feature_name()],
    missing => [feature_name()]
}.

-spec compare_features(features(), features()) ->
    true | {false, difference()}.

compare_features(Feature, Feature) ->
    true;
compare_features(NewFeaturesBin, OldFeaturesBin) ->
    NewFeatures = erlang:binary_to_term(NewFeaturesBin),
    OldFeatures = erlang:binary_to_term(OldFeaturesBin),

    UnEqualFeatures = unequal_features(NewFeatures, OldFeatures),
    ExcessFeatures  = delete_same_prefix(UnEqualFeatures, excess_features(NewFeatures, OldFeatures)),
    MissingFeatures = delete_same_prefix(UnEqualFeatures, missing_features(NewFeatures, OldFeatures)),
    {false, genlib_map:compact(#{
        unequal => UnEqualFeatures,
        excess => ExcessFeatures,
        missing => MissingFeatures
    })}.

-spec payment_features(any()) -> map().

payment_features(PaymentParamsSwag) ->
    Prefix = [],
    {_, PaymentParamsFlat} = maps:fold(fun map_to_flat/3, {Prefix, #{}}, PaymentParamsSwag),
    Attributes = filter_attribute(payment_attributes(), PaymentParamsFlat),
    erlang:term_to_binary(Attributes).

-spec invoice_features(any()) -> binary(). %% TODO[0x42]: make strong type definition

invoice_features(InvoiceParamsSwag) ->
    Prefix = [],
    {_, InvoiceParamsFlat} = maps:fold(fun map_to_flat/3, {Prefix, #{}}, InvoiceParamsSwag),
    Attributes = filter_attribute(invoice_attributes(), InvoiceParamsFlat),
    erlang:term_to_binary(Attributes).

-spec refund_features(any()) -> binary(). %% TODO[0x42]: make strong type definition

refund_features(RefundParamsSwag) ->
    Prefix = [],
    {_, RefundParamsFlat} = maps:fold(fun map_to_flat/3, {Prefix, #{}}, RefundParamsSwag),
    Attributes = filter_attribute(refund_attributes(), RefundParamsFlat),
    erlang:term_to_binary(Attributes).


filter_attribute(Attribute, ParamsFlat) ->
    FunAdd = fun
        (_, undefined, Acc) -> Acc;
        (Key, Value, Acc) -> Acc#{Key => Value}
    end,
    FunHash = fun
        (undefined) -> undefined;
        (Terms) -> erlang:phash2(Terms)
    end,
    lists:foldl(fun
        ({cut_name, Attr}, Acc) ->
            Value = maps:get(Attr, ParamsFlat, undefined),
            Key = lists:droplast(Attr),
            FunAdd(Key, Value, Acc);
        ({to_hash, Attr}, Acc) ->
            Value = maps:get(Attr, ParamsFlat, undefined),
            FunAdd(Attr, FunHash(Value), Acc);
        (Attr, Acc) ->
            Value = maps:get(Attr, ParamsFlat, undefined),
            FunAdd(Attr, Value, Acc)
    end, #{}, Attribute).

%% Idempotent attributes
%% cut_name - используется, чтобы признаки payerType | type находились уровнем выше
%%            и если они отличаются, то diff дочерних признаков не попадал в вывод.
%% to_hash -  считает hash от значения признака, для упрощения алгоритма сравнения (fun wrong_value)
%%
payment_attributes() ->
    [
        {cut_name, [<<"payer">>, <<"payerType">>]},
        {cut_name, [<<"payer">>, <<"paymentTool">>, <<"type">>]},
        [<<"payer">>, <<"paymentTool">>, <<"token">>],
        [<<"payer">>, <<"paymentTool">>, <<"cardholder_name">>],
        [<<"payer">>, <<"paymentTool">>, <<"exp_date">>],
        [<<"payer">>, <<"CustomerID">>],
        [<<"payer">>, <<"recurrentParentPayment">>, <<"invoiceID">>],
        [<<"payer">>, <<"recurrentParentPayment">>, <<"paymentID">>]
    ].

invoice_attributes() ->
    [
        [<<"shopID">>],
        [<<"amount">>],
        [<<"currency">>],
        [<<"product">>],
        {to_hash, [<<"cart">>]}
    ].

refund_attributes() ->
    [
        [<<"invoiceID">>],
        [<<"paymentID">>],
        [<<"amount">>],
        [<<"currency">>],
        {to_hash, [<<"cart">>]}
    ].

%% sets intersection by features values

unequal_features(NewFeatures, OldFeatures) ->
    Keys = maps:keys(OldFeatures),
    maps:fold(fun(K, Value, Acc) ->
        case maps:get(K, OldFeatures) of
            Value -> Acc;
            _WrongValue -> [K | Acc]
        end
    end, [], maps:with(Keys, NewFeatures)).

excess_features(NewFeatures, OldFeatures) ->
    Keys = maps:keys(OldFeatures),
    maps:keys(maps:without(Keys, NewFeatures)).

missing_features(NewFeatures, OldFeatures) ->
    Keys = maps:keys(NewFeatures),
    maps:keys(maps:without(Keys, OldFeatures)).

delete_same_prefix(Prefixs, Attrs) ->
    Result = lists:foldl(fun(Prefix, Acc) ->
        lists:filter(fun(Attr) ->
            not lists:prefix(Prefix, Attr)
        end, Acc)
    end, Attrs, Prefixs),
    case Result of
        [] -> undefined;
        _ -> Result
    end.

map_to_flat(Key, #{} = Value, {Prefix, Acc}) ->
    {_Prefix2, AccOut} = maps:fold(fun map_to_flat/3, {[Key | Prefix], Acc}, Value),
    {Prefix, AccOut};
map_to_flat(Key, Value, {Prefix, Acc}) ->
    add_prefix(Key, Value, {Prefix, Acc}).

add_prefix(Key, Value, {Prefix, Acc}) ->
    FlatKey = lists:reverse([Key | Prefix]),
    {Prefix, Acc#{FlatKey => Value}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec wrong_features_test() -> _.
wrong_features_test() ->
    Set1 = #{
        [a] => 1,
        [a, b] => 2,
        [a, b, c] => 3
    },
    Set2 = #{
        [a] => 1,
        [a, b] => 3
    },
    ?assertEqual(wrong_features(Set1, Set2#{[a,b] => 2}), []),
    ?assertEqual(wrong_features(Set1, Set2), [[a, b]]).

-spec excess_features_test() -> _.
excess_features_test() ->
    Set1 = #{[a] => 1, [a, c] => 3},
    Set2 = #{[a] => 1, [a, b] => 2},
    ?assertEqual(excess_features(Set1, Set2), [[a, c]]),
    ?assertEqual(excess_features(Set2, Set1), [[a, b]]).

-spec compare_features_test() -> _.
compare_features_test() ->
    Set1 = erlang:term_to_binary(#{
        [name] => <<"Degus">>,
        [payer] => <<"ResourcePayer">>,
        [payer, paymentTool, token] => 12345678
    }),
    Set2 = #{
        [name] => <<"Degus">>,
        [payer] => <<"CustomerPayer">>,
        [payer, customerID] => 12345678
    },
    ?assertEqual(compare_features(Set1, erlang:term_to_binary(Set2)), {false, #{wrong => [[payer]]}}),
    ?assertEqual(
        compare_features(Set1, erlang:term_to_binary(Set2#{[payer] => <<"ResourcePayer">>})),
        {false, #{
            wrong => [],
            missing => [[payer, customerID]],
            excess => [[payer, paymentTool, token]]
        }}
    ).

-endif.
