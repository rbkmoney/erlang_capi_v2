-module(capi_idempotent).

-export([compare_signs/2]).
-export([payment_signs/1]).
-export([invoice_signs/1]).
-export([refund_signs/1]).

-type sign_name()  :: [binary()].
-type sign_value() :: any().
-type signs()      :: [{sign_name(), sign_value()}].
-type difference() :: #{
    wrong   => [sign_name()],
    excess  => [sign_name()],
    missing => [sign_name()]
}.

-spec compare_signs(signs(), signs()) ->
    true | {false, difference()}.

compare_signs(Sign, Sign) ->
    true;
compare_signs(NewSignsBin, OldSignsBin) ->
    NewSigns = erlang:binary_to_term(NewSignsBin),
    OldSigns = erlang:binary_to_term(OldSignsBin),

    WrongSigns   = wrong_signs(NewSigns, OldSigns),
    ExcessSigns  = delete_same_prefix(WrongSigns, excess_signs(NewSigns, OldSigns)),
    MissingSigns = delete_same_prefix(WrongSigns, missing_signs(NewSigns, OldSigns)),
    {false, genlib_map:compact(#{
        wrong => WrongSigns,
        excess => ExcessSigns,
        missing => MissingSigns
    })}.

-spec payment_signs(any()) -> map().

payment_signs(PaymentParamsSwag) ->
    Prefix = [],
    {_, PaymentParamsFlat} = maps:fold(fun map_to_flat/3, {Prefix, #{}}, PaymentParamsSwag),
    Attributes = filter_attribute(payment_attributes(), PaymentParamsFlat),
    erlang:term_to_binary(Attributes).

-spec invoice_signs(any()) -> binary(). %% TODO[0x42]: make strong type definition

invoice_signs(InvoiceParamsSwag) ->
    Prefix = [],
    {_, InvoiceParamsFlat} = maps:fold(fun map_to_flat/3, {Prefix, #{}}, InvoiceParamsSwag),
    Attributes = filter_attribute(invoice_attributes(), InvoiceParamsFlat),
    erlang:term_to_binary(Attributes).

-spec refund_signs(any()) -> binary(). %% TODO[0x42]: make strong type definition

refund_signs(RefundParamsSwag) ->
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

%% sets intersection by signs values

wrong_signs(NewSigns, OldSigns) ->
    Keys = maps:keys(OldSigns),
    maps:fold(fun(K, Value, Acc) ->
        case maps:get(K, OldSigns) of
            Value -> Acc;
            _WrongValue -> [K | Acc]
        end
    end, [], maps:with(Keys, NewSigns)).

excess_signs(NewSigns, OldSigns) ->
    Keys = maps:keys(OldSigns),
    maps:keys(maps:without(Keys, NewSigns)).

missing_signs(NewSigns, OldSigns) ->
    Keys = maps:keys(NewSigns),
    maps:keys(maps:without(Keys, OldSigns)).

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

-spec wrong_sings_test() -> _.
wrong_sings_test() ->
    Set1 = #{
        [a] => 1,
        [a, b] => 2,
        [a, b, c] => 3
    },
    Set2 = #{
        [a] => 1,
        [a, b] => 3
    },
    ?assertEqual(wrong_signs(Set1, Set2#{[a,b] => 2}), []),
    ?assertEqual(wrong_signs(Set1, Set2), [[a, b]]).

-spec excess_signs_test() -> _.
excess_signs_test() ->
    Set1 = #{[a] => 1, [a, c] => 3},
    Set2 = #{[a] => 1, [a, b] => 2},
    ?assertEqual(excess_signs(Set1, Set2), [[a, c]]),
    ?assertEqual(excess_signs(Set2, Set1), [[a, b]]).

-spec compare_signs_test() -> _.
compare_signs_test() ->
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
    ?assertEqual(compare_signs(Set1, erlang:term_to_binary(Set2)), {false, #{wrong => [[payer]]}}),
    ?assertEqual(
        compare_signs(Set1, erlang:term_to_binary(Set2#{[payer] => <<"ResourcePayer">>})),
        {false, #{
            wrong => [],
            missing => [[payer, customerID]],
            excess => [[payer, paymentTool, token]]
        }}
    ).

-endif.
