-module(capi_idempotent).

-export([compare_payment_signs/2]).
-export([payment_signs/1]).

-type sign_name()  :: [binary()].
-type sign_value() :: any().
-type signs()      :: [{sign_name(), sign_value()}].
-type difference() :: #{
    wrong   => [sign_name()],
    excess  => [sign_name()],
    missing => [sign_name()]
}.

-spec compare_payment_signs(signs(), signs()) ->
    true | {false, difference()}.

compare_payment_signs(Sign, Sign) ->
    true;
compare_payment_signs(NewSings, OldSings) ->
    WrongSigns   = wrong_signs(NewSings, OldSings),
    ExcessSigns  = delete_same_prefix(WrongSigns, excess_signs(NewSings, OldSings)),
    MissingSigns = delete_same_prefix(WrongSigns, missing_signs(NewSings, OldSings)),
    {false, genlib_map:compact(#{
        wrong => WrongSigns,
        excess => ExcessSigns,
        missing => MissingSigns
    })}.

-spec payment_signs(any()) -> map().

payment_signs(PaymentParamsSwag) ->
    Prefix = [],
    {_, PaymentParamsFlat} = maps:fold(fun map_to_flat/3, {Prefix, #{}}, PaymentParamsSwag),
    filter_attribute(payment_attribute(), PaymentParamsFlat).

filter_attribute(Attribute, ParamsFlat) ->
    FunAdd = fun
        (_, undefined, Acc) -> Acc;
        (Key, Value, Acc) -> Acc#{Key => Value}
    end,
    lists:foldl(fun
        ({cut_name, Attr}, Acc) ->
            Value = maps:get(Attr, ParamsFlat, undefined),
            Key = lists:droplast(Attr),
            FunAdd(Key, Value, Acc);
        (Attr, Acc) ->
            Value = maps:get(Attr, ParamsFlat, undefined),
            FunAdd(Attr, Value, Acc)
    end, #{}, Attribute).

%% Idempotent attributes

payment_attribute() ->
    [
        [<<"externalID">>],
        {cut_name, [<<"payer">>, <<"payerType">>]},
        {cut_name, [<<"payer">>, <<"paymentTool">>, <<"type">>]},
        [<<"payer">>, <<"paymentTool">>, <<"token">>],
        [<<"payer">>, <<"paymentTool">>, <<"cardholder_name">>],
        [<<"payer">>, <<"paymentTool">>, <<"exp_date">>],
        [<<"payer">>, <<"CustomerID">>],
        [<<"payer">>, <<"recurrentParentPayment">>, <<"invoiceID">>],
        [<<"payer">>, <<"recurrentParentPayment">>, <<"paymentID">>]
    ].

%% sets intersection by signs values

wrong_signs(NewSigns, OldSigns) ->
    WrongSigns = maps:fold(fun(K, Value, Acc) ->
        Compare = fun
            (#{K := NewSignValue}) when NewSignValue =:= Value -> Acc;
            (#{K := _WrongValue}) -> [K|Acc];
            (_) -> Acc %% Attribute(Idempotent sign) doesn't exist
        end,
        Compare(NewSigns)
    end, [], OldSigns),
    WrongSigns.

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

