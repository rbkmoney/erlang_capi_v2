-module(capi_idemp_features).

-define(DIFFERENCE, -1).

-type request()         :: #{binary() := request_value()}.
-type request_value()   :: integer() | binary() | request() | [request()].
-type schema()          :: capi_feature_schemas:schema().
-type difference()      :: features().
-type feature_name()    :: binary().
-type feature_value()   :: integer() | features() | [features()].
-type features()        :: #{feature_name() := feature_value() | undefined}.

-export_type([difference/0]).
-export_type([schema/0]).
-export_type([features/0]).

-export([read_features/2]).
-export([equal_features/2]).
-export([list_diff_fields/2]).

-spec read_features(schema(), request()) ->
    features().

read_features(Schema, Request) ->
    maps:fold(
        fun
            (Name, Fs, Acc) when is_map(Fs) ->
                Value = read_features(Fs, Request),
                Acc#{Name => Value};
            (Name, Accessor, Acc) when is_list(Accessor) ->
                FeatureValue = read_request_value(Accessor, Request),
                Acc#{Name => FeatureValue};
            (_Name, 'reserved', Acc) ->
                Acc
        end,
        #{},
        Schema
    ).

read_request_value([], undefined) ->
    undefined;
read_request_value([], V) ->
    hash(V);
read_request_value([Schema = #{}], Request = #{}) ->
    read_features(Schema, Request);
read_request_value([[Schema = #{}]], List) when is_list(List) ->
    {_, Value} = lists:foldl(fun(Req, {N, Acc}) ->
        {N + 1, Acc#{N => read_features(Schema, Req)}}
    end, {0, #{}}, lists:sort(List)),
    Value;
read_request_value([Key | Rest], Request = #{}) when is_binary(Key) ->
    read_request_value(Rest, maps:get(Key, Request, undefined));
read_request_value(_, undefined) ->
    undefined;
read_request_value(Key, Request) ->
    logger:warning("Unable to extract idemp feature with schema: ~p from client request subset: ~p", [Key, Request]),
    undefined.

hash(V) ->
    erlang:phash2(V).

-spec list_diff_fields(schema(), difference()) ->
    [binary()].

list_diff_fields(Schema, Diff) ->
    ConvertedDiff = map_to_flat(features_to_schema(Diff, Schema)),
    maps:fold(fun(Keys, _, AccIn) ->
        KeysBin = lists:map(fun genlib:to_binary/1, Keys),
        [list_to_binary(lists:join(<<".">>, KeysBin)) | AccIn]
    end, [], ConvertedDiff).

features_to_schema(Diff, Schema) ->
    zipfold(
        fun
            (_Feature, ?DIFFERENCE, [Key, ValueWith], AccIn) when is_map(ValueWith) ->
                AccIn#{Key => ?DIFFERENCE};
            (_Feature, Value, [Key, ValueWith], AccIn) when is_map(ValueWith) and is_map(Value) ->
                AccIn#{Key => features_to_schema(Value, ValueWith)};
            (_Feature, Values, [Key, [ValueWith]], AccIn) when is_map(ValueWith) and is_map(Values) ->
                Result = maps:fold(fun(Index, Value, Acc) ->
                    Acc#{Index => features_to_schema(Value, ValueWith)}
                end, #{}, Values),
                AccIn#{Key => Result};
            (_Feature, Value, [Key], AccIn) when is_binary(Key) ->
                AccIn#{Key => Value};
            (_Feature, Value, ValueWith, AccIn) when is_map(ValueWith) ->
                maps:merge(AccIn, features_to_schema(Value, ValueWith));
            (_Feature, undefined, [Key, ValueWith], AccIn) when is_map(ValueWith) ->
                AccIn#{Key => ?DIFFERENCE}
        end,
        #{},
        Diff,
        Schema).

-spec equal_features(features(), features()) ->
    true | {false, difference()}.

equal_features(Features, FeaturesWith) ->
    case compare_features(Features, FeaturesWith) of
        Diff when map_size(Diff) > 0 ->
            {false, Diff};
        _ ->
            true
    end.

compare_features(Fs, FsWith) ->
    zipfold(
        fun
            (Key, Value, ValueWith, Diff) when is_map(ValueWith) and is_map(Value) ->
                case compare_features(Value, ValueWith) of
                    ValueWith ->
                        Diff#{Key => ?DIFFERENCE}; % different everywhere
                    #{<<"$type">> := _} ->
                        Diff#{Key => ?DIFFERENCE};
                    Diff1 when map_size(Diff1) > 0 ->
                        Diff#{Key => Diff1};
                    #{} ->
                        Diff % no notable differences
                end;
            %% We expect that clients may _at any time_ change their implementation and start
            %% sending information they were not sending beforehand, so this is not considered a
            %% conflict. Yet, we DO NOT expect them to do the opposite, to stop sending
            %% information they were sending, this is still a conflict.
            (_Key, _Value, undefined, Diff) ->
                Diff;
            (_Key, Value, Value, Diff) ->
                Diff;
            (Key, Value, _ValueWith, Diff) ->
                Diff#{Key => Value}
        end,
        #{},
        Fs,
        FsWith
    ).

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

map_to_flat(Value) ->
    Prefix = [],
    Acc = #{},
    {_, FlatMap} = maps:fold(fun map_to_flat/3, {Prefix, Acc}, Value),
    FlatMap.

map_to_flat(Key, #{} = Value, {Prefix, Acc}) ->
    {_Prefix2, AccOut} = maps:fold(fun map_to_flat/3, {[Key | Prefix], Acc}, Value),
    {Prefix, AccOut};
map_to_flat(Key, [#{} | _ ] = Value, {Prefix, Acc}) ->
    {_, _, AccOut} = lists:foldl(fun
        (Map, {Pr, N, AccIn}) ->
            {_, FlatMap} = maps:fold(fun map_to_flat/3, {[integer_to_binary(N), Pr], #{}}, Map),
            {Pr, N + 1, maps:merge(FlatMap, AccIn)}
        end,
        {Key, 0, Acc},
        Value
    ),
    {Prefix, AccOut};
map_to_flat(Key, Value, {Prefix, Acc}) ->
    add_prefix(Key, Value, {Prefix, Acc}).

add_prefix(Key, Value, {Prefix, Acc}) ->
    FlatKey = lists:reverse([Key | Prefix]),
    {Prefix, Acc#{FlatKey => Value}}.

%%
%% TESTS
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(PAYER, #{
    <<"payer">> => #{
        <<"type">>        => undefined,
        <<"recurrent">>   => undefined,
        <<"customer">>    => undefined,
        <<"tool">>        => #{}
    }
}).
-define(INVOICE, #{
    <<"shop_id">>   => undefined,
    <<"amount">>    => undefined,
    <<"currency">>  => undefined,
    <<"product">>   => undefined,
    <<"cart">>      => undefined
}).
-define(PRODUCT, #{
    <<"product">>  => undefined,
    <<"quantity">> => undefined,
    <<"price">>    => undefined,
    <<"tax">>      => undefined
}).

%% Test helpers

deep_merge(M1, M2) ->
    maps:fold(
        fun (K, V, MAcc) when is_map(V) ->
                Value = deep_merge(maps:get(K, MAcc, #{}), V),
                MAcc#{K => Value};
            (K, V, MAcc) ->
                MAcc#{K => V}
        end, M1, M2).

-spec test() -> _.

-spec read_payment_features_value_test() -> _.
read_payment_features_value_test() ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType    = <<"bank_card">>,
    Token       = <<"cds token">>,
    CardHolder  = <<"0x42">>,
    ExpDate     = {exp_date, 02, 2022},
    Request = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => ToolType,
                <<"token">>           => Token,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder
            }
    }},

    Payer = #{
        <<"payer">> => #{
            <<"type">> => hash(PayerType),
            <<"customer">> => undefined,
            <<"recurrent">> => undefined,
            <<"tool">> => #{
                <<"$type">> => hash(ToolType),
                <<"bank_card">> => #{
                    <<"cardholder">> => hash(CardHolder),
                    <<"expdate">>    => hash(ExpDate),
                    <<"token">>      => hash(Token)},
                <<"crypto">> => #{<<"currency">> => undefined},
                <<"mobile_commerce">> => #{
                    <<"operator">> => undefined,
                    <<"phone">>    => undefined},
                <<"terminal">> => #{<<"terminal_type">> => undefined},
                <<"wallet">> => #{
                    <<"id">>        => undefined,
                    <<"provider">>  => undefined,
                    <<"token">>     => hash(Token)}
            }
        }
    },
    ?assertEqual(Payer, read_features(capi_feature_schemas:payment(), Request)).

-spec read_payment_customer_features_value_test() -> _.
read_payment_customer_features_value_test() ->
    PayerType = <<"CustomerPayer">>,
    CustomerID = <<"some customer id">>,
    Request = #{
        <<"payer">> => #{
            <<"payerType">>  => PayerType,
            <<"customerID">> => CustomerID
        }
    },
    Payer = #{
        <<"payer">> => #{
            <<"type">>       => hash(PayerType),
            <<"customer">>   => hash(CustomerID),
            <<"recurrent">>  => undefined,
            <<"tool">>       => undefined
        }
    },
    ?assertMatch(Payer, read_features(capi_feature_schemas:payment(), Request)).

-spec compare_payment_bank_card_test() -> _.
compare_payment_bank_card_test() ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType    = <<"bank_card">>,
    Token1      = <<"cds token">>,
    Token2      = <<"cds token 2">>,
    CardHolder1 = <<"0x42">>,
    CardHolder2 = <<"Cake">>,
    ExpDate     = {exp_date, 02, 2022},
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => ToolType,
                <<"token">>           => Token1,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder1
            }
    }},
    Request2 = deep_merge(Request1, #{<<"payer">> => #{
        <<"paymentTool">> => #{
            <<"token">> => Token2,
            <<"cardholder_name">> => CardHolder2
        }
    }}),
    Schema = capi_feature_schemas:payment(),
    F1 = read_features(Schema, Request1),
    F2 = read_features(Schema, Request2),
    ?assertEqual(true, equal_features(F1, F1)),
    {false, Diff} = equal_features(F1, F2),
    ?assertEqual([
        <<"payer.paymentTool.token">>,
        <<"payer.paymentTool.cardholder_name">>
    ], list_diff_fields(Schema, Diff)).

-spec compare_different_payment_tool_test() -> _.
compare_different_payment_tool_test() ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType1   = <<"bank_card">>,
    ToolType2   = <<"wallet">>,
    Token1      = <<"cds token">>,
    Token2      = <<"wallet token">>,
    CardHolder  = <<"0x42">>,
    ExpDate     = {exp_date, 02, 2022},
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => ToolType1,
                <<"token">>           => Token1,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder
            }
    }},
    Request2 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>  => ToolType2,
                <<"token">> => Token2
            }
        }
    },

    Schema = capi_feature_schemas:payment(),
    F1 = read_features(Schema, Request1),
    F2 = read_features(Schema, Request2),
    ?assertEqual(true, equal_features(F1, F1)),
    {false, Diff} = equal_features(F1, F2),
    ?assertEqual([<<"payer.paymentTool">>], list_diff_fields(Schema, Diff)).

-spec read_invoice_features_value_test() -> _.
read_invoice_features_value_test() ->
    ShopID      = <<"shopus">>,
    Cur         = <<"XXX">>,
    Prod1       = <<"yellow duck">>,
    Prod2       = <<"blue duck">>,
    Price1      = 10000,
    Price2      = 20000,
    Quantity    = 1,
    Product = deep_merge(?PRODUCT, #{
        <<"product">>   => hash(Prod1),
        <<"quantity">>  => hash(Quantity),
        <<"price">>     => hash(Price1)
    }),
    Product2 = Product#{
        <<"product">> => hash(Prod2),
        <<"price">> => hash(Price2)
    },
    Invoice = #{
        <<"amount">>    => undefined,
        <<"product">>   => undefined,
        <<"shop_id">>   => hash(ShopID),
        <<"currency">>  => hash(Cur),
        <<"cart">>      => #{
            0 => Product,
            1 => Product2
        }
    },
    Request = #{
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"cart">> => [
            #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2},
            #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1}
        ]
    },
    ?assertEqual(Invoice, read_features(capi_feature_schemas:invoice(), Request)).

-spec compare_invoices_test() -> _.
compare_invoices_test() ->
    ShopID      = <<"shopus">>,
    Cur         = <<"RUB">>,
    Prod1       = <<"yellow duck">>,
    Prod2       = <<"blue duck">>,
    Price1      = 10000,
    Price2      = 20000,
    Product = #{
        <<"product">> => Prod1,
        <<"quantity">> => 1,
        <<"price">> => Price1,
        <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>,
            <<"rate">> => <<"10%">>
        }
    },
    Request1 = #{
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"cart">> => [Product]
    },
    Request2 = deep_merge(Request1, #{
        <<"cart">> => [#{<<"product">> => Prod2, <<"price">> => Price2}]
    }),
    Request3 = deep_merge(Request1, #{
        <<"cart">> => [#{<<"product">> => Prod2, <<"price">> => Price2, <<"quantity">> => undefined}]
    }),
    Schema = capi_feature_schemas:invoice(),
    Invoice1 = read_features(Schema, Request1),
    InvoiceChg1 = read_features(Schema, Request1#{<<"cart">> => [
        Product#{
            <<"price">> => Price2,
            <<"taxMode">> => #{
                <<"rate">> => <<"18%">>
            }}
    ]}),
    Invoice2 = read_features(Schema, Request2),
    InvoiceWithFullCart = read_features(Schema, Request3),

    ?assertEqual({false, #{<<"cart">> => #{
        0 => #{
            <<"price">>     => hash(Price2),
            <<"product">>   => hash(Prod2),
            <<"quantity">>  => undefined,
            <<"tax">>       => undefined
    }}}}, equal_features(Invoice2, Invoice1)),
    ?assert(equal_features(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(equal_features(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(equal_features(Invoice2, InvoiceWithFullCart)),
    % %% When second request didn't contain feature, this situation detected as conflict.
    ?assertMatch({false, #{<<"cart">> := undefined}}, equal_features(Invoice1#{<<"cart">> => undefined}, Invoice1)),

    {false, Diff} = equal_features(Invoice1, InvoiceChg1),
    ?assertEqual([<<"cart.0.taxMode.rate">>, <<"cart.0.price">>], list_diff_fields(Schema, Diff)),
    ?assert(equal_features(Invoice1, Invoice1#{<<"cart">> => undefined})).

-endif.
