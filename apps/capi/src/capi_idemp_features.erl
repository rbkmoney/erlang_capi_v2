-module(capi_idemp_features).

-define(DIFFERENCE, -1).

-type request()         :: #{binary() := request_value()}.
-type request_value()   :: integer() | binary() | request() | [request()].
-type difference()      :: features().
-type feature_name()    :: binary().
-type feature_value()   :: integer() | features() | [features()].
-type features()        :: #{feature_name() := feature_value() | undefined}.

-export_type([difference/0]).
-export_type([features/0]).

-export([read/2]).
-export([compare/2]).
-export([list_diff_fields/2]).

-spec read(capi_feature_schemas:schema(), request()) ->
    {features(), request()}.

read(Schema, Request) ->
    {Features, Trace} = read_(Schema, Request),
    Diff = unused_params(Request, Trace),
    {Features, Diff}.

unused_params(Params, UsedParams) ->
    maps:fold(fun(K, ParamValue, DiffIn) ->
        case maps:get(K, UsedParams, undefined) of
            undefined ->
                DiffIn#{K => ParamValue};
            UsedValues when is_list(UsedValues) and is_list(ParamValue) ->
                case compare_params(lists:sort(ParamValue), UsedValues, []) of
                    [] ->
                         DiffIn;
                    DiffOut ->
                        DiffIn#{K => DiffOut}
                end;
            UsedValue when is_map(UsedValue) and is_map(ParamValue) ->
                DiffOut = unused_params(ParamValue, UsedValue),
                case maps:size(DiffOut) of
                    0 ->
                        DiffIn;
                    _ ->
                        DiffIn#{K => DiffOut}
                end;
            ParamValue ->
                DiffIn;
            _ValueOther ->
                DiffIn#{K => ParamValue}
        end
    end, #{}, Params).

compare_params([], UsedVs, DiffIn) ->
    UsedVs ++ DiffIn;
compare_params([Value | Values], [ValueUsed | UsedVs], DiffIn) ->
    DiffOut = unused_params(Value, ValueUsed),
    case maps:size(DiffOut) of
        0 ->
           compare_params(Values, UsedVs, DiffIn);
        _ ->
           compare_params(Values, UsedVs, [DiffOut | DiffIn])
    end.

read_(Schema, Request) ->
    maps:fold(
        fun
            (Name, Fs, {Acc, TraceIn}) when is_map(Fs) ->
                {Value, Trace} = read_(Fs, Request),
                {Acc#{Name => Value}, maps:merge(Trace, TraceIn)};
            (Name, Accessor, {Acc, TraceIn}) when is_list(Accessor) ->
                {FeatureValue, Trace} = read_request_value(Accessor, Request),
                {Acc#{Name => FeatureValue}, maps:merge(TraceIn, Trace)};
            (_Name, 'reserved', Acc) ->
                Acc
        end,
        {#{}, #{}},
        Schema
    ).

read_request_value([], undefined) ->
    {undefined, undefined};
read_request_value([], V) ->
    {hash(V), V};
read_request_value([Schema = #{}], Request = #{}) ->
    read_(Schema, Request);
read_request_value([{set, [Schema = #{}]}], List) when is_list(List) ->
    {_, Value, Trace} = lists:foldl(fun(Req, {N, Acc, TraceIn}) ->
        {Value, TraceOut} = read_(Schema, Req),
        {N + 1, Acc#{N => Value}, [TraceOut | TraceIn]}
    end, {0, #{}, []}, lists:sort(List)),
    {Value, lists:reverse(Trace)};
read_request_value([Key | Rest], Request = #{}) when is_binary(Key) ->
    {Value, Trace} = read_request_value(Rest, maps:get(Key, Request, undefined)),
    {Value, #{Key => Trace}};
read_request_value(_, undefined) ->
    {undefined, undefined};
read_request_value(Key, Request) ->
    logger:warning("Unable to extract idemp feature with schema: ~p from client request subset: ~p", [Key, Request]),
    {undefined, undefined}.

hash(V) ->
    erlang:phash2(V).

-spec list_diff_fields(capi_feature_schemas:schema(), difference()) ->
    [binary()].

list_diff_fields(Schema, Diff) ->
    ConvertedDiff = features_to_schema(Diff, Schema),
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

features_to_schema(Diff, Schema) ->
    zipfold(
        fun
            (_Feature, ?DIFFERENCE, [Key, _SchemaPart], AccIn)  ->
                [[Key] | AccIn];
            (_Feature, Value, [Key, SchemaPart], AccIn) when is_map(SchemaPart) and is_map(Value) ->
                List = add(Key, features_to_schema(Value, SchemaPart)),
                List ++ AccIn;
            (_Feature, Values, [Key, {set, [SchemaPart]}], AccIn) when is_map(SchemaPart) and is_map(Values) ->
                Result = maps:fold(fun(Index, Value, Acc) ->
                    List = add([Key, Index], features_to_schema(Value, SchemaPart)),
                    List ++ Acc
                end, [], Values),
                Result ++ AccIn;
            (_Feature, _Value, [Key], AccIn) when is_binary(Key) ->
                [[Key] | AccIn];
            (_Feature, Value, SchemaPart, AccIn) when is_map(SchemaPart) ->
                Result = features_to_schema(Value, SchemaPart),
                Result ++ AccIn
        end,
        [],
        Diff,
        Schema).

add(Key, [H | _] = Lists) when is_binary(Key) and is_list(H) ->
    lists:foldl(fun(List, Acc) ->
        [[Key | List] | Acc]
    end, [], Lists);
add(Keys, [H | _] = Lists) when is_list(Keys) and is_list(H) ->
    lists:foldl(fun(List, Acc) ->
        [lists:merge(Keys, List) | Acc]
    end, [], Lists).


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
            (Key, Value, ValueWith, Diff) when is_map(ValueWith) and is_map(Value) ->
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
                end;
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
    Category    = <<"BUSINESS">>,
    ExpDate     = {exp_date, 02, 2022},
    Request = #{
        <<"flow">> => #{
            <<"type">> => <<"PaymentFlowHold">>
        },
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => ToolType,
                <<"token">>           => Token,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder,
                <<"category">>        => Category
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
    Diff = #{
        <<"flow">> => #{<<"type">> => <<"PaymentFlowHold">>},
        <<"payer">> => #{<<"paymentTool">> => #{<<"category">> => Category}}
    },
    {Features, RequestNotUse} = read(capi_feature_schemas:payment(), Request),
    ?assertEqual(Payer, Features),
    ?assertEqual(Diff, RequestNotUse).

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
    ?assertMatch({Payer, #{}}, read(capi_feature_schemas:payment(), Request)).

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
    {F1, #{}} = read(Schema, Request1),
    {F2, #{}} = read(Schema, Request2),
    ?assertEqual(true, compare(F1, F1)),
    {false, Diff} = compare(F1, F2),
    ?assertEqual([
        <<"payer.paymentTool.cardholder_name">>,
        <<"payer.paymentTool.token">>
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
    {F1, #{}} = read(Schema, Request1),
    {F2, #{}} = read(Schema, Request2),
    ?assertEqual(true, compare(F1, F1)),
    {false, Diff} = compare(F1, F2),
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
            0 => Product2,
            1 => Product
        }
    },
    Request = #{
        <<"externalID">>  => <<"externalID">>,
        <<"dueDate">>     => <<"2019-08-24T14:15:22Z">>,
        <<"shopID">>      => ShopID,
        <<"currency">>    => Cur,
        <<"description">> => <<"Wild birds.">>,
        <<"cart">> => [
            #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2},
            #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1, <<"not feature">> => <<"hmm">>}
        ],
        <<"metadata">> => #{}
    },
    {Features, RequestNotUse} = read(capi_feature_schemas:invoice(), Request),
    ?assertEqual(Invoice, Features),
    ?assertEqual(#{
        <<"externalID">>  => <<"externalID">>,
        <<"dueDate">>     => <<"2019-08-24T14:15:22Z">>,
        <<"description">> => <<"Wild birds.">>,
        <<"metadata">>    => #{},
        <<"cart">> => [#{<<"not feature">> => <<"hmm">>}]
    }, RequestNotUse).

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
    {Invoice1, #{}} = read(Schema, Request1),
    {InvoiceChg1, #{}} = read(Schema, Request1#{<<"cart">> => [
        Product#{
            <<"price">> => Price2,
            <<"taxMode">> => #{
                <<"rate">> => <<"18%">>
            }}
    ]}),
    {Invoice2, #{}} = read(Schema, Request2),
    {InvoiceWithFullCart, #{}} = read(Schema, Request3),

    ?assertEqual({false, #{<<"cart">> => #{
        0 => #{
            <<"price">>     => ?DIFFERENCE,
            <<"product">>   => ?DIFFERENCE,
            <<"quantity">>  => ?DIFFERENCE,
            <<"tax">>       => ?DIFFERENCE
    }}}}, compare(Invoice2, Invoice1)),
    ?assert(compare(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(compare(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(compare(Invoice2, InvoiceWithFullCart)),
    % %% When second request didn't contain feature, this situation detected as conflict.
    ?assertMatch({false, #{<<"cart">> := ?DIFFERENCE}}, compare(Invoice1#{<<"cart">> => undefined}, Invoice1)),

    {false, Diff} = compare(Invoice1, InvoiceChg1),
    ?assertEqual([<<"cart.0.taxMode.rate">>, <<"cart.0.price">>], list_diff_fields(Schema, Diff)),
    ?assert(compare(Invoice1, Invoice1#{<<"cart">> => undefined})).

-endif.