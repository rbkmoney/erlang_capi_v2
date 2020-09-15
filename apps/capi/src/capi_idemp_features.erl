-module(capi_idemp_features).

-include("capi_feature_schemas.hrl").

-define(DIFFERENCE, -1).

-type request()         :: #{binary() := request_value()}.
-type request_value()   :: integer() | binary() | request() | [request()].
-type difference()      :: features().
-type feature_name()    :: integer().
-type feature_value()   :: integer() | features() | [feature_value()] | undefined.
-type features()        :: #{feature_name() := feature_value()}.
-type schema()          :: #{feature_name() := [binary() | schema() | {set, schema()}]}.

-type event()           :: {invalid_schema_fragment, feature_name(), request()} |
                           {request_visited, {request, request()}} |
                           {request_key_index_visit, integer()} |
                           {request_key_index_visited, integer()} |
                           {request_key_visit, {key, integer(), request()}} |
                           {request_key_visited, {key, integer()}}.

-type options()         :: term().
-type event_handler()   :: {module(), options()} | undefined.

-export_type([event_handler/0]).
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

-callback handle_event(event(), options()) -> ok.

-spec read(schema(), request()) ->
    features().

read(Schema, Request) ->
    read(get_event_handler(), Schema, Request).

-spec read(event_handler(), schema(), request()) ->
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
    lists:foldl(fun({Index, Req}, Acc) ->
        handle_event(get_event_handler(Handler), {request_key_index_visit, Index}),
        Value = read_(Schema, Req, Handler),
        handle_event(get_event_handler(Handler), {request_key_index_visited, Index}),
        [[Index, Value] | Acc]
    end, [], ListSorted);
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

get_event_handler() ->
    genlib_app:env(capi, idempotence_event_handler).

get_event_handler({Mod, Options}) ->
    {Mod, Options};
get_event_handler(undefined) ->
    undefined.

hash(V) ->
    erlang:phash2(V).

-spec list_diff_fields(schema(), difference()) ->
    [binary()].

list_diff_fields(Schema, Diff) ->
    {ConvertedDiff, _} = list_diff_fields_(Diff, Schema, {[], []}),
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

list_diff_fields_(Diffs, {set, Schema}, Acc) when is_map(Schema) ->
    maps:fold(
        fun (I, Diff, {PathsAcc, PathRev}) ->
            list_diff_fields_(Diff, Schema, {PathsAcc, [I | PathRev]})
        end,
        Acc,
        Diffs
    );
list_diff_fields_(Diff, Schema, Acc) when is_map(Schema) ->
    zipfold(
        fun
            (_Feature, ?DIFFERENCE, [Key | _SchemaPart], {PathsAcc, PathRev}) ->
                Path = lists:reverse([Key | PathRev]),
                {[Path | PathsAcc], PathRev};
            (_Feature, DiffPart, SchemaPart, AccIn) ->
                list_diff_fields_(DiffPart, SchemaPart, AccIn)
        end,
        Acc,
        Diff,
        Schema);
list_diff_fields_(Diff, [Schema], Acc) ->
    list_diff_fields_(Diff, Schema, Acc);
list_diff_fields_(Diff, [Key | Schema], {PathsAcc, PathRev}) ->
    list_diff_fields_(Diff, Schema, {PathsAcc, [Key | PathRev]}).


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
            (Key, Values, ValuesWith, Diff) when
            is_list(ValuesWith), is_list(Values) ->
                compare_list_features(Key, Values, ValuesWith, Diff);
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


compare_list_features(Key, L1, L2, Diff) when
length(L1) =/= length(L2) ->
    Diff#{Key => ?DIFFERENCE};
compare_list_features(Key, L1, L2, Acc) ->
    case compare_list_features_(L1, L2, #{}) of
        Diff when map_size(Diff) > 0 ->
            Acc#{Key => Diff};
        #{} ->
            Acc
    end.

compare_list_features_([], [], Diff) ->
    Diff;
compare_list_features_([[Index, V1] | Values], [[_, V2] | ValuesWith], Acc) ->
    Diff = compare_features_(Index, V1, V2, Acc),
    compare_list_features_(Values, ValuesWith, Diff).

compare_features_(Key, Value, ValueWith, Diff) when
is_map(Value) and is_map(ValueWith) ->
    case compare_features(Value, ValueWith) of
        ValueWith ->
            Diff#{Key => ?DIFFERENCE}; % different everywhere
        #{?descriminator := _} ->
            % Different with regard to descriminator, semantically same as different everywhere.
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

deep_merge(M1, M2) ->
    maps:fold(
        fun (K, V, MAcc) when is_map(V) ->
                Value = deep_merge(maps:get(K, MAcc, #{}), V),
                MAcc#{K => Value};
            (K, V, MAcc) ->
                MAcc#{K => V}
        end, M1, M2).

-spec test() -> _.

-spec read_payment_features_test() ->
    _.

read_payment_features_test() ->
    PayerType   = <<"PaymentResourcePayer">>,
    ToolType    = <<"bank_card">>,
    Token       = <<"cds token">>,
    CardHolder  = <<"0x42">>,
    Category    = <<"BUSINESS">>,
    ExpDate     = {exp_date, 02, 2022},
    Flow        = <<"PaymentFlowHold">>,
    Request = #{
        <<"flow">> => #{
            <<"type">> => Flow
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
        ?invoice_id => undefined,
        ?make_recurrent => undefined,
        ?flow => #{
            ?descriminator => hash(Flow),
            ?hold_exp => undefined
        },
        ?payer => #{
            ?descriminator => hash(PayerType),
            ?customer => undefined,
            ?recurrent => undefined,
            ?tool => #{
                ?descriminator => hash(ToolType),
                ?bank_card => #{
                    ?expdate    => hash(ExpDate),
                    ?token      => hash(Token)},
                ?crypto => #{?currency => undefined},
                ?mobile_commerce => #{
                    ?operator => undefined,
                    ?phone    => undefined},
                ?terminal => #{?descriminator => undefined},
                ?wallet => #{
                    ?id        => undefined,
                    ?provider  => undefined,
                    ?token     => hash(Token)}
            }
        }
    },
    Features = read(capi_feature_schemas:payment(), Request),
    ?assertEqual(Payer, Features).

-spec compare_payment_bank_card_test() ->
    _.
compare_payment_bank_card_test() ->
    Token2      = <<"cds token 2">>,
    CardHolder2 = <<"Cake">>,

    PaymentTool1 = bank_card(),
    PaymentTool2 = PaymentTool1#{
        <<"token">> => Token2,
        <<"cardholder_name">> => CardHolder2
    },
    Request1 = payment_params(PaymentTool1),
    Request2 = payment_params(PaymentTool2),

    Schema = capi_feature_schemas:payment(),
    F1 = read(Schema, Request1),
    F2 = read(Schema, Request2),
    ?assertEqual(true, compare(F1, F1)),
    {false, Diff} = compare(F1, F2),
    ?assertEqual([
        <<"payer.paymentTool.token">>
    ], list_diff_fields(Schema, Diff)).

-spec compare_different_payment_tool_test() ->
    _.
compare_different_payment_tool_test() ->
    ToolType2   = <<"wallet">>,
    Token2      = <<"wallet token">>,
    PaymentTool1= bank_card(),
    PaymentTool2 = #{
        <<"type">>  => ToolType2,
        <<"token">> => Token2
    },
    Request1 = payment_params(PaymentTool1),
    Request2 = payment_params(PaymentTool2),
    Schema = capi_feature_schemas:payment(),
    F1 = read(Schema, Request1),
    F2= read(Schema, Request2),
    ?assertEqual(true, compare(F1, F1)),
    {false, Diff} = compare(F1, F2),
    ?assertEqual([<<"payer.paymentTool">>], list_diff_fields(Schema, Diff)).

-spec feature_multi_accessor_test() ->
    _.
feature_multi_accessor_test() ->
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => <<"PaymentResourcePayer">>,
            <<"paymentTool">> => #{
                <<"wrapper">> => bank_card()
            }
        }
    },
    Request2 = deep_merge(Request1, #{
        <<"payer">> => #{
            <<"paymentTool">> => #{<<"wrapper">> => #{
                <<"token">> => <<"cds token 2">>,
                <<"cardholder_name">> => <<"Cake">>
            }
        }
    }}),
    Schema = #{
        <<"payer">> => [<<"payer">>, #{
            <<"type">> => [<<"payerType">>],
            <<"tool">> => [<<"paymentTool">>, <<"wrapper">>, #{
                <<"$type">> => [<<"type">>],
                <<"bank_card">> => #{
                    <<"token">>      => [<<"token">>],
                    <<"expdate">>    => [<<"exp_date">>]
                }
            }]
        }]
    },
    F1 = read(Schema, Request1),
    F2 = read(Schema, Request2),
    ?assertEqual(true, compare(F1, F1)),
    {false, Diff} = compare(F1, F2),
    ?assertEqual([
        <<"payer.paymentTool.wrapper.token">>
    ], list_diff_fields(Schema, Diff)).

-spec read_payment_customer_features_value_test() ->
    _.
read_payment_customer_features_value_test() ->
    PayerType = <<"CustomerPayer">>,
    CustomerID = <<"some customer id">>,
    Request = #{
        <<"payer">> => #{
            <<"payerType">>  => PayerType,
            <<"customerID">> => CustomerID
        }
    },
    Features = read(capi_feature_schemas:payment(), Request),
    ?assertEqual(#{
        ?invoice_id => undefined,
        ?make_recurrent => undefined,
        ?flow => undefined,
        ?payer => #{
            ?descriminator => hash(PayerType),
            ?customer  => hash(CustomerID),
            ?recurrent => undefined,
            ?tool      => undefined
        }
    }, Features).

-spec read_invoice_features_test() ->
    _.
read_invoice_features_test() ->
    ShopID      = <<"shopus">>,
    Cur         = <<"XXX">>,
    Prod1       = <<"yellow duck">>,
    Prod2       = <<"blue duck">>,
    DueDate     = <<"2019-08-24T14:15:22Z">>,
    Price1      = 10000,
    Price2      = 20000,
    Quantity    = 1,
    Product = #{
        ?product   => hash(Prod1),
        ?quantity  => hash(Quantity),
        ?price     => hash(Price1),
        ?tax       => undefined
    },
    Product2 = Product#{
        ?product => hash(Prod2),
        ?price   => hash(Price2)
    },
    Invoice = #{
        ?amount    => undefined,
        ?currency  => hash(Cur),
        ?shop_id   => hash(ShopID),
        ?product   => undefined,
        ?due_date  => hash(DueDate),
        ?cart      => [
            [1, Product],
            [0, Product2]
        ]
    },
    Request = #{
        <<"externalID">>  => <<"externalID">>,
        <<"dueDate">>     => DueDate,
        <<"shopID">>      => ShopID,
        <<"currency">>    => Cur,
        <<"description">> => <<"Wild birds.">>,
        <<"cart">> => [
            #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2},
            #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1, <<"not feature">> => <<"hmm">>}
        ],
        <<"metadata">> => #{}
    },

    Features = read(capi_feature_schemas:invoice(), Request),
    ?assertEqual(Invoice, Features).

-spec compare_invoices_features_test() ->
    _.
compare_invoices_features_test() ->
    ShopID  = <<"shopus">>,
    Cur     = <<"RUB">>,
    Prod1   = <<"yellow duck">>,
    Prod2   = <<"blue duck">>,
    Price1  = 10000,
    Price2  = 20000,
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
    Invoice1 = read(Schema, Request1),
    InvoiceChg1 = read(Schema, Request1#{<<"cart">> => [
        Product#{
            <<"price">> => Price2,
            <<"taxMode">> => #{
                <<"rate">> => <<"18%">>
            }}
    ]}),
    Invoice2 = read(Schema, Request2),
    InvoiceWithFullCart = read(Schema, Request3),
    ?assertEqual({false, #{?cart => #{
        0 => #{
            ?price     => ?DIFFERENCE,
            ?product   => ?DIFFERENCE,
            ?quantity  => ?DIFFERENCE,
            ?tax       => ?DIFFERENCE
        }}
    }}, compare(Invoice2, Invoice1)),
    ?assert(compare(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(compare(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(compare(Invoice2, InvoiceWithFullCart)),
    %% When second request didn't contain feature, this situation detected as conflict.
    ?assertMatch(
        {false, #{?cart := ?DIFFERENCE}},
        compare(Invoice1#{?cart => undefined}, Invoice1)
    ),

    {false, Diff} = compare(Invoice1, InvoiceChg1),
    ?assertEqual(
        [<<"cart.0.price">>, <<"cart.0.taxMode.rate">>],
        list_diff_fields(Schema, Diff)
    ),
    ?assert(compare(Invoice1, Invoice1#{?cart => undefined})).

payment_params(ExternalID, MakeRecurrent) ->
    genlib_map:compact(#{
        <<"externalID">> => ExternalID,
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> => MakeRecurrent,
        <<"metadata">> => #{<<"bla">> => <<"*">>},
        <<"processingDeadline">> => <<"5m">>
    }).

payment_params(ExternalID, Jwe, ContactInfo, MakeRecurrent) ->
    Params = payment_params(ExternalID, MakeRecurrent),
    genlib_map:compact(Params#{
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => <<"payment.session">>,
            <<"paymentToolToken">> => Jwe,
            <<"contactInfo">> => ContactInfo
        }
    }).

payment_params(PaymentTool) ->
    Params = payment_params(<<"EID">>, <<"Jwe">>, #{}, false),
    PaymentParams = deep_merge(Params, #{<<"payer">> => #{<<"paymentTool">> => PaymentTool}}),
    PaymentParams.

bank_card() ->
    #{
        <<"type">>            => <<"bank_card">>,
        <<"token">>           => <<"cds token">>,
        <<"payment_system">>  => <<"visa">>,
        <<"bin">>             => <<"411111">>,
        <<"last_digits">>     => <<"1111">>,
        <<"exp_date">>        => <<"2019-08-24T14:15:22Z">>,
        <<"cardholder_name">> => <<"Degus Degusovich">>,
        <<"is_cvv_empty">>    => false
    }.

-endif.
