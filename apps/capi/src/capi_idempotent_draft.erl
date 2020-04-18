-module(capi_idempotent_draft).

-type schema()          :: #{feature_name() := [_]}.
-type features()        :: #{feature_name() := feature_value()}.
-type request()         :: capi_handler:request_data().
-type difference()      :: features().
-type feature_name()    :: binary().
-type feature_value()   :: integer() | features() | [features()].

-export([payment_schema/0]).
-export([invoice_schema/0]).
-export([refund_schema/0]).
-export([read_payment_features/1]).
-export([read_invoice_features/1]).
-export([read_refund_features/1]).
-export([read_features/2]).
-export([equal_features/2]).


-spec payment_schema() -> schema().

payment_schema() -> #{
    <<"payer">> => [<<"payer">>, #{
        <<"type">> => [<<"payerType">>],
        <<"tool">> => [<<"paymentTool">>, #{
            <<"type">>          => [<<"type">>],
            <<"token">>         => [<<"token">>],
            <<"cardholder">>    => [<<"cardholder_name">>],
            <<"expdate">>       => [<<"exp_date">>],
            % 'provider'      => 'reserved', % to mark some field as dangerous to reuse
            <<"provider">>      => [<<"provider">>],
            <<"id">>            => [<<"id">>],
            <<"terminal_type">> => [<<"terminal_type">>],
            <<"currency">>      => [<<"currency">>],
            <<"operator">>      => [<<"operator">>],
            <<"phone">>         => [<<"phone">>]
        }],
        <<"customer">> => [<<"customerID">>],
        <<"recurrent">> => [<<"recurrentParentPayment">>, #{
            <<"invoice">> => [<<"invoiceID">>],
            <<"payment">> => [<<"paymentID">>]
        }]
    }]
}.

-spec invoice_schema() -> schema().

invoice_schema() -> #{
    <<"shop_id">>   => [<<"shopID">>],
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"product">>   => [<<"product">>],
    <<"cart">>      => [<<"cart">>, [#{
            <<"product">>  => [<<"product">>],
            <<"quantity">> => [<<"quantity">>],
            <<"price">>    => [<<"price">>],
            <<"tax">>      => #{
                <<"type">> => [<<"type">>],
                <<"rate">> => [<<"rate">>]
            }
        }
    ]]
}.

-spec refund_schema() -> schema().

refund_schema() -> #{
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"cart">>      => [<<"cart">>, [#{
            <<"product">>  => [<<"product">>],
            <<"quantity">> => [<<"quantity">>],
            <<"price">>    => [<<"price">>],
            <<"tax">>      => #{
                <<"type">> => [<<"type">>],
                <<"rate">> => [<<"rate">>]
            }
        }
    ]]
}.

-spec read_payment_features(request()) -> features().

read_payment_features(Request) ->
    read_features(payment_schema(), Request).

-spec read_invoice_features(request()) -> features().

read_invoice_features(Request) ->
    read_features(invoice_schema(), Request).

-spec read_refund_features(request()) -> features().

read_refund_features(Request) ->
    read_features(refund_schema(), Request).


-spec read_features(schema(), request()) -> features().

read_features(Features, Request) ->
    maps:fold(
        fun
            (Name, Fs = #{}, Acc) ->
                Acc#{Name => read_features(Fs, Request)};
            (Name, Accessor, Acc) when is_list(Accessor) ->
                V = read_request_value(Accessor, Request),
                Acc#{Name => V};
            (_Name, 'reserved', Acc) ->
                Acc
        end,
        #{},
        Features
    ).

read_request_value([], undefined) ->
    undefined;
read_request_value([], V) ->
    % We probably should bark when `V` is not scalar, this generally means
    % that we made a mistake in feature schema somewhere
    hash(V);
read_request_value([Schema = #{}], Request = #{}) ->
    read_features(Schema, Request);
read_request_value([Key | [[Schema = #{}]]], Request = #{}) when is_binary(Key) ->
    case maps:get(Key, Request, undefined) of
        undefined ->
            undefined;
        ValueList -> lists:foldl(fun(Req, Acc) ->
            [read_features(Schema, Req) | Acc]
        end, [], ValueList)
    end;
read_request_value([Key | Rest], Request = #{}) when is_binary(Key) ->
    read_request_value(Rest, maps:get(Key, Request, undefined));
read_request_value(_, undefined) ->
    undefined;
read_request_value(_, _) ->
    % TODO logger:warning() if not `undefined` probably
    % Maybe we need throw error, because set wrong 'features schema'
    undefined.

hash(V) ->
    erlang:phash2(V).

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
                        Diff#{Key => '_'}; % different everywhere
                    Diff1 when map_size(Diff1) > 0 ->
                        Diff#{Key => Diff1};
                    #{} ->
                        Diff % no notable differences
                end;
            (Key, [#{} | _] = ListValues, [#{} | _] = ListValuesWith, Diff) ->
                case compare_list_features(ListValues, ListValuesWith) of
                    false ->
                        Diff#{Key => '_'};
                    true ->
                        Diff
                end;
            %% User can't receive response, but entity(payment, invoice ) was successfully created.
            %% User makes retries and set a feature value(not specify before).
            %% System return response with entity instead of external_id conflict.
            (_Key, _Value, undefined, Diff) ->
                Diff;
            (Key, _, [#{} | _] = _ValueWith, Diff) ->
                Diff#{Key => '_'};
            (Key, [#{} | _],  _, Diff) ->
                Diff#{Key => '_'};
            (_Key, Value, Value, Diff) ->
                Diff;
            (Key, Value, _ValueWith, Diff) ->
                Diff#{Key => Value}
        end,
        #{},
        Fs,
        FsWith
    ).

compare_list_features(L, L) ->
    true;
compare_list_features(L1, L2)
when length(L1) =/= length(L2) ->
    false;
compare_list_features([Fs | List1], [FsWith | List2]) ->
    case compare_features(Fs, FsWith) of
        Diff when map_size(Diff) > 0 ->
            false;
        _ ->
            compare_list_features(List1, List2)
    end.

zipfold(Fun, Acc, M1, M2) ->
    maps:fold(
        fun (Key, V1, AccIn) ->
            case maps:find(Key, M2) of
                {ok, V2} ->
                    Fun(Key, V1, V2, AccIn);
                error    -> AccIn
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
        <<"tool">> => #{
            <<"type">>            => undefined,
            <<"token">>           => undefined,
            <<"cardholder">>      => undefined,
            <<"expdate">>         => undefined,
            <<"currency">>        => undefined,
            <<"id">>              => undefined,
            <<"operator">>        => undefined,
            <<"phone">>           => undefined,
            <<"provider">>        => undefined,
            <<"terminal_type">>   => undefined
        }
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
    <<"tax">>      => #{
        <<"type">> => undefined,
        <<"rate">> => undefined
    }
}).

%% Test helpers

set_value({[Key], V}, Map) ->
    Map#{Key => V};
set_value({[Key| T], V}, Map) ->
    Map#{Key => set_value({T, V}, maps:get(Key, Map))}.

set_values(Map, Values) ->
    lists:foldl(fun set_value/2, Map, Values).

-spec test() -> _.

-spec read_payment_features_value_test() -> _.
read_payment_features_value_test() ->
    PayerType   = <<"PaymentResourcePayer">>,
    Tool        = <<"bank_card">>,
    Token       = <<"cds token">>,
    CardHolder  = <<"0x42">>,
    ExpDate     = {exp_date, 02, 2022},
    Request = #{
        <<"payer">> => #{
            <<"payerType">>     => PayerType,
            <<"paymentTool">>   => #{
                <<"type">>            => Tool,
                <<"token">>           => Token,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder
            }
    }},
    Payer = set_values(?PAYER, [
        {[<<"payer">>, <<"type">>],             hash(PayerType)},
        {[<<"payer">>, <<"tool">>, <<"type">>],       hash(Tool)},
        {[<<"payer">>, <<"tool">>, <<"token">>],      hash(Token)},
        {[<<"payer">>, <<"tool">>, <<"expdate">>],    hash(ExpDate)},
        {[<<"payer">>, <<"tool">>, <<"cardholder">>], hash(CardHolder)}
    ]),
    ?assertEqual(Payer, read_features(payment_schema(), Request)).

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
    Payer = set_values(?PAYER, [
        {[<<"payer">>, <<"type">>],     hash(PayerType)},
        {[<<"payer">>, <<"customer">>], hash(CustomerID)},
        {[<<"payer">>, <<"tool">>],     undefined}
    ]),
    ?assertEqual(Payer, read_features(payment_schema(), Request)).

-spec compare_payment_bank_card_test() -> _.
compare_payment_bank_card_test() ->
    PayerType   = <<"PaymentResourcePayer">>,
    Tool        = <<"bank_card">>,
    Token1      = <<"cds token">>,
    Token2      = <<"cds token 2">>,
    CardHolder  = <<"0x42">>,
    ExpDate     = {exp_date, 02, 2022},
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">>   => PayerType,
            <<"paymentTool">> => #{
                <<"type">>            => Tool,
                <<"token">>           => Token1,
                <<"exp_date">>        => ExpDate,
                <<"cardholder_name">> => CardHolder
            }
    }},
    Request2 = set_value({[<<"payer">>, <<"paymentTool">>, <<"token">>], Token2}, Request1),

    F1 = read_features(payment_schema(), Request1),
    F2 = read_features(payment_schema(), Request2),
    ?assertEqual(true, equal_features(F1, F1)),
    ?assertEqual({false, #{<<"payer">> => #{<<"tool">> => #{<<"token">> => hash(Token1)}}}}, equal_features(F1, F2)).

-spec read_invoice_features_value_test() -> _.
read_invoice_features_value_test() ->
    ShopID      = <<"shopus">>,
    Cur         = <<"XXX">>,
    Prod1       = <<"yellow duck">>,
    Prod2       = <<"blue duck">>,
    Price1      = 10000,
    Price2      = 20000,
    Quantity    = 1,
    Product = set_values(?PRODUCT, [
       {[<<"product">>],  hash(Prod1)},
       {[<<"quantity">>], hash(Quantity)},
       {[<<"price">>],    hash(Price1)}
    ]),
    Product2 = Product#{
        <<"product">> => hash(Prod2),
        <<"price">> => hash(Price2)
    },
    Invoice = set_values(?INVOICE, [
        {[<<"shop_id">>], hash(ShopID)},
        {[<<"currency">>], hash(Cur)},
        {[<<"cart">>], [Product, Product2]}
    ]),
    Request = #{
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"cart">> => [
            #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2},
            #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1}
        ]
    },
    ?assertEqual(Invoice, read_features(invoice_schema(), Request)).

-spec compare_invoices_test() -> _.
compare_invoices_test() ->
    ShopID      = <<"shopus">>,
    Cur         = <<"RUB">>,
    Prod1       = <<"yellow duck">>,
    Prod2       = <<"blue duck">>,
    Price1      = 10000,
    Price2      = 20000,
    Quantity    = 1,
    Product1 = set_values(?PRODUCT, [
       {[<<"product">>],  hash(Prod1)},
       {[<<"quantity">>], hash(Quantity)},
       {[<<"price">>],    hash(Price1)}
    ]),
    Invoice1 = set_values(?INVOICE, [
        {[<<"shop_id">>], hash(ShopID)},
        {[<<"currency">>], hash(Cur)},
        {[<<"cart">>], [Product1]}
    ]),
    Product2 = set_values(?PRODUCT, [
       {[<<"product">>],  hash(Prod2)},
       {[<<"price">>],    hash(Price2)}
    ]),
    Product2_ = Product2#{
        <<"quantity">> => undefined
    },
    Invoice2 = Invoice1#{<<"cart">> => [Product2]},
    InvoiceWithFullCart = Invoice2#{cart => [Product2_]},
    ?assertEqual({false, #{<<"cart">> => '_'}}, equal_features(Invoice2, Invoice1)),
    ?assert(equal_features(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(equal_features(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(equal_features(Invoice2, InvoiceWithFullCart)),
    %% When second request didn't contain feature, this situation detected as conflict.
    ?assertEqual({false, #{<<"cart">> => '_'}}, equal_features(Invoice1#{<<"cart">> => undefined}, Invoice1)),

    ?assert(equal_features(Invoice1, Invoice1#{<<"cart">> => undefined})).


-endif.