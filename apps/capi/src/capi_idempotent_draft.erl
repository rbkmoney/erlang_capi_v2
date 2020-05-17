-module(capi_idempotent_draft).

-include_lib("eunit/include/eunit.hrl").
-define(DIFFERENCE, -1).

-type schema()          :: {schema_type(), schema_body()}.
-type schema_type()     :: payment | invoice | refund.
-type schema_body()     :: #{feature_name() := [accessor() | schema_body() | [schema_body()]]}.
-type accessor()        :: binary(). % name of field in a map

-type request()         :: #{binary() := request_value()}.
-type request_value()   :: integer() | binary() | request() | [request()].

-type features()        :: {schema_type(), #{feature_name() := feature_value()}}.
-type difference()      :: features().
-type feature_name()    :: binary().
-type feature_value()   :: integer() | features() | [features()].

-export_type([schema/0]).

-export([get_schema/1]).
-export([payment_schema/0]).
-export([invoice_schema/0]).
-export([refund_schema/0]).
-export([read_payment_features/1]).
-export([read_invoice_features/1]).
-export([read_refund_features/1]).
-export([read_features/2]).
-export([equal_features/2]).
-export([handle_diff/2]).

-spec get_schema(schema_type()) ->
    schema().

get_schema(payment = Type) ->
    {Type, payment_schema()};
get_schema(invoice = Type) ->
    {Type, invoice_schema()};
get_schema(refund = Type) ->
    {Type, refund_schema()}.

-spec payment_schema() -> schema_body().

payment_schema() -> #{
    <<"payer">> => [<<"payer">>, #{
        <<"type">> => [<<"payerType">>],
        <<"tool">> => [<<"paymentTool">>,
            {filter, [<<"type">>], #{
                <<"bank_card">> => #{
                    <<"type">>       => [<<"type">>],
                    <<"token">>      => [<<"token">>],
                    <<"cardholder">> => [<<"cardholder_name">>],
                    <<"expdate">>    => [<<"exp_date">>]
                },
                <<"terminal">> => #{
                    <<"type">>          => [<<"type">>],
                    <<"terminal_type">> => [<<"terminal_type">>]
                },
                <<"wallet">> => #{
                    <<"type">>     => [<<"type">>],
                    <<"provider">> => [<<"provider">>],
                    <<"id">>       => [<<"id">>],
                    <<"token">>    => [<<"token">>]
                },
                <<"crypto">> => #{
                    <<"type">>     => [<<"type">>],
                    <<"currency">> => [<<"currency">>]
                },
                <<"mobile_commerce">> => #{
                    <<"type">>     => [<<"type">>],
                    <<"operator">> => [<<"operator">>],
                    <<"phone">>    => [<<"phone">>]
                }
            }}
        ],
        <<"customer">> => [<<"customerID">>],
        <<"recurrent">> => [<<"recurrentParentPayment">>, #{
            <<"invoice">> => [<<"invoiceID">>],
            <<"payment">> => [<<"paymentID">>]
        }]
    }]
}.

-spec invoice_schema() -> schema_body().

invoice_schema() -> #{
    <<"shop_id">>   => [<<"shopID">>],
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"product">>   => [<<"product">>],
    <<"cart">>      => [<<"cart">>, [cart_schema()]]
}.

-spec refund_schema() -> schema_body().

refund_schema() -> #{
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"cart">>      => [<<"cart">>, [cart_schema()]]
}.

-spec cart_schema() -> schema_body().

cart_schema() ->
    #{
        <<"product">>  => [<<"product">>],
        <<"quantity">> => [<<"quantity">>],
        <<"price">>    => [<<"price">>],
        <<"tax">>      => #{
            <<"type">> => [<<"type">>],
            <<"rate">> => [<<"rate">>]
        }
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

read_features({Type, Features}, Request) ->
    Result = read_features_(Features, Request),
    {Type, Result}.

read_features_(Features, Request) ->
    maps:fold(
        fun
            (Name, Fs = #{}, Acc) ->
                Acc#{Name => read_features_(Fs, Request)};
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
    hash(V);
read_request_value([{filter, _, _Schemas}], undefined) ->
    undefined;
read_request_value([{filter, [Key], Schemas}], Request) ->
    Filter = maps:get(Key, Request, undefined),
    case maps:get(Filter, Schemas, undefined) of
        undefined ->
            undefined;
        Schema ->
            {Filter, read_features_(Schema, Request)}
    end;
read_request_value([Schema = #{}], Request = #{}) ->
    read_features_(Schema, Request);
read_request_value([[Schema = #{}]], List) when is_list(List) ->
    lists:map(fun (Req) -> read_features_(Schema, Req) end, List);
read_request_value([Key | Rest], Request = #{}) when is_binary(Key) ->
    read_request_value(Rest, maps:get(Key, Request, undefined));
read_request_value(_, undefined) ->
    undefined;
read_request_value(Key, Request) ->
    % TODO logger:warning() if not `undefined` probably
    logger:warning("Read request value is not undefined ~nKey: ~p~nRequest: ~p", [Key, Request]),
    undefined.

hash(V) ->
    erlang:phash2(V).

-spec handle_diff(schema_type(), difference()) ->
    [binary()].
handle_diff(Diff, Features) ->
    ConvertedDiff = features_to_schema(Diff, Features),
    ?debugFmt(">>> ~p", [ConvertedDiff]),
    to_flat(ConvertedDiff).

features_to_schema(Diff, Features) ->
    zipfold(
        fun
            (_Feature, Value, [Key, ValueWith], AccIn) when is_map(ValueWith) and is_map(Value) ->
                % ?debugFmt("~n1 - Feature:~p V: ~p ~n---~n~p----~nAcc:~p", [Feature, Value, ValueWith, AccIn]),
                AccIn#{Key => features_to_schema(Value, ValueWith)};
            (_Feature, {FilterValue, Value}, [Key, {filter, _, ValueWith}], AccIn) ->
                % ?debugFmt("~n2 - Feature:~pFilterValue[~p] V: ~p ~n---~n~p----~nAcc:~p", [Feature, FilterValue, Value, ValueWith, AccIn]),
                FilteredValue = maps:get(FilterValue, ValueWith),
                AccIn#{Key => features_to_schema(Value, FilteredValue)};
            (_Feature, Value, [Key], AccIn) when is_binary(Key) ->
                % ?debugFmt("~n3 - Feature:~p ~nV: ~p ~n---~nKey: ~n~p----~nAcc:~p", [Feature, Value, Key, AccIn]),
                AccIn#{Key => Value}
        end,
        #{},
        Diff,
        Features).

to_flat(_) ->
    #{}.

-spec equal_features(features(), features()) ->
    true | {false, {schema_type(), difference()}}.

equal_features({Type, Features}, {Type, FeaturesWith}) ->
    case compare_features(Features, FeaturesWith) of
        Diff when map_size(Diff) > 0 ->
            {false, {Type, Diff}};
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
                    Diff1 when map_size(Diff1) > 0 ->
                        Diff#{Key => Diff1};
                    #{} ->
                        Diff % no notable differences
                end;
            (Key, {Filter, Value}, {Filter, ValueWith}, Diff) when is_map(ValueWith) and is_map(Value) ->
                %% TODO refact double code lines
                case compare_features(Value, ValueWith) of
                    ValueWith ->
                        Diff#{Key => {Filter, ?DIFFERENCE}}; % different everywhere
                    Diff1 when map_size(Diff1) > 0 ->
                        Diff#{Key => {Filter, Diff1}};
                    #{} ->
                        Diff % no notable differences
                end;
            (Key, {FilterValue, Value}, {_, ValueWith}, Diff) when is_map(ValueWith) and is_map(Value) ->
                Diff#{Key => FilterValue};
            (Key, [#{} | _] = ListValues, [#{} | _] = ListValuesWith, Diff) ->
                case compare_list_features(ListValues, ListValuesWith) of
                    false ->
                        Diff#{Key => ?DIFFERENCE};
                    true ->
                        Diff
                end;
            %% We expect that clients may _at any time_ change their implementation and start
            %% sending information they were not sending beforehand, so this is not considered a
            %% conflict. Yet, we DO NOT expect them to do the opposite, to stop sending
            %% information they were sending, this is still a conflict.
            (_Key, _Value, undefined, Diff) ->
                Diff;
            (Key, _, [#{} | _] = _ValueWith, Diff) ->
                Diff#{Key => ?DIFFERENCE};
            (Key, [#{} | _],  _, Diff) ->
                Diff#{Key => ?DIFFERENCE};
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
                    % ?debugFmt("zipfold(ok) ~p~n----end~n", [{Key, M2, V2}]),
                    Fun(Key, V1, V2, AccIn);
                error ->
                    % ?debugFmt("zipfold(error) ~p", [{Key, M2}]),
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

deep_merge(Map, Values) ->
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
    Payer = deep_merge(?PAYER, [
        {[<<"payer">>, <<"type">>],                    hash(PayerType)},
        {[<<"payer">>, <<"tool">>, <<"type">>],        hash(Tool)},
        {[<<"payer">>, <<"tool">>, <<"token">>],       hash(Token)},
        {[<<"payer">>, <<"tool">>, <<"expdate">>],     hash(ExpDate)},
        {[<<"payer">>, <<"tool">>, <<"cardholder">>],  hash(CardHolder)}
    ]),
    SchemaType = payment,
    ?assertEqual({SchemaType, Payer}, read_features(get_schema(SchemaType), Request)).

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
    Payer = deep_merge(?PAYER, [
        {[<<"payer">>, <<"type">>],     hash(PayerType)},
        {[<<"payer">>, <<"customer">>], hash(CustomerID)},
        {[<<"payer">>, <<"tool">>],     undefined}
    ]),
    SchemaType = payment,
    ?assertEqual({SchemaType, Payer}, read_features(get_schema(SchemaType), Request)).

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
    SchemaType = payment,
    F1 = read_features(get_schema(SchemaType), Request1),
    F2 = read_features(get_schema(SchemaType), Request2),
    ?assertEqual(true, equal_features(F1, F1)),
    {false, {Type, Diff}} = equal_features(F1, F2),
    ?debugFmt(" ~n>>> Diff >>>~n ~p", [Diff]),
    {_, Features} = get_schema(Type),
    Result = handle_diff(Diff, Features),
    ?debugFmt(" ~n>>> Result >>>~n ~p", [Result]),
    ok.
    % ?assertEqual({false, #{
    %     <<"payer">> => #{
    %         <<"tool">> => #{
    %             <<"bank_card">> => #{
    %                 <<"token">> => hash(Token1)
    %             },
    %             <<"wallet">> => #{
    %                 <<"token">> => hash(Token1)
    %             }
    %     }
    % }}}, equal_features(F1, F2)).

% -spec read_invoice_features_value_test() -> _.
% read_invoice_features_value_test() ->
%     ShopID      = <<"shopus">>,
%     Cur         = <<"XXX">>,
%     Prod1       = <<"yellow duck">>,
%     Prod2       = <<"blue duck">>,
%     Price1      = 10000,
%     Price2      = 20000,
%     Quantity    = 1,
%     Product = deep_merge(?PRODUCT, [
%        {[<<"product">>],  hash(Prod1)},
%        {[<<"quantity">>], hash(Quantity)},
%        {[<<"price">>],    hash(Price1)}
%     ]),
%     Product2 = Product#{
%         <<"product">> => hash(Prod2),
%         <<"price">> => hash(Price2)
%     },
%     Invoice = deep_merge(?INVOICE, [
%         {[<<"shop_id">>], hash(ShopID)},
%         {[<<"currency">>], hash(Cur)},
%         {[<<"cart">>], [Product, Product2]}
%     ]),
%     Request = #{
%         <<"shopID">> => ShopID,
%         <<"currency">> => Cur,
%         <<"cart">> => [
%             #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1},
%             #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2}
%         ]
%     },
%     ?assertEqual(Invoice, read_features(invoice_schema(), Request)).

% -spec compare_invoices_test() -> _.
% compare_invoices_test() ->
%     ShopID      = <<"shopus">>,
%     Cur         = <<"RUB">>,
%     Prod1       = <<"yellow duck">>,
%     Prod2       = <<"blue duck">>,
%     Price1      = 10000,
%     Price2      = 20000,
%     Quantity    = 1,
%     Product1 = deep_merge(?PRODUCT, [
%        {[<<"product">>],  hash(Prod1)},
%        {[<<"quantity">>], hash(Quantity)},
%        {[<<"price">>],    hash(Price1)}
%     ]),
%     Invoice1 = deep_merge(?INVOICE, [
%         {[<<"shop_id">>], hash(ShopID)},
%         {[<<"currency">>], hash(Cur)},
%         {[<<"cart">>], [Product1]}
%     ]),
%     Product2 = deep_merge(?PRODUCT, [
%        {[<<"product">>],  hash(Prod2)},
%        {[<<"price">>],    hash(Price2)}
%     ]),
%     Product2_ = Product2#{
%         <<"quantity">> => undefined
%     },
%     Invoice2 = Invoice1#{<<"cart">> => [Product2]},
%     InvoiceWithFullCart = Invoice2#{cart => [Product2_]},
%     ?assertEqual({false, #{<<"cart">> => ?DIFFERENCE}}, equal_features(Invoice2, Invoice1)),
%     ?assert(equal_features(Invoice1, Invoice1)),
%     %% Feature was deleted
%     ?assert(equal_features(InvoiceWithFullCart, Invoice2)),
%     %% Feature was add
%     ?assert(equal_features(Invoice2, InvoiceWithFullCart)),
%     %% When second request didn't contain feature, this situation detected as conflict.
%     ?assertEqual({false, #{<<"cart">> => ?DIFFERENCE}}, equal_features(Invoice1#{<<"cart">> => undefined}, Invoice1)),

%     ?assert(equal_features(Invoice1, Invoice1#{<<"cart">> => undefined})).


-endif.
