-module(capi_idempotent_new).

-type features()     :: #{feature_name() => any()}.
-type feature_name() :: atom().
-type payment_swag() :: any().
-type invoice_swag() :: any().
-type refund_swag()  :: any().
-type difference()   :: [feature_name()].

-export([payment_features/1]).
-export([invoice_features/1]).
-export([refund_features/1]).
-export([compare_features/2]).

-spec payment_features(payment_swag()) ->
    features().

payment_features(PaymentSwag) ->
    Features = [
        {'payer',                                  [<<"payer">>, <<"payerType">>]},
        {'payer.paymentTool',                      [<<"payer">>, <<"paymentTool">>, <<"type">>]},
        {'payer.paymentTool.token',                [<<"payer">>, <<"paymentTool">>, <<"token">>]},
        {'payer.paymentTool.cardholder_name',      [<<"payer">>, <<"paymentTool">>, <<"cardholder_name">>]},
        {'payer.paymentTool.exp_date',             [<<"payer">>, <<"paymentTool">>, <<"exp_date">>]},

        {'payer.CustomerID',                       [<<"payer">>, <<"CustomerID">>]},
        {'payer.recurrentParentPayment.invoiceID', [<<"payer">>, <<"recurrentParentPayment">>, <<"invoiceID">>]},
        {'payer.recurrentParentPayment.paymentID', [<<"payer">>, <<"recurrentParentPayment">>, <<"paymentID">>]}
    ],
    read_features_value(Features, PaymentSwag).

%% payer.payerType = CustomerPayer, PaymentResourcePayer, RecurrentPayer
% foo() ->
% #{
%     payer => {
%         {swag, [<<"payer">>, <<"payerType">>]}, #{
%         <<"CustomerPayer">> => #{
%             customerID => {swag, [<<"payer">>, <<"CustomerID">>]}
%         },
%         <<"PaymentResourcePayer">> => {
%             {swag, [<<"payer">>, <<"paymentTool">>, <<"type">>]}, #{
%                 <<"bank_card">> => #{
%                     token           => {swag, [<<"payer">>, <<"paymentTool">>, <<"token">>]},
%                     cardholder_name => {swag, [<<"payer">>, <<"paymentTool">>, <<"cardholder_name">>]},
%                     exp_date        => {swag, [<<"payer">>, <<"paymentTool">>, <<"exp_date">>]}
%                 },
%                 <<"digital_wallet">> => #{
%                     provider => {swag, [<<"payer">>, <<"paymentTool">>, <<"provider">>]},
%                     id       => {swag, [<<"payer">>, <<"paymentTool">>, <<"id">>]},
%                     token    => {swag, [<<"payer">>, <<"paymentTool">>, <<"token">>]}
%                 }
%             }
%         }}

%     }
% }.

-spec invoice_features(invoice_swag()) ->
    features().

invoice_features(InvoiceSwag) ->
    Features = [
        {'shopID',               [<<"shopID">>]},
        {'amount',               [<<"amount">>]},
        {'currency',             [<<"currency">>]},
        {'product',              [<<"product">>]},
        {'cart',     fun hash/1, [<<"cart">>]}
    ],
    read_features_value(Features, InvoiceSwag).

-spec refund_features(refund_swag()) ->
    features().

refund_features(RefundSwag) ->
    Features = [
        {'invoiceID',            [<<"invoiceID">>]},
        {'paymentID',            [<<"paymentID">>]},
        {'amount',               [<<"amount">>]},
        {'currency',             [<<"currency">>]},
        {'cart',     fun hash/1, [<<"cart">>]}
    ],
    read_features_value(Features, RefundSwag).

-spec compare_features(features(), features()) ->
    true | {false, difference()}.

compare_features(Features, Features) ->
    true;
compare_features(Features, FeaturesOld) ->
    Keys = maps:keys(FeaturesOld),
    maps:fold(fun(K, Value, Acc) ->
        case maps:get(K, FeaturesOld) of
            Value -> Acc;
            _OtherValue -> [K | Acc]
        end
    end, [], maps:with(Keys, Features)).

read_features_value(Features, Swag) ->
    lists:foldl(fun
        ({FeatureName, Keys}, Acc) ->
            Value = get_swag_value(Keys, Swag),
            Acc#{FeatureName => Value};
        ({FeatureName, Fun, Keys}, Acc) ->
            Value = get_swag_value(Keys, Swag),
            Acc#{FeatureName => Fun(Value)}
    end, #{}, Features).

get_swag_value(_, undefined) ->
    undefined;
get_swag_value([], Value) ->
    Value;
get_swag_value([Key | Tail], Swag) ->
    Value = maps:get(Key, Swag, undefined),
    get_swag_value(Tail, Value).

hash(Value) ->
    erlang:phash2(Value).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec compare_features_test() -> _.
compare_features_test() ->
    Features1 = #{
        'payer' => a,
        'payer.paymentTool' => <<"bank_card">>,
        'payer.paymentTool.token' => <<"cds_token">>
    },
    Features2 = #{
        'payer' => b,
        'payer.customerID' => <<"customer id">>
    },
    Features3 = Features1#{
        'payer.paymentTool' => <<"digital_wallet">>,
        'payer.paymentTool.token' => <<"digital_wallet_token">>
    },
    ?assertEqual([payer], compare_features(Features2, Features1)),
    ?assertEqual(['payer.paymentTool', 'payer.paymentTool.token'], compare_features(Features3, Features1).

-endif.
