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

-export([payment_features_2/1]).
% -export([compare_features_2/2]).

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


%% ---------------
%% Second version
%% ---------------

-type pfeatures() :: #{
    payer => {payer_type(), customer() | payment_resource()}
}.


-type payer_type()        :: binary().
-type payment_tool_type() :: binary().
-type customer()          :: #{customerID => binary()}.
-type payment_resource()  :: {payment_tool_type(), bank_card() | payment_terminal()}.
-type bank_card()         :: #{
    token => binary(),
    cardholder_name => binary(),
    exp_date => any()
}.
-type payment_terminal() :: #{terminal_type => binary()}.

-spec payment_features_2(payment_swag()) -> pfeatures().

payment_features_2(Swag) ->
    PayerType       = get_swag_value([<<"payer">>, <<"payerType">>],                          Swag),
    CustomerID      = get_swag_value([<<"payer">>, <<"CustomerID">>],                         Swag),
    PaymentToolType = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"type">>],            Swag),
    Token           = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"token">>],           Swag),
    CardHolderName  = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"cardholder_name">>], Swag),
    ExpDate         = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"exp_date">>],        Swag),
    Provider        = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"provider">>],        Swag),
    ID              = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"id">>],              Swag),
    TerminalType    = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"terminal_type">>],   Swag),
    CryptoCurrency  = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"currency">>],        Swag),
    Operator        = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"operator">>],        Swag),
    Phone           = get_swag_value([<<"payer">>, <<"paymentTool">>, <<"phone">>],           Swag),
    gen_features(#{
        payer => {PayerType, #{
            <<"CustomerPayer">> => #{
                customerID => CustomerID
            },
            <<"PaymentResourcePayer">> => #{
                payment_tool => {PaymentToolType, #{
                    <<"bank_card">> => #{
                        token           => Token,
                        cardholder_name => CardHolderName,
                        exp_date        => ExpDate
                    },
                    <<"payment_terminal">> => #{
                        terminal_type => TerminalType
                    },
                    <<"digital_wallet">> => #{
                        provider => Provider,
                        id       => ID,
                        token    => Token
                    },
                    <<"crypto_currency">> => #{
                        currency => CryptoCurrency
                    },
                    <<"mobile_commerce">> => #{
                        operator => Operator,
                        phone => Phone
                    }
                }}
            }}
        }
    }).
-include_lib("eunit/include/eunit.hrl").

gen_features(Features) ->
    maps:fold(fun
        (K, Values, Acc) when is_map(Values) ->
            Acc#{K => gen_features(Values)};
        (K, {Name, Values}, Acc) when is_map(Values) ->
            case maps:get(Name, Values) of
                Map when is_map(Map) -> Acc#{K => {Name, gen_features(Map)}};
                OtherType ->
                    Acc#{K => OtherType}
            end;
        (K, Values, Acc) ->
            Acc#{K => Values}
    end, #{}, Features).


%% -----

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
% -include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec payment_features_test() -> _.
payment_features_test() ->
    Swag1 = #{
        <<"externalID">> => <<"abc123">>,
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentTool">> => #{
                <<"type">> => <<"bank_card">>,
                <<"token">> => <<"cds_token">>,
                <<"payment_system">> => <<"master_card">>,
                <<"cardholder_name">> => <<"Degus Degusovich">>,
                <<"exp_date">> => <<"10:10:2020">>
            }
        }
    },
    Swag2 = #{
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentTool">> => #{
                <<"type">> => <<"digital_wallet">>,
                <<"token">> => <<"digital_token">>
            }
        }
    },
    Features1 = payment_features_2(Swag1),
    Features2 = payment_features_2(Swag2),
    ?debugFmt("~nFeatures1[payment] >>> ~n~p", [Features1]),
    ?debugFmt("~nFeatures2[payment] >>> ~n~p", [Features2]),
    Diff = compare2(Features2, Features1),
    ?debugFmt("ITOG Diff >>> ~n~p", [Diff]),
    ok.

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
    ?assertEqual(compare_features(Features2, Features1), [payer]),
    ?assertEqual(compare_features(Features3, Features1), ['payer.paymentTool.token', 'payer.paymentTool']).

-endif.
