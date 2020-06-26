-module(capi_feature_schemas).

-type schema_type()     :: payment | invoice | refund.
-type schema()          :: #{binary() := [accessor() | schema() | [schema()] | filter()]}.
-type accessor()        :: binary(). % name of field in a map
-type filter()          :: {filter, [accessor()], schema()}.

-export_type([schema_type/0]).
-export_type([schema/0]).

-export([payment/0]).
-export([invoice/0]).
-export([refund/0]).

-spec payment() -> schema().

payment() -> #{
    <<"payer">> => [<<"payer">>, #{
        <<"type">> => [<<"payerType">>],
        <<"tool">> => [<<"paymentTool">>, #{
            <<"$type">> => [<<"type">>],
            <<"bank_card">> => #{
                <<"token">>      => [<<"token">>],
                <<"cardholder">> => [<<"cardholder_name">>],
                <<"expdate">>    => [<<"exp_date">>]
            },
            <<"terminal">> => #{
                <<"terminal_type">> => [<<"terminal_type">>]
            },
            <<"wallet">> => #{
                <<"provider">> => [<<"provider">>],
                <<"id">>       => [<<"id">>],
                <<"token">>    => [<<"token">>]
            },
            <<"crypto">> => #{
                <<"currency">> => [<<"currency">>]
            },
            <<"mobile_commerce">> => #{
                <<"operator">> => [<<"operator">>],
                <<"phone">>    => [<<"phone">>]
            }
        }],
        <<"customer">> => [<<"customerID">>],
        <<"recurrent">> => [<<"recurrentParentPayment">>, #{
            <<"invoice">> => [<<"invoiceID">>],
            <<"payment">> => [<<"paymentID">>]
        }]
    }]
}.

-spec invoice() -> schema().

invoice() -> #{
    <<"shop_id">>   => [<<"shopID">>],
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"product">>   => [<<"product">>],
    <<"cart">>      => [<<"cart">>, [cart_line_schema()]]
}.

-spec refund() -> schema().

refund() -> #{
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"cart">>      => [<<"cart">>, [cart_line_schema()]]
}.

-spec cart_line_schema() -> schema().

cart_line_schema() ->
    #{
        <<"product">>  => [<<"product">>],
        <<"quantity">> => [<<"quantity">>],
        <<"price">>    => [<<"price">>],
        <<"tax">>      => [<<"taxMode">>, #{
            <<"type">> => [<<"type">>],
            <<"rate">> => [<<"rate">>]
        }]
    }.
