-module(capi_req_schemas).

-type schema_type()     :: payment | invoice | refund.
-type schema()          :: #{binary() := [accessor() | schema() | [schema()] | filter()]}.
-type accessor()        :: binary(). % name of field in a map
-type filter()          :: {filter, [accessor()], schema()}.

-export_type([schema_type/0]).
-export_type([schema/0]).

-export([get_schema/1]).
-export([payment_schema/0]).
-export([invoice_schema/0]).
-export([refund_schema/0]).

-spec get_schema(schema_type()) ->
    schema().

get_schema(payment) ->
    payment_schema();
get_schema(invoice) ->
    invoice_schema();
get_schema(refund) ->
    refund_schema().

-spec payment_schema() -> schema().

payment_schema() -> #{
    <<"payer">> => [<<"payer">>, #{
        <<"type">> => [<<"payerType">>],
        <<"tool">> => [<<"paymentTool">>, #{
            '$type' => [<<"type">>],
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

-spec invoice_schema() -> schema().

invoice_schema() -> #{
    <<"shop_id">>   => [<<"shopID">>],
    <<"amount">>    => [<<"amount">>],
    <<"currency">>  => [<<"currency">>],
    <<"product">>   => [<<"product">>],
    <<"cart">>      => [<<"cart">>, [cart_line_schema()]]
}.

-spec refund_schema() -> schema().

refund_schema() -> #{
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
