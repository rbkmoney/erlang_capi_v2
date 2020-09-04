-module(capi_feature_schemas).

-type schema() :: capi_idemp_features:schema().

-include("capi_feature_schemas.hrl").

-export([payment/0]).
-export([invoice/0]).
-export([refund/0]).

-spec payment() -> schema().

payment() -> #{
    ?invoice_id => [<<"invoiceID">>],
    ?make_recurrent => [<<"makeRecurrent">>],
    ?flow => [<<"flow">>, #{
        ?type => [<<"type">>],
        ?hold_exp => [<<"onHoldExpiration">>]
    }],
    ?payer => [<<"payer">>, #{
        ?type => [<<"payerType">>],
        ?tool => [<<"paymentTool">>, #{
            ?_type_ => [<<"type">>],
            ?bank_card => #{
                ?token   => [<<"token">>],
                ?expdate => [<<"exp_date">>]
            },
            ?terminal => #{
                ?terminal_type => [<<"terminal_type">>]
            },
            ?wallet => #{
                ?provider => [<<"provider">>],
                ?id       => [<<"id">>],
                ?token    => [<<"token">>]
            },
            ?crypto => #{
                ?currency => [<<"currency">>]
            },
            ?mobile_commerce => #{
                ?operator => [<<"operator">>],
                ?phone    => [<<"phone">>]
            }
        }],
        ?customer => [<<"customerID">>],
        ?recurrent => [<<"recurrentParentPayment">>, #{
            ?invoice => [<<"invoiceID">>],
            ?payment => [<<"paymentID">>]
        }]
    }]
}.

-spec invoice() -> schema().

invoice() -> #{
    ?shop_id     => [<<"shopID">>],
    ?amount      => [<<"amount">>],
    ?currency    => [<<"currency">>],
    ?product     => [<<"product">>],
    ?due_date    => [<<"dueDate">>],
    ?cart        => [<<"cart">>, {set, cart_line_schema()}]
}.

-spec refund() -> schema().

refund() -> #{
    ?amount      => [<<"amount">>],
    ?currency    => [<<"currency">>],
    ?cart        => [<<"cart">>, {set, cart_line_schema()}]
}.

-spec cart_line_schema() -> schema().

cart_line_schema() ->
    #{
        ?product  => [<<"product">>],
        ?quantity => [<<"quantity">>],
        ?price    => [<<"price">>],
        ?tax      => [<<"taxMode">>, #{
            ?type =>[<<"type">>],
            ?rate => [<<"rate">>]
        }]
    }.
