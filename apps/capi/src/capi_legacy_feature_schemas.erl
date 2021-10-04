-module(capi_legacy_feature_schemas).

-type schema() :: capi_legacy_feature_schemas:schema().

-include("capi_legacy_feature_schemas.hrl").

-define(id, 1).
-define(invoice_id, 2).
-define(make_recurrent, 3).
-define(flow, 4).
-define(hold_exp, 5).
-define(payer, 6).
-define(payment_tool, 7).
-define(token, 8).
-define(bank_card, 9).
-define(exp_date, 10).
-define(terminal, 11).
-define(terminal_type, 12).
-define(wallet, 13).
-define(provider, 14).
-define(crypto, 15).
-define(currency, 16).
-define(mobile_commerce, 17).
-define(operator, 18).
-define(phone, 19).
-define(customer, 20).
-define(recurrent, 21).
-define(invoice, 22).
-define(payment, 23).
-define(shop_id, 24).
-define(amount, 25).
-define(product, 26).
-define(due_date, 27).
-define(cart, 28).
-define(quantity, 29).
-define(price, 30).
-define(tax, 31).
-define(rate, 32).
-define(bank_account, 33).
-define(account, 34).
-define(bank_bik, 35).
-define(payment_resource, 36).
-define(payment_session, 37).
-define(lifetime, 38).
-define(details, 39).
-define(days, 40).
-define(months, 41).
-define(years, 42).
-define(single_line, 43).
-define(multiline, 44).
-define(range, 45).
-define(fixed, 46).
-define(lower_bound, 47).
-define(upper_bound, 48).
-define(invoice_template_id, 49).
-define(contact_info, 50).
-define(email, 51).
-define(phone_number, 52).
-define(allocation, 53).
-define(target, 54).
-define(total, 55).
-define(fee, 56).
-define(share, 57).
-define(matisse, 58).
-define(exponent, 59).

-export([payment/0]).
-export([invoice/0]).
-export([invoice_template/0]).
-export([refund/0]).
-export([customer_binding/0]).
-export([customer/0]).

-spec payment() -> schema().
payment() ->
    #{
        ?invoice_id => [<<"invoiceID">>],
        ?make_recurrent => [<<"makeRecurrent">>],
        ?flow => [
            <<"flow">>,
            #{
                ?discriminator => [<<"type">>],
                ?hold_exp => [<<"onHoldExpiration">>]
            }
        ],
        ?payer => [
            <<"payer">>,
            #{
                ?discriminator => [<<"payerType">>],
                ?payment_tool => [<<"paymentTool">>, payment_tool_schema()],
                ?customer => [<<"customerID">>],
                ?recurrent => [
                    <<"recurrentParentPayment">>,
                    #{
                        ?invoice => [<<"invoiceID">>],
                        ?payment => [<<"paymentID">>]
                    }
                ]
            }
        ]
    }.

-spec invoice() -> schema().
invoice() ->
    #{
        ?shop_id => [<<"shopID">>],
        ?amount => [<<"amount">>],
        ?currency => [<<"currency">>],
        ?product => [<<"product">>],
        ?due_date => [<<"dueDate">>],
        ?cart => [<<"cart">>, {set, cart_line_schema()}],
        ?bank_account => [<<"bankAccount">>, bank_account_schema()],
        ?invoice_template_id => [<<"invoiceTemplateID">>],
        ?allocation => [<<"allocation">>, {set, allocation_transaction()}]
    }.

-spec invoice_template() -> schema().
invoice_template() ->
    #{
        ?shop_id => [<<"shopID">>],
        ?lifetime => [<<"lifetime">>, lifetime_schema()],
        ?details => [<<"details">>, invoice_template_details_schema()]
    }.

-spec invoice_template_details_schema() -> schema().
invoice_template_details_schema() ->
    #{
        ?discriminator => [<<"templateType">>],
        ?single_line => #{
            ?product => [<<"product">>],
            ?price => [<<"price">>, invoice_template_line_cost()],
            ?tax => [<<"taxMode">>, tax_mode_schema()]
        },
        ?multiline => #{
            ?currency => [<<"currency">>],
            ?cart => [<<"cart">>, {set, cart_line_schema()}]
        }
    }.

-spec refund() -> schema().
refund() ->
    #{
        ?amount => [<<"amount">>],
        ?currency => [<<"currency">>],
        ?cart => [<<"cart">>, {set, cart_line_schema()}],
        ?allocation => [<<"allocation">>, {set, allocation_transaction()}]
    }.

-spec customer() -> schema().
customer() ->
    #{
        ?shop_id => [<<"shopID">>],
        ?contact_info => [<<"contactInfo">>, contact_info_schema()]
    }.

-spec customer_binding() -> schema().
customer_binding() ->
    #{
        ?payment_resource => [
            <<"paymentResource">>,
            #{
                ?payment_session => [<<"paymentSession">>],
                ?payment_tool => [<<"paymentTool">>, payment_tool_schema()]
            }
        ]
    }.

-spec payment_tool_schema() -> schema().
payment_tool_schema() ->
    #{
        ?discriminator => [<<"type">>],
        ?bank_card => #{
            ?token => [<<"token">>],
            ?exp_date => [<<"exp_date">>]
        },
        ?terminal => #{
            ?discriminator => [<<"terminal_type">>]
        },
        ?wallet => #{
            ?provider => [<<"provider">>],
            ?id => [<<"id">>],
            ?token => [<<"token">>]
        },
        ?crypto => #{
            ?currency => [<<"currency">>]
        },
        ?mobile_commerce => #{
            ?operator => [<<"operator">>],
            ?phone => [<<"phone">>]
        }
    }.

-spec allocation_transaction() -> schema().
allocation_transaction() ->
    #{
        ?target => [<<"target">>, allocation_target()],
        ?discriminator => [<<"allocationBodyType">>],
        ?amount => [<<"amount">>],
        ?total => [<<"total">>],
        ?currency => [<<"currency">>],
        ?fee => [
            <<"fee">>,
            #{
                ?target => [<<"target">>, allocation_target()],
                ?discriminator => [<<"allocationFeeType">>],
                ?amount => [<<"amount">>],
                ?share => [<<"share">>, decimal()]
            }
        ],
        ?cart => [<<"cart">>, {set, cart_line_schema()}]
    }.

-spec allocation_target() -> schema().
allocation_target() ->
    #{
        ?discriminator => [<<"allocationTargetType">>],
        ?shop_id => [<<"shopID">>]
    }.

-spec decimal() -> schema().
decimal() ->
    #{
        ?matisse => [<<"m">>],
        ?exponent => [<<"exp">>]
    }.

-spec cart_line_schema() -> schema().
cart_line_schema() ->
    #{
        ?product => [<<"product">>],
        ?quantity => [<<"quantity">>],
        ?price => [<<"price">>],
        ?tax => [<<"taxMode">>, tax_mode_schema()]
    }.

-spec tax_mode_schema() -> schema().
tax_mode_schema() ->
    #{
        ?discriminator => [<<"type">>],
        ?rate => [<<"rate">>]
    }.

-spec bank_account_schema() -> schema().
bank_account_schema() ->
    #{
        ?discriminator => [<<"accountType">>],
        ?account => [<<"account">>],
        ?bank_bik => [<<"bankBik">>]
    }.

invoice_template_line_cost() ->
    #{
        ?discriminator => [<<"costType">>],
        ?range => #{
            ?currency => [<<"currency">>],
            ?range => [<<"range">>, cost_amount_range()]
        },
        ?fixed => #{
            ?currency => [<<"currency">>],
            ?amount => [<<"amount">>]
        }
        %% Unlim has no params and is fully contained in discriminator
    }.

-spec cost_amount_range() -> schema().
cost_amount_range() ->
    #{
        ?upper_bound => [<<"upperBound">>],
        ?lower_bound => [<<"lowerBound">>]
    }.

-spec lifetime_schema() -> schema().
lifetime_schema() ->
    #{
        ?days => [<<"days">>],
        ?months => [<<"months">>],
        ?years => [<<"years">>]
    }.

-spec contact_info_schema() -> schema().
contact_info_schema() ->
    #{
        ?email => [<<"email">>],
        ?phone_number => [<<"phoneNumber">>]
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("capi_dummy_data.hrl").

deep_merge(M1, M2) ->
    maps:fold(
        fun
            (K, V, MAcc) when is_map(V) ->
                Value = deep_merge(maps:get(K, MAcc, #{}), V),
                MAcc#{K => Value};
            (K, V, MAcc) ->
                MAcc#{K => V}
        end,
        M1,
        M2
    ).

deep_fetch(Map, Keys) ->
    lists:foldl(fun(K, M) -> maps:get(K, M) end, Map, Keys).

hash(Term) ->
    capi_legacy_idemp_features:hash(Term).

read(Schema, Request) ->
    capi_legacy_idemp_features:read(Schema, Request).

compare(Features1, Features2) ->
    capi_legacy_idemp_features:compare(Features1, Features2).

list_diff_fields(Schema, Diff) ->
    capi_legacy_idemp_features:list_diff_fields(Schema, Diff).

-spec test() -> _.

-spec read_payment_features_test() -> _.

read_payment_features_test() ->
    PayerType = <<"PaymentResourcePayer">>,
    ToolType = <<"bank_card">>,
    Token = <<"cds token">>,
    CardHolder = <<"0x42">>,
    Category = <<"BUSINESS">>,
    ExpDate = {exp_date, 02, 2022},
    Flow = <<"PaymentFlowHold">>,
    Request = #{
        <<"flow">> => #{
            <<"type">> => Flow
        },
        <<"payer">> => #{
            <<"payerType">> => PayerType,
            <<"paymentTool">> => #{
                <<"type">> => ToolType,
                <<"token">> => Token,
                <<"exp_date">> => ExpDate,
                <<"cardholder_name">> => CardHolder,
                <<"category">> => Category
            }
        }
    },
    Payer = #{
        ?invoice_id => undefined,
        ?make_recurrent => undefined,
        ?flow => #{
            ?discriminator => hash(Flow),
            ?hold_exp => undefined
        },
        ?payer => #{
            ?discriminator => hash(PayerType),
            ?customer => undefined,
            ?recurrent => undefined,
            ?payment_tool => #{
                ?discriminator => hash(ToolType),
                ?bank_card => #{
                    ?exp_date => hash(ExpDate),
                    ?token => hash(Token)
                },
                ?crypto => #{?currency => undefined},
                ?mobile_commerce => #{
                    ?operator => undefined,
                    ?phone => undefined
                },
                ?terminal => #{?discriminator => undefined},
                ?wallet => #{
                    ?id => undefined,
                    ?provider => undefined,
                    ?token => hash(Token)
                }
            }
        }
    },
    Features = read(payment(), Request),
    ?assertEqual(Payer, Features).

-spec compare_payment_bank_card_test() -> _.
compare_payment_bank_card_test() ->
    Token2 = <<"cds token 2">>,
    CardHolder2 = <<"Cake">>,

    PaymentTool1 = bank_card(),
    PaymentTool2 = PaymentTool1#{
        <<"token">> => Token2,
        <<"cardholder_name">> => CardHolder2
    },
    Request1 = payment_params(PaymentTool1),
    Request2 = payment_params(PaymentTool2),

    common_compare_tests(payment(), Request1, Request2, [
        <<"payer.paymentTool.token">>
    ]).

-spec compare_different_payment_tool_test() -> _.
compare_different_payment_tool_test() ->
    ToolType2 = <<"wallet">>,
    Token2 = <<"wallet token">>,
    PaymentTool1 = bank_card(),
    PaymentTool2 = #{
        <<"type">> => ToolType2,
        <<"token">> => Token2
    },
    Request1 = payment_params(PaymentTool1),
    Request2 = payment_params(PaymentTool2),

    common_compare_tests(payment(), Request1, Request2, [<<"payer.paymentTool">>]).

-spec feature_multi_accessor_test() -> _.
feature_multi_accessor_test() ->
    Request1 = #{
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentTool">> => #{
                <<"wrapper">> => bank_card()
            }
        }
    },
    Request2 = deep_merge(Request1, #{
        <<"payer">> => #{
            <<"paymentTool">> => #{
                <<"wrapper">> => #{
                    <<"token">> => <<"cds token 2">>,
                    <<"cardholder_name">> => <<"Cake">>
                }
            }
        }
    }),
    Schema = #{
        <<"payer">> => [
            <<"payer">>,
            #{
                <<"type">> => [<<"payerType">>],
                <<"tool">> => [
                    <<"paymentTool">>,
                    <<"wrapper">>,
                    #{
                        <<"$type">> => [<<"type">>],
                        <<"bank_card">> => #{
                            <<"token">> => [<<"token">>],
                            <<"exp_date">> => [<<"exp_date">>]
                        }
                    }
                ]
            }
        ]
    },
    common_compare_tests(Schema, Request1, Request2, [
        <<"payer.paymentTool.wrapper.token">>
    ]).

-spec read_payment_customer_features_value_test() -> _.
read_payment_customer_features_value_test() ->
    PayerType = <<"CustomerPayer">>,
    CustomerID = <<"some customer id">>,
    Request = #{
        <<"payer">> => #{
            <<"payerType">> => PayerType,
            <<"customerID">> => CustomerID
        }
    },
    Features = read(payment(), Request),
    ?assertEqual(
        #{
            ?invoice_id => undefined,
            ?make_recurrent => undefined,
            ?flow => undefined,
            ?payer => #{
                ?discriminator => hash(PayerType),
                ?customer => hash(CustomerID),
                ?recurrent => undefined,
                ?payment_tool => undefined
            }
        },
        Features
    ).

-spec read_invoice_features_test() -> _.
read_invoice_features_test() ->
    ShopID = <<"shopus">>,
    Cur = <<"XXX">>,
    Prod1 = <<"yellow duck">>,
    Prod2 = <<"blue duck">>,
    DueDate = <<"2019-08-24T14:15:22Z">>,
    Price1 = 10000,
    Price2 = 20000,
    Quantity = 1,
    Product = #{
        ?product => hash(Prod1),
        ?quantity => hash(Quantity),
        ?price => hash(Price1),
        ?tax => undefined
    },
    Product2 = Product#{
        ?product => hash(Prod2),
        ?price => hash(Price2)
    },
    BankAccount = #{
        ?discriminator => hash(<<"InvoiceRussianBankAccount">>),
        ?account => hash(<<"12345678901234567890">>),
        ?bank_bik => hash(<<"123456789">>)
    },
    Invoice = #{
        ?amount => undefined,
        ?currency => hash(Cur),
        ?shop_id => hash(ShopID),
        ?product => undefined,
        ?due_date => hash(DueDate),
        ?bank_account => BankAccount,
        ?cart => [
            [1, Product],
            [0, Product2]
        ],
        ?invoice_template_id => undefined,
        ?allocation => undefined
    },
    Request = #{
        <<"externalID">> => <<"externalID">>,
        <<"dueDate">> => DueDate,
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"description">> => <<"Wild birds.">>,
        <<"bankAccount">> => #{
            <<"accountType">> => <<"InvoiceRussianBankAccount">>,
            <<"account">> => <<"12345678901234567890">>,
            <<"bankBik">> => <<"123456789">>
        },
        <<"cart">> => [
            #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2},
            #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1, <<"not feature">> => <<"hmm">>}
        ],
        <<"metadata">> => #{}
    },

    Features = read(invoice(), Request),
    ?assertEqual(Invoice, Features),

    TemplateID = <<"42">>,
    RequestWithTemplate = Request#{<<"invoiceTemplateID">> => TemplateID},
    FeaturesWithTemplate = read(invoice(), RequestWithTemplate),
    ?assertEqual(hash(TemplateID), maps:get(?invoice_template_id, FeaturesWithTemplate)).

-spec compare_invoices_features_test() -> _.
compare_invoices_features_test() ->
    ShopID = <<"shopus">>,
    Cur = <<"RUB">>,
    Prod1 = <<"yellow duck">>,
    Prod2 = <<"blue duck">>,
    Price1 = 10000,
    Price2 = 20000,
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
    Schema = invoice(),
    Invoice1 = read(Schema, Request1),
    InvoiceChg1 = read(Schema, Request1#{
        <<"cart">> => [
            Product#{
                <<"price">> => Price2,
                <<"taxMode">> => #{
                    <<"rate">> => <<"18%">>
                }
            }
        ]
    }),
    Invoice2 = read(Schema, Request2),
    InvoiceWithFullCart = read(Schema, Request3),
    ?assertEqual(
        {false, #{
            ?cart => #{
                0 => #{
                    ?price => ?difference,
                    ?product => ?difference,
                    ?quantity => ?difference,
                    ?tax => ?difference
                }
            }
        }},
        compare(Invoice2, Invoice1)
    ),
    ?assert(compare(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(compare(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(compare(Invoice2, InvoiceWithFullCart)),
    %% When second request didn't contain feature, this situation detected as conflict.
    ?assertEqual(
        {false, #{?cart => ?difference}},
        compare(Invoice1#{?cart => undefined}, Invoice1)
    ),

    {false, Diff} = compare(Invoice1, InvoiceChg1),
    ?assertEqual(
        [<<"cart.0.price">>, <<"cart.0.taxMode.rate">>],
        list_diff_fields(Schema, Diff)
    ),
    ?assert(compare(Invoice1, Invoice1#{?cart => undefined})).

-spec read_customer_features_test() -> _.
read_customer_features_test() ->
    Request = ?CUSTOMER_PARAMS,
    Features = #{
        ?shop_id => hash(?STRING),
        ?contact_info => #{
            ?email => hash(<<"bla@bla.ru">>),
            ?phone_number => undefined
        }
    },
    ?assertEqual(
        Features,
        read(customer(), Request)
    ).

-spec compare_customer_features_test() -> _.
compare_customer_features_test() ->
    Request = ?CUSTOMER_PARAMS,
    RequestSame = Request#{
        <<"partyID">> => <<"ANOTHER PARTY">>,

        <<"metadata">> => #{<<"text">> => <<"sample text">>}
    },
    RequestDifferent = Request#{
        <<"shopID">> => hash(<<"Another shop">>),
        <<"contactInfo">> => #{
            <<"email">> => hash(<<"bla@example.com">>),
            <<"phoneNumber">> => <<"8-800-555-35-35">>
        }
    },
    common_compare_tests(
        customer(),
        Request,
        RequestSame,
        RequestDifferent,
        [
            <<"shopID">>,
            <<"contactInfo.email">>,
            <<"contactInfo.phoneNumber">>
        ]
    ).

-spec read_customer_binding_features_test() -> _.
read_customer_binding_features_test() ->
    Session = ?TEST_PAYMENT_SESSION(<<"Session">>),
    Tool = ?TEST_PAYMENT_TOOL(visa, <<"TOKEN">>),
    Request = payment_resource(Session, Tool),
    Features = #{
        ?payment_resource => #{
            ?payment_session => hash(Session),
            ?payment_tool => #{
                ?discriminator => hash(<<"bank_card">>),
                ?bank_card => #{
                    ?token => hash(<<"TOKEN">>),
                    ?exp_date => hash(<<"12/2012">>)
                },

                ?terminal => #{
                    ?discriminator => undefined
                },
                ?wallet => #{
                    ?provider => undefined,
                    ?id => undefined,
                    ?token => hash(<<"TOKEN">>)
                },
                ?crypto => #{
                    ?currency => undefined
                },
                ?mobile_commerce => #{
                    ?operator => undefined,
                    ?phone => undefined
                }
            }
        }
    },

    ?assertEqual(
        Features,
        read(customer_binding(), Request)
    ).

-spec compare_customer_binding_features_test() -> _.
compare_customer_binding_features_test() ->
    Session1 = ?TEST_PAYMENT_SESSION(<<"Session1">>),
    Tool1 = ?TEST_PAYMENT_TOOL(visa),
    Request1 = payment_resource(Session1, Tool1),

    Session2 = ?TEST_PAYMENT_SESSION(<<"Session2">>),
    Tool2 = ?TEST_PAYMENT_TOOL(mastercard)#{<<"exp_date">> => <<"01/2020">>},
    Request2 = payment_resource(Session2, Tool2),

    common_compare_tests(customer_binding(), Request1, Request2, [
        <<"paymentResource.paymentTool.exp_date">>,
        <<"paymentResource.paymentSession">>
    ]).

%% Add invoice_template tests

-spec read_invoice_template_features_test() -> _.
read_invoice_template_features_test() ->
    ShopID = <<"1">>,
    Request = #{
        <<"shopID">> => ShopID,
        <<"lifetime">> => lifetime_dummy(1, 2, 3),
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS(42)
    },
    Features = #{
        ?shop_id => hash(ShopID),
        ?lifetime => #{
            ?days => hash(1),
            ?months => hash(2),
            ?years => hash(3)
        },
        ?details => #{
            ?discriminator => hash(<<"InvoiceTemplateMultiLine">>),
            ?single_line => #{
                ?product => undefined,
                ?price => undefined,
                ?tax => undefined
            },
            ?multiline => #{
                ?currency => hash(<<"RUB">>),
                ?cart => [
                    [
                        1,
                        #{
                            ?product => hash(?STRING),
                            ?quantity => hash(42),
                            ?price => hash(?INTEGER),
                            ?tax => #{?discriminator => hash(<<"InvoiceLineTaxVAT">>), ?rate => hash(<<"18%">>)}
                        }
                    ],
                    [
                        0,
                        #{
                            ?product => hash(?STRING),
                            ?quantity => hash(42),
                            ?price => hash(?INTEGER),
                            ?tax => undefined
                        }
                    ]
                ]
            }
        }
    },

    ?assertEqual(
        Features,
        read(invoice_template(), Request)
    ).

-spec compare_invoice_template_features_test() -> _.
compare_invoice_template_features_test() ->
    ShopID1 = <<"1">>,
    ShopID2 = <<"2">>,
    Request1 = #{
        <<"shopID">> => ShopID1,
        <<"lifetime">> => lifetime_dummy(1, 2, 3),
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS(42)
    },
    Request2 = deep_merge(
        Request1,
        #{
            <<"shopID">> => ShopID2,
            <<"lifetime">> => lifetime_dummy(1, 2, 42),
            <<"details">> => #{
                <<"currency">> => ?USD,
                <<"cart">> => [hd(deep_fetch(Request1, [<<"details">>, <<"cart">>]))]
            }
        }
    ),

    common_compare_tests(invoice_template(), Request1, Request2, [
        <<"shopID">>,
        <<"lifetime.years">>,
        <<"details.currency">>,
        <<"details.cart">>
    ]).

-spec read_allocation_transaction_test_() -> _.
read_allocation_transaction_test_() ->
    Request1 = ?ALLOCATION_TRANSACTION_PARAMS,
    Features1 = #{
        ?target => #{
            ?discriminator => hash(<<"AllocationTargetShop">>),
            ?shop_id => hash(?STRING)
        },
        ?discriminator => hash(<<"AllocationBodyTotal">>),
        ?amount => undefined,
        ?total => hash(?INTEGER),
        ?currency => hash(?USD),
        ?fee => #{
            ?target => #{
                ?discriminator => hash(<<"AllocationTargetShop">>),
                ?shop_id => hash(?STRING)
            },
            ?discriminator => hash(<<"AllocationFeeShare">>),
            ?amount => hash(?INTEGER),
            ?share => #{
                ?matisse => hash(?INTEGER),
                ?exponent => hash(?INTEGER)
            }
        },
        ?cart => [
            [
                0,
                #{
                    ?product => hash(?STRING),
                    ?quantity => hash(?INTEGER),
                    ?price => hash(?INTEGER),
                    ?tax => undefined
                }
            ]
        ]
    },
    Request2 = Request1#{
        <<"fee">> => #{
            <<"target">> => ?ALLOCATION_TARGET,
            <<"allocationFeeType">> => <<"AllocationFeeFixed">>,
            <<"amount">> => 1024
        }
    },
    Features2 = Features1#{
        ?fee => #{
            ?target => #{
                ?discriminator => hash(<<"AllocationTargetShop">>),
                ?shop_id => hash(?STRING)
            },
            ?discriminator => hash(<<"AllocationFeeFixed">>),
            ?amount => hash(1024),
            ?share => undefined
        }
    },
    [
        ?_assertEqual(Features1, read(allocation_transaction(), Request1)),
        ?_assertEqual(Features2, read(allocation_transaction(), Request2))
    ].

-spec compare_allocation_transaction_test() -> _.
compare_allocation_transaction_test() ->
    Request1 = ?ALLOCATION_TRANSACTION_PARAMS,
    Request2 = ?ALLOCATION_TRANSACTION_PARAMS#{
        <<"total">> => 1024,
        <<"amount">> => 512,
        <<"fee">> => #{
            <<"target">> => ?ALLOCATION_TARGET,
            <<"allocationFeeType">> => <<"AllocationFeeFixed">>,
            <<"amount">> => ?INTEGER,
            <<"share">> => undefined
        }
    },
    Request3 = #{
        <<"target">> => ?ALLOCATION_TARGET#{<<"shopID">> => <<"SomeShop">>},
        <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"cart">> => [
            #{<<"product">> => ?STRING, <<"quantity">> => 1, <<"price">> => ?INTEGER}
        ]
    },
    Request4 = Request1#{
        <<"fee">> => deep_merge(maps:get(<<"fee">>, Request1), #{
            <<"amount">> => 1024,
            <<"share">> => #{<<"m">> => 1024, <<"exp">> => 1024}
        })
    },
    common_compare_tests(allocation_transaction(), Request1, Request2, [
        <<"amount">>, <<"total">>, <<"fee">>
    ]),
    common_compare_tests(allocation_transaction(), Request1, Request3, [
        <<"target.shopID">>, <<"allocationBodyType">>, <<"currency">>, <<"amount">>, <<"cart.0.quantity">>
    ]),
    common_compare_tests(allocation_transaction(), Request1, Request4, [
        <<"fee.amount">>, <<"fee.share.m">>, <<"fee.share.exp">>
    ]).

-spec demo_compare_allocation_transaction_test() -> _.
demo_compare_allocation_transaction_test() ->
    Request1 = ?ALLOCATION_TRANSACTION_PARAMS,
    Request2 = #{
        <<"allocationBodyType">> => <<"AllocationBodyAmount">>
    },
    Request3 = #{
        <<"fee">> => deep_merge(maps:get(<<"fee">>, Request1), #{
            <<"allocationFeeType">> => <<"AllocationFeeFixed">>
        })
    },
    common_compare_tests(allocation_transaction(), Request1, Request2, [
        <<"allocationBodyType">>
    ]),
    common_compare_tests(allocation_transaction(), Request1, Request3, [
        <<"fee">>
    ]).

payment_resource(Session, Tool) ->
    #{
        <<"paymentResource">> => #{
            <<"paymentSession">> => Session,
            <<"paymentTool">> => Tool
        }
    }.

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
        <<"type">> => <<"bank_card">>,
        <<"token">> => <<"cds token">>,
        <<"payment_system">> => <<"visa">>,
        <<"bin">> => <<"411111">>,
        <<"last_digits">> => <<"1111">>,
        <<"exp_date">> => <<"2019-08-24T14:15:22Z">>,
        <<"cardholder_name">> => <<"Degus Degusovich">>,
        <<"is_cvv_empty">> => false
    }.

lifetime_dummy(Days, Months, Years) ->
    #{
        <<"days">> => Days,
        <<"months">> => Months,
        <<"years">> => Years
    }.

common_compare_tests(Schema, Request, RequestDifferent, DiffFeatures) ->
    common_compare_tests(Schema, Request, Request, RequestDifferent, DiffFeatures).

common_compare_tests(Schema, Request, RequestWithIgnoredFields, RequestDifferent, DiffFeatures) ->
    Features = read(Schema, Request),
    FeaturesIgnored = read(Schema, RequestWithIgnoredFields),
    FeaturesDifferent = read(Schema, RequestDifferent),

    %% Equal to self
    ?assertEqual(true, compare(Features, Features)),
    %% Equal to feature-wise same request
    ?assertEqual(true, compare(Features, FeaturesIgnored)),

    %% Has correct diff with different request
    Result = compare(Features, FeaturesDifferent),
    ?assertMatch({false, _}, Result),

    {false, Diff} = Result,
    ?assertEqual(lists:sort(DiffFeatures), lists:sort(list_diff_fields(Schema, Diff))).

-endif.
