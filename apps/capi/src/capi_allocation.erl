-module(capi_allocation).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([validate/1]).
-export([encode/2]).
-export([decode/1]).

-type allocation() :: dmsl_domain_thrift:'Allocation'().
-type encode_data() :: _.
-type decode_data() :: _.
-type validate_error() :: duplicate_allocation.

-spec validate(map()) -> ok | validate_error().
validate(undefined) ->
    ok;
validate(#{<<"items">> := Transactions}) ->
    Uniq = lists:usort([maps:get(<<"shopID">>, Target) || #{<<"target">> := Target} <- Transactions]),
    Duplictate = erlang:length(Uniq) =/= erlang:length(Transactions),
    if
        Duplictate -> duplicate_allocation;
        true -> ok
    end.

-spec encode(map() | undefined, binary()) -> encode_data() | undefined.
encode(undefined, _PartyID) ->
    undefined;
encode(Transactions, PartyID) ->
    #domain_AllocationPrototype{
        transactions = lists:map(
            fun(T) ->
                Transaction = encode_transaction(T),
                {shop, Target} = Transaction#domain_AllocationTransactionPrototype.target,
                NewTarget = Target#domain_AllocationTransactionTargetShop{owner_id = PartyID},
                Transaction#domain_AllocationTransactionPrototype{
                    target = {shop, NewTarget}
                }
            end,
            Transactions
        )
    }.

encode_transaction(Transaction) ->
    #domain_AllocationTransactionPrototype{
        id = <<"wtf">>,
        target = encode_target(maps:get(<<"target">>, Transaction)),
        body = encode_body(Transaction),
        details = encode_details(Transaction)
    }.

encode_target(#{<<"allocationTargetType">> := <<"AllocationTargetShop">>} = Target) ->
    {shop, #domain_AllocationTransactionTargetShop{
        owner_id = <<"wtf">>,
        shop_id = maps:get(<<"shopID">>, Target)
    }}.

encode_details(#{<<"cart">> := _Cart} = Transaction) ->
    #domain_AllocationTransactionDetails{
        cart = capi_handler_encoder:encode_invoice_cart(Transaction)
    };
encode_details(_) ->
    undefined.

encode_body(#{<<"allocationBodyType">> := <<"AllocationBodyAmount">>} = Transaction) ->
    {amount, #domain_AllocationTransactionPrototypeBodyAmount{
        amount = capi_handler_encoder:encode_cash(Transaction)
    }};
encode_body(#{<<"allocationBodyType">> := <<"AllocationBodyTotal">>} = Transaction) ->
    Currency = maps:get(<<"currency">>, Transaction),
    Total = maps:get(<<"total">>, Transaction),
    % в swag помечен как необязательный
    Fee = maps:get(<<"fee">>, Transaction),
    {total, #domain_AllocationTransactionPrototypeBodyTotal{
        total = capi_handler_encoder:encode_cash(Total, Currency),
        fee = encode_fee(Fee, Currency)
    }}.

encode_fee(#{<<"allocationFeeType">> := <<"AllocationFeeFixed">>} = Fee, Currency) ->
    Amount = maps:get(<<"amount">>, Fee),
    {fixed, #domain_AllocationTransactionPrototypeFeeFixed{
        amount = capi_handler_encoder:encode_cash(Amount, Currency)
    }};
encode_fee(#{<<"allocationFeeType">> := <<"AllocationFeeShare">>} = Fee, _Currency) ->
    Share = maps:get(<<"share">>, Fee),
    {share, #domain_AllocationTransactionPrototypeFeeShare{
        parts = encode_parts(Share)
    }}.

encode_parts(#{<<"m">> := M, <<"exp">> := Exp}) ->
    case Exp < 0 of
        true ->
            Q = erlang:trunc(math:pow(10, -Exp)),
            #'Rational'{p = M, q = Q};
        _ ->
            P = M * erlang:trunc(math:pow(10, Exp)),
            #'Rational'{p = P, q = 1}
    end.

-spec decode(allocation() | undefined) -> decode_data() | undefined.
decode(undefined) ->
    undefined;
decode(#domain_Allocation{transactions = Transactions}) ->
    [decode_transaction(L) || L <- Transactions].

decode_transaction(Transaction) ->
    Amount = Transaction#domain_AllocationTransaction.amount,
    Map0 = capi_handler_utils:merge_and_compact(
        #{
            <<"target">> => decode_target(Transaction#domain_AllocationTransaction.target)
        },
        decode_body(Transaction#domain_AllocationTransaction.body, Amount)
    ),
    capi_handler_utils:merge_and_compact(
        Map0,
        decode_details(Transaction#domain_AllocationTransaction.details)
    ).

decode_target({shop, AllocationShop}) ->
    #{
        <<"allocationTargetType">> => <<"AllocationTargetShop">>,
        <<"shopID">> => AllocationShop#domain_AllocationTransactionTargetShop.shop_id
    }.

decode_details(undefined) ->
    undefined;
decode_details(AllocationDetails) ->
    #{
        <<"cart">> => capi_handler_decoder_invoicing:decode_invoice_cart(
            AllocationDetails#domain_AllocationTransactionDetails.cart
        )
    }.

decode_body(undefined, TransactionAmount) ->
    #{
        <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
        <<"amount">> => TransactionAmount#domain_Cash.amount,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(TransactionAmount#domain_Cash.currency)
    };
decode_body(Body, TransactionAmount) ->
    TotalAmount = Body#domain_AllocationTransactionBodyTotal.total,
    FeeAmount = Body#domain_AllocationTransactionBodyTotal.fee_amount,
    FeeTarget = Body#domain_AllocationTransactionBodyTotal.fee_target,
    Fee = Body#domain_AllocationTransactionBodyTotal.fee,
    #{
        <<"allocationBodyType">> => <<"AllocationBodyTotal">>,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(TotalAmount#domain_Cash.currency),
        <<"total">> => TotalAmount#domain_Cash.amount,
        <<"amount">> => TransactionAmount#domain_Cash.amount,
        <<"fee">> => decode_fee(Fee, FeeTarget, FeeAmount)
    }.

decode_fee(undefined, FeeTarget, FeeAmount) ->
    #{
        <<"allocationFeeType">> => <<"AllocationFeeFixed">>,
        <<"target">> => decode_target(FeeTarget),
        <<"amount">> => FeeAmount#domain_Cash.amount
    };
decode_fee(Fee, FeeTarget, FeeAmount) ->
    #{
        <<"allocationFeeType">> => <<"AllocationFeeShare">>,
        <<"target">> => decode_target(FeeTarget),
        <<"amount">> => FeeAmount#domain_Cash.amount,
        <<"share">> => decode_parts(Fee#domain_AllocationTransactionFeeShare.parts)
    }.

decode_parts(Parts) ->
    #'Rational'{p = P, q = Q} = Parts,
    Exponent = erlang:trunc(math:log10(Q)),
    #{
        <<"m">> => P,
        <<"exp">> => -Exponent
    }.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec validate_test() -> _.
validate_test() ->
    ?assertEqual(
        duplicate_allocation,
        validate(#{
            <<"items">> => [
                #{<<"target">> => #{<<"shopID">> => <<"shop1">>}},
                #{<<"target">> => #{<<"shopID">> => <<"shop2">>}},
                #{<<"target">> => #{<<"shopID">> => <<"shop1">>}}
            ]
        })
    ).

-spec encode_parts_test_() -> _.
encode_parts_test_() ->
    [
        ?_assertEqual(make_rational(8, 1), encode_parts(make_decimal(8, 0))),
        ?_assertEqual(make_rational(10, 10), encode_parts(make_decimal(10, -1))),
        ?_assertEqual(make_rational(11000700, 100000000), encode_parts(make_decimal(11000700, -8)))
    ].

-spec encode_test() -> _.
encode_test() ->
    AllocationCart = [
        #{
            <<"product">> => <<"info">>,
            <<"quantity">> => 2,
            <<"price">> => 16
        }
    ],
    Allocation = [
        #{
            <<"target">> => #{
                <<"allocationTargetType">> => <<"AllocationTargetShop">>,
                <<"shopID">> => <<"shopID1">>
            },
            <<"allocationBodyType">> => <<"AllocationBodyTotal">>,
            <<"total">> => 32,
            <<"currency">> => <<"RUB">>,
            <<"fee">> => #{
                <<"target">> => #{
                    <<"allocationTargetType">> => <<"AllocationTargetShop">>,
                    <<"shopID">> => <<"shopID2">>
                },
                <<"allocationFeeType">> => <<"AllocationFeeShare">>,
                <<"amount">> => 24,
                <<"share">> => make_decimal(80, -1)
            },
            <<"cart">> => AllocationCart
        }
    ],
    Expected = #domain_AllocationPrototype{
        transactions = [
            #domain_AllocationTransactionPrototype{
                id = <<"wtf">>,
                target =
                    {shop, #domain_AllocationTransactionTargetShop{
                        owner_id = <<"partyID">>,
                        shop_id = <<"shopID1">>
                    }},
                body =
                    {total, #domain_AllocationTransactionPrototypeBodyTotal{
                        total = make_cash(32),
                        fee =
                            {share, #domain_AllocationTransactionPrototypeFeeShare{
                                parts = make_rational(80, 10)
                            }}
                    }},
                details = #domain_AllocationTransactionDetails{
                    cart = capi_handler_encoder:encode_invoice_cart(AllocationCart, <<"RUB">>)
                }
            }
        ]
    },
    Result = encode(Allocation, <<"partyID">>),
    ?assertEqual(Expected, Result).

-spec decode_parts_test_() -> _.
decode_parts_test_() ->
    [
        ?_assertEqual(make_decimal(8, 0), decode_parts(make_rational(8, 1))),
        ?_assertEqual(make_decimal(10, -1), decode_parts(make_rational(10, 10))),
        ?_assertEqual(make_decimal(11000700, -8), decode_parts(make_rational(11000700, 100000000)))
    ].

-spec decode_test() -> _.
decode_test() ->
    AllocationCart = #domain_InvoiceCart{
        lines = [
            #domain_InvoiceLine{
                product = <<"info">>,
                quantity = 2,
                price = make_cash(16)
            }
        ]
    },
    Allocation = #domain_Allocation{
        transactions = [
            #domain_AllocationTransaction{
                id = <<"transactionID">>,
                target =
                    {shop, #domain_AllocationTransactionTargetShop{
                        owner_id = <<"partyID1">>,
                        shop_id = <<"shopID1">>
                    }},
                amount = make_cash(32),
                body = #domain_AllocationTransactionBodyTotal{
                    fee_target =
                        {shop, #domain_AllocationTransactionTargetShop{
                            owner_id = <<"partyID2">>,
                            shop_id = <<"shopID2">>
                        }},
                    total = make_cash(16),
                    fee_amount = make_cash(8),
                    fee = #domain_AllocationTransactionFeeShare{
                        parts = make_rational(80, 10)
                    }
                },
                details = #domain_AllocationTransactionDetails{
                    cart = AllocationCart
                }
            }
        ]
    },
    Expected = [
        #{
            <<"target">> => #{
                <<"allocationTargetType">> => <<"AllocationTargetShop">>,
                <<"shopID">> => <<"shopID1">>
            },
            <<"allocationBodyType">> => <<"AllocationBodyTotal">>,
            <<"currency">> => <<"RUB">>,
            <<"total">> => 16,
            <<"amount">> => 32,
            <<"fee">> => #{
                <<"target">> => #{
                    <<"allocationTargetType">> => <<"AllocationTargetShop">>,
                    <<"shopID">> => <<"shopID2">>
                },
                <<"allocationFeeType">> => <<"AllocationFeeShare">>,
                <<"amount">> => 8,
                <<"share">> => decode_parts(make_rational(80, 10))
            },
            <<"cart">> => capi_handler_decoder_invoicing:decode_invoice_cart(AllocationCart)
        }
    ],
    Result = decode(Allocation),
    ?assertEqual(Expected, Result).

make_cash(Amount) -> #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}}.
make_decimal(M, E) -> #{<<"m">> => M, <<"exp">> => E}.
make_rational(P, Q) -> #'Rational'{p = P, q = Q}.

-endif.
