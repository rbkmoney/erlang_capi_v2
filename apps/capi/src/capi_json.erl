-module(capi_json).

-include_lib("cp_proto/include/cp_payment_processing_thrift.hrl").
-include_lib("cp_proto/include/cp_domain_thrift.hrl").
-include_lib("cp_proto/include/cp_cds_thrift.hrl").
-include_lib("cp_proto/include/cp_merch_stat_thrift.hrl").
-include_lib("cp_proto/include/cp_webhooker_thrift.hrl").
-include_lib("cp_proto/include/cp_user_interaction_thrift.hrl").
-include_lib("cp_proto/include/cp_geo_ip_thrift.hrl").

-export([decode/1]).
-export([encode/2]).


-define(invpaid()      , {paid, #webhooker_InvoicePaid{}}).
-define(invcancelled() , {cancelled, #webhooker_InvoiceCancelled{}}).
-define(invfulfilled() , {fulfilled, #webhooker_InvoiceFulfilled{}}).

-define(pmtprocessed() , {processed, #webhooker_InvoicePaymentProcessed{}}).
-define(pmtcaptured()  , {captured, #webhooker_InvoicePaymentCaptured{}}).
-define(pmtcancelled() , {cancelled, #webhooker_InvoicePaymentCancelled{}}).
-define(pmtfailed()    , {failed, #webhooker_InvoicePaymentFailed{}}).

%% Interface

-spec decode(any()) -> map() | [map()].

decode(X) ->
    case X of
        #domain_Party{} -> decode_party(X);
        #domain_Contract{} -> decode_contract(X);
        #domain_PayoutTool{} -> decode_payout_tool(X);
        #domain_ContractAdjustment{} -> decode_contract_adjustment(X);
        #domain_Shop{} -> decode_shop(X);
        #domain_CategoryObject{} -> decode_category(X);
        #payproc_AccountState{} -> decode_account_state(X);
        #payproc_Claim{} -> decode_claim(X);
        #domain_Invoice{} -> decode_invoice(X);
        {InvoiceID, #payproc_InvoicePayment{} = P} -> decode_invoice_payment(InvoiceID, P);
        #merchstat_StatInvoice{} -> decode_stat_invoice(X);
        #merchstat_StatPayment{} -> decode_stat_payment(X);
        #payproc_Event{} -> decode_event(X);
        #domain_BankCard{} -> decode_bank_card(X);
        #webhooker_Webhook{} -> decode_webhook(X);
        {StatType, #merchstat_StatResponse{data = {records, S}}} -> decode_stat_response(StatType, S);
        _ -> error({badarg, X})
    end.

-spec encode(atom(), map() | [map()] | binary()) -> any().

encode(Type, X) ->
    case Type of
        invoice_params  -> encode_invoice_params(X);
        bank_card       -> encode_bank_card(X);
        stat_request    -> encode_stat_request(X);
        claim_changeset -> encode_claim_changeset(X);
        reason          -> encode_reason(X);
        event_filter    -> encode_event_filter(X);
        webhook_id      -> encode_webhook_id(X);
        _ -> error({badarg, X})
    end.

%% Internal

decode_event(#payproc_Event{
    id = EventID,
    created_at = CreatedAt,
    payload =  {invoice_changes, InvoiceChanges},
    source =  {invoice_id, InvoiceID} %%@TODO deal with Party source
}) ->
    #{
        <<"id">> => EventID,
        <<"createdAt">> => CreatedAt,
        <<"changes">> => decode_invoice_changes(InvoiceID, InvoiceChanges)
    }.

decode_invoice_changes(InvoiceID, InvoiceChanges) when is_list(InvoiceChanges) ->
    lists:foldl(
        fun(Change, Acc) ->
            case decode_invoice_change(InvoiceID, Change) of
                #{} = Decoded ->
                    Acc ++ [Decoded];
                undefined ->
                    Acc
            end
        end,
        [],
        InvoiceChanges
    ).

decode_invoice_change(_, {
    invoice_created,
    #payproc_InvoiceCreated{invoice = Invoice}
}) ->
    #{
        <<"changeType">> => <<"InvoiceCreated">>,
        <<"invoice">> => decode_invoice(Invoice)
    };

decode_invoice_change(_, {
    invoice_status_changed,
    #payproc_InvoiceStatusChanged{status = {Status, _}}
}) ->
    #{
        <<"changeType">> => <<"InvoiceStatusChanged">>,
        <<"status">> => genlib:to_binary(Status)
    };

decode_invoice_change(
    InvoiceID,
    {invoice_payment_change, #payproc_InvoicePaymentChange{
        id = PaymentID,
        payload = Change
    }}
) ->
    decode_payment_change(InvoiceID, PaymentID, Change);

decode_invoice_change(_, _) ->
    undefined.

decode_payment_change(
    InvoiceID,
    _PaymentID,
    {invoice_payment_started, #payproc_InvoicePaymentStarted{
        payment = Payment
    }}
) ->
    #{
        <<"changeType">> => <<"PaymentStarted">>,
        <<"payment">> => decode_payment(InvoiceID, Payment)
    };

decode_payment_change(
    _InvoiceID,
    PaymentID,
    {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
        payload = {invoice_payment_session_interaction_requested,
            #payproc_InvoicePaymentSessionInteractionRequested{
                interaction = Interaction
            }
        }
    }}
) ->
    #{
        <<"changeType">> => <<"PaymentInteractionRequested">>,
        <<"paymentID">> => PaymentID,
        <<"userInteraction">> => decode_user_interaction(Interaction)
    };

decode_payment_change(
    _InvoiceID,
    PaymentID,
    {invoice_payment_status_changed, #payproc_InvoicePaymentStatusChanged{
        status = Status
    }}
) ->
    genlib_map:compact(maps:merge(
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID
        },
        decode_payment_status(Status)
    ));

decode_payment_change(_, _, _) ->
    undefined.

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{
    payment = Payment
}) ->
    decode_payment(InvoiceID, Payment).

decode_payment(InvoiceID, #domain_InvoicePayment{
    id = PaymentID,
    created_at = CreatedAt,
    status = Status,
    payer = #domain_Payer{
        payment_tool = PaymentTool,
        session_id = PaymentSession,
        contact_info = ContactInfo,
        client_info = #domain_ClientInfo{
            ip_address = IP,
            fingerprint = Fingerprint
        }
    },
    cost = #domain_Cash{
        amount = Amount,
        currency = Currency
    }
}) ->

    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"createdAt">> => CreatedAt,
        % TODO whoops, nothing to get it from yet
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"amount">> => Amount,
        <<"currency">> => decode_currency(Currency),
        <<"contactInfo">> => decode_contact_info(ContactInfo),
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => decode_payment_tool_token(PaymentTool),
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"ip">> => IP,
        <<"fingerprint">> => Fingerprint
    }, decode_payment_status(Status))).

decode_payment_tool_token({bank_card, BankCard}) ->
    decode_bank_card(BankCard).

decode_bank_card(#domain_BankCard{
    'token'  = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan
}) ->
    base64url:encode(jsx:encode(#{
        <<"token">> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin">> => Bin,
        <<"masked_pan">> => MaskedPan
    })).

decode_payment_tool_details({bank_card, #domain_BankCard{
    'payment_system' = PaymentSystem,
    'masked_pan' = MaskedPan
}}) ->
    #{
        <<"detailsType">> => <<"PaymentToolDetailsCardData">>,
        <<"cardNumberMask">> => decode_masked_pan(MaskedPan),
        <<"paymentSystem">> => genlib:to_binary(PaymentSystem)
    }.

-define(MASKED_PAN_MAX_LENGTH, 4).

decode_masked_pan(MaskedPan) ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH}).

decode_contact_info(#domain_ContactInfo{
    phone_number = PhoneNumber,
    email = Email
}) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => PhoneNumber,
        <<"email">> => Email
    }).

decode_payment_status({Status, StatusInfo}) ->
    Error = case StatusInfo of
        #domain_InvoicePaymentFailed{failure = OperationFailure} ->
            decode_operation_failure(OperationFailure);
        _ ->
            undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_operation_failure({operation_timeout, _}) ->
    decode_operation_failure(timeout, <<"timeout">>);

decode_operation_failure({external_failure, #domain_ExternalFailure{
    code = Code,
    description = Description
}}) ->
    decode_operation_failure(Code, Description).

decode_operation_failure(Code, Description) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Description)}.

decode_stat_payment(#merchstat_StatPayment{
    id = PaymentID,
    invoice_id = InvoiceID,
    shop_id = ShopID,
    created_at = CreatedAt,
    status = Status,
    amount = Amount,
    fee = Fee,
    currency_symbolic_code = Currency,
    payment_tool = PaymentTool,
    ip_address = IP,
    fingerprint = Fingerprint,
    phone_number = PhoneNumber,
    email = Email,
    session_id = PaymentSession,
    context = RawContext,
    location_info = Location
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> =>  PaymentID,
        <<"invoiceID">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"amount">> => Amount,
        <<"fee">> => Fee,
        <<"currency">> => Currency,
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email">> => Email
        }),
        <<"paymentSession">> => PaymentSession,
        <<"paymentToolToken">> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"ip">> => IP,
        <<"geoLocationInfo">> => decode_geo_location_info(Location),
        <<"fingerprint">> => Fingerprint,
        <<"metadata">> =>  decode_context(RawContext)
    }, decode_stat_payment_status(Status))).

decode_stat_payment_tool_token(PaymentTool) ->
    decode_payment_tool_token(merchstat_to_domain(PaymentTool)).

decode_stat_payment_tool_details(PaymentTool) ->
    decode_payment_tool_details(merchstat_to_domain(PaymentTool)).

decode_stat_payment_status(PaymentStatus) ->
    decode_payment_status(merchstat_to_domain(PaymentStatus)).

merchstat_to_domain({bank_card, #merchstat_BankCard{
    'token'  = Token,
    'payment_system' = PaymentSystem,
    'bin' = Bin,
    'masked_pan' = MaskedPan
}}) ->
    {bank_card, #domain_BankCard{
        'token'  = Token,
        'payment_system' = PaymentSystem,
        'bin' = Bin,
        'masked_pan' = MaskedPan
    }};

merchstat_to_domain({Status, #merchstat_InvoicePaymentPending{}}) ->
    {Status, #domain_InvoicePaymentPending{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentProcessed{}}) ->
    {Status, #domain_InvoicePaymentProcessed{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentCaptured{}}) ->
    {Status, #domain_InvoicePaymentCaptured{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentCancelled{}}) ->
    {Status, #domain_InvoicePaymentCancelled{}};

merchstat_to_domain({Status, #merchstat_InvoicePaymentFailed{
    failure = #merchstat_OperationFailure{
        code = Code,
        description = Description
    }
}}) ->
    {Status, #domain_InvoicePaymentFailed{
        failure = {external_failure, #domain_ExternalFailure{
            code = Code,
            description = Description
        }}
    }};

merchstat_to_domain({Status, #merchstat_InvoiceUnpaid{}}) ->
    {Status, #domain_InvoiceUnpaid{}};

merchstat_to_domain({Status, #merchstat_InvoicePaid{}}) ->
    {Status, #domain_InvoicePaid{}};

merchstat_to_domain({Status, #merchstat_InvoiceCancelled{details = Details}}) ->
    {Status, #domain_InvoiceCancelled{details = Details}};

merchstat_to_domain({Status, #merchstat_InvoiceFulfilled{details = Details}}) ->
    {Status, #domain_InvoiceFulfilled{details = Details}}.

decode_invoice(#domain_Invoice{
    id = InvoiceID,
    created_at = CreatedAt,
    status = InvoiceStatus,
    due  = DueDate,
    details = #domain_InvoiceDetails{
        product = Product,
        description = Description
    },
    cost = #domain_Cash{
        amount = Amount,
        currency = Currency
    },
    context = RawContext,
    shop_id = ShopID
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"dueDate">> => DueDate,
        <<"amount">> => Amount,
        <<"currency">> =>  decode_currency(Currency),
        <<"metadata">> =>  decode_context(RawContext),
        <<"product">> => Product,
        <<"description">> => Description
    }, decode_invoice_status(InvoiceStatus))).

decode_invoice_status({Status, StatusInfo}) ->
    Reason = case StatusInfo of
        #domain_InvoiceCancelled{details = Details} -> Details;
        #domain_InvoiceFulfilled{details = Details} -> Details;
        _ -> undefined
    end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"reason">> => Reason
    }.

decode_stat_invoice(#merchstat_StatInvoice{
    id = InvoiceID,
    shop_id = ShopID,
    created_at = CreatedAt,
    status = InvoiceStatus,
    product = Product,
    description = Description,
    due  = DueDate,
    amount = Amount,
    currency_symbolic_code = Currency,
    context = RawContext
}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">> => InvoiceID,
        <<"shopID">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"dueDate">> => DueDate,
        <<"amount">> => Amount,
        <<"currency">> =>  Currency,
        <<"metadata">> =>  decode_context(RawContext),
        <<"product">> => Product,
        <<"description">> => Description
    }, decode_stat_invoice_status(InvoiceStatus))).

decode_stat_invoice_status(Status) ->
    decode_invoice_status(merchstat_to_domain(Status)).

decode_context(#'Content'{
    type = <<"application/json">>,
    data = InvoiceContext
}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);

decode_context(undefined) ->
    undefined.

decode_party(#domain_Party{
    id = PartyID,
    blocking = Blocking,
    suspension = Suspension
}) ->
    #{
        <<"id">> => PartyID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension)
    }.

decode_contract(#domain_Contract{
    id = ContractID,
    created_at = CreatedAt,
    contractor = Contractor,
    valid_since = ValidSince,
    valid_until = ValidUntil,
    status = Status0,
    legal_agreement = LegalAgreement
}) ->
    Status = decode_contract_status(Status0),
    genlib_map:compact(maps:merge(#{
        <<"id">> => ContractID,
        <<"createdAt">> => CreatedAt,
        <<"contractor">> => decode_contractor(Contractor),
        <<"validSince">> => ValidSince,
        <<"validUntil">> => ValidUntil,
        <<"legalAgreement">> => decode_legal_agreement(LegalAgreement)
    }, Status)).

decode_contract_status({active, _}) ->
    #{
        <<"status">> => <<"active">>
    };

decode_contract_status({terminated, #domain_ContractTerminated{
    terminated_at = TerminatedAt
}}) ->
    #{
        <<"status">> => <<"terminated">>,
        <<"terminatedAt">> => TerminatedAt
    }.

decode_payout_tool(#domain_PayoutTool{
    id = ID,
    currency = Currency,
    payout_tool_info = Info
}) ->
    maps:merge(
        #{<<"id">> => ID},
        decode_payout_tool_params(Currency, Info)
    ).

decode_payout_tool_params(#payproc_PayoutToolParams{
    currency = Currency,
    tool_info = ToolInfo
}) ->
    decode_payout_tool_params(Currency, ToolInfo).

decode_payout_tool_params(Currency, Info) ->
    #{
        <<"currency">> => decode_currency(Currency),
        <<"details">> => decode_payout_tool_info(Info)
    }.

decode_payout_tool_info({bank_account, BankAccount}) ->
    #{
        <<"payoutToolType">> => <<"PayoutToolBankAccount">>,
        <<"bankAccount">> => decode_bank_account(BankAccount)
    }.

decode_bank_account(#domain_BankAccount{
    account = Account,
    bank_name = BankName,
    bank_post_account = BankPostAccount,
    bank_bik = BankBik
}) ->
    #{
        <<"account">> => Account,
        <<"bankName">> => BankName,
        <<"bankPostAccount">> => BankPostAccount,
        <<"bankBik">> => BankBik
    }.

decode_contract_adjustment(#domain_ContractAdjustment{
    id = ID,
    created_at = CreatedAt,
    valid_since = ValidSince,
    valid_until = ValidUntil
}) ->
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"createdAt">> => CreatedAt,
        <<"validSince">> => ValidSince,
        <<"validUntil">> => ValidUntil
    }).

decode_shop(#domain_Shop{
    id = ShopID,
    created_at = CreatedAt,
    blocking = Blocking,
    suspension = Suspension,
    category  = #domain_CategoryRef{
        id = CategoryRef
    },
    details  = ShopDetails,
    location = Location,
    account = ShopAccount,
    contract_id = ContractID,
    payout_tool_id = PayoutToolID
}) ->
    genlib_map:compact(#{
        <<"id">> => ShopID,
        <<"createdAt">> => CreatedAt,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension),
        <<"categoryID">> => CategoryRef,
        <<"details">> => decode_shop_details(ShopDetails),
        <<"location">> => decode_shop_location(Location),
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID,
        <<"account">> => decode_shop_account(ShopAccount)
    }).

decode_shop_details(#domain_ShopDetails{
    name = Name,
    description = Description
}) ->
    genlib_map:compact(#{
      <<"name">> => Name,
      <<"description">> => Description
    }).

decode_shop_location({url, Location}) ->
    #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => Location
    }.

decode_contractor({legal_entity, LegalEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"LegalEntity">>}, decode_legal_entity(LegalEntity));

decode_contractor({registered_user, RegisteredUser}) ->
    maps:merge(#{<<"contractorType">> => <<"RegisteredUser">>}, decode_registered_user(RegisteredUser)).

decode_legal_entity({
    russian_legal_entity,
    #domain_RussianLegalEntity{
        registered_name = RegisteredName,
        registered_number = RegisteredNumber,
        inn = Inn,
        actual_address = ActualAddress,
        post_address = PostAddress,
        representative_position = RepresentativePosition,
        representative_full_name = RepresentativeFullName,
        representative_document = RepresentativeDocument,
        bank_account = BankAccount
    }
}) ->
    #{
        <<"entityType">> => <<"RussianLegalEntity">>,
        <<"registeredName">> => RegisteredName,
        <<"registeredNumber">> => RegisteredNumber,
        <<"inn">> => Inn,
        <<"actualAddress">> => ActualAddress,
        <<"postAddress">> => PostAddress,
        <<"representativePosition">> => RepresentativePosition,
        <<"representativeFullName">> => RepresentativeFullName,
        <<"representativeDocument">> => RepresentativeDocument,
        <<"bankAccount">> => decode_bank_account(BankAccount)
    }.

decode_registered_user(#domain_RegisteredUser{email = Email}) ->
    #{<<"email">> => Email}.


decode_stat_response(customers_rate_stat, [Response]) ->
    #{
        <<"uniqueCount">> => genlib:to_int(maps:get(<<"unic_count">>, Response))
    };

decode_stat_response(customers_rate_stat, []) ->
    #{
        <<"uniqueCount">> => 0
    };

decode_stat_response(StatType, Response) when is_list(Response) ->
    [decode_stat(StatType, R) || R <- Response].

decode_stat(payments_conversion_stat, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"successfulCount">> => genlib:to_int(maps:get(<<"successful_count">>, Response)),
        <<"totalCount">> => genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"conversion">> => genlib:to_float(maps:get(<<"conversion">>, Response))
    };

decode_stat(payments_geo_stat, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"geoID">> => genlib:to_int(maps:get(<<"city_id">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat(payments_turnover, Response) ->
    #{
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"currency">> => maps:get(<<"currency_symbolic_code">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> => genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    };

decode_stat(payments_pmt_cards_stat, Response) ->
    #{
        <<"statType">> => <<"PaymentMethodBankCardStat">>, %% @TODO deal with nested responses decoding
        <<"offset">> => genlib:to_int(maps:get(<<"offset">>, Response)),
        <<"totalCount">> =>  genlib:to_int(maps:get(<<"total_count">>, Response)),
        <<"paymentSystem">> =>  maps:get(<<"payment_system">>, Response),
        <<"profit">> => genlib:to_int(maps:get(<<"amount_with_fee">>, Response)),
        <<"revenue">> =>  genlib:to_int(maps:get(<<"amount_without_fee">>, Response))
    }.

decode_claim(#payproc_Claim{
    id = ID,
    revision = Revision,
    created_at = CreatedAt,
    updated_at = UpdatedAt,
    status = Status,
    changeset = ChangeSet
}) ->
    genlib_map:compact(maps:merge(
        #{
            <<"id">> => ID,
            <<"revision">> => Revision,
            <<"createdAt">> => CreatedAt,
            <<"updatedAt">> => UpdatedAt,
            <<"changeset">> => decode_party_changeset(ChangeSet)
        },
        decode_claim_status(Status)
    )).

decode_claim_status({'pending', _}) ->
    #{
        <<"status">> => <<"ClaimPending">>
    };

decode_claim_status({'accepted', #payproc_ClaimAccepted{}}) ->
    #{
        <<"status">> => <<"ClaimAccepted">>
    };

decode_claim_status({'denied', #payproc_ClaimDenied{
    reason = Reason
}}) ->
    #{
        <<"status">> => <<"ClaimDenied">>,
        <<"reason">> => Reason
    };

decode_claim_status({'revoked', #payproc_ClaimRevoked{
    reason = Reason
}}) ->
    #{
        <<"status">> => <<"ClaimRevoked">>,
        <<"reason">> => Reason
    }.

decode_party_changeset(PartyChangeset) ->
    [decode_party_modification(PartyModification) || PartyModification <- PartyChangeset].

decode_party_modification({
    contract_modification,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = Modification
    }
}) ->
    maps:merge(#{
        <<"partyModificationType">> => <<"ContractModification">>,
        <<"contractID">> => ContractID
    }, decode_contract_modification(Modification));

decode_party_modification({
    shop_modification,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = ShopModification
    }
}) ->
    maps:merge(#{
        <<"partyModificationType">> => <<"ShopModification">>,
        <<"shopID">> => ShopID
    }, decode_shop_modification(ShopModification)).

decode_contract_modification({creation, #payproc_ContractParams{
    contractor = Contractor
}}) ->
    #{
        <<"contractModificationType">> => <<"ContractCreation">>,
        <<"contractor">> => decode_contractor(Contractor)
    };

decode_contract_modification({legal_agreement_binding, LegalAgreement}) ->
    #{
        <<"contractModificationType">> => <<"ContractLegalAgreementBinding">>,
        <<"legalAgreement">> => decode_legal_agreement(LegalAgreement)
    };

decode_contract_modification({adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
    adjustment_id = AdjustmentID,
    modification = {creation, #payproc_ContractAdjustmentParams{
        % FIXME need swag support for this
        template = _Template
    }}
}}) ->
    #{
        <<"contractModificationType">> => <<"ContractAdjustmentCreation">>,
        <<"adjustmentID">> => AdjustmentID
    };

decode_contract_modification({termination, #payproc_ContractTermination{
    reason = Reason
}}) ->
    genlib_map:compact(#{
        <<"contractModificationType">> => <<"ContractTermination">>,
        <<"reason">> => Reason
    });

decode_contract_modification({payout_tool_modification, #payproc_PayoutToolModificationUnit{
    payout_tool_id = PayoutToolID,
    modification = {creation, PayoutToolParams}
}}) ->
    Basic = #{
        <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
        <<"payoutToolID">> => PayoutToolID
    },
    maps:merge(Basic, decode_payout_tool_params(PayoutToolParams)).

decode_legal_agreement(#domain_LegalAgreement{
    signed_at = SignedAt,
    legal_agreement_id = ID
}) ->
    #{
        <<"id">> => ID,
        <<"signedAt">> => SignedAt
    };

decode_legal_agreement(undefined) ->
    undefined.


decode_shop_modification({creation, ShopParams}) ->
    maps:merge(
        #{<<"shopModificationType">> => <<"ShopCreation">>},
        decode_shop_params(ShopParams)
    );

decode_shop_modification({shop_account_creation, #payproc_ShopAccountParams{
    currency = Currency
}}) ->
    #{
        <<"shopModificationType">> => <<"ShopAccountCreation">>,
        <<"currency">> => decode_currency(Currency)
    };

decode_shop_modification({category_modification, CategoryRef}) ->
    #{
        <<"shopModificationType">> => <<"ShopCategoryChange">>,
        <<"categoryID">> => decode_category_ref(CategoryRef)
    };

decode_shop_modification({location_modification, Location}) ->
    #{
        <<"shopModificationType">> => <<"ShopLocationChange">>,
        <<"location">> => decode_shop_location(Location)
    };

decode_shop_modification({details_modification, Details}) ->
    #{
        <<"shopModificationType">> => <<"ShopDetailsChange">>,
        <<"details">> => decode_shop_details(Details)
    };

decode_shop_modification({contract_modification, #payproc_ShopContractModification{
    contract_id = ContractID,
    payout_tool_id = PayoutToolID
}}) ->
    #{
        <<"shopModificationType">> => <<"ShopContractBinding">>,
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID
    }.

decode_shop_params(#payproc_ShopParams{
    location =  Location,
    details = Details,
    contract_id = ContractID,
    payout_tool_id = PayoutToolID
}) ->
    #{
        <<"location">> => decode_shop_location(Location),
        <<"details">> => decode_shop_details(Details),
        <<"contractID">> => ContractID,
        <<"payoutToolID">> => PayoutToolID
    }.

decode_category(#domain_CategoryObject{
    ref = #domain_CategoryRef{
        id = CategoryRef
    },
    data = #domain_Category{
        name = Name,
        description = Description
    }
}) ->
    genlib_map:compact(#{
        <<"name">> => Name,
        <<"categoryID">> => CategoryRef,
        <<"description">> => Description
    }).

decode_category_ref(#domain_CategoryRef{
    id = CategoryRef
}) ->
    CategoryRef.

decode_shop_account(undefined) ->
    undefined;

decode_shop_account(#domain_ShopAccount{
    currency = Currency,
    settlement = SettlementID,
    guarantee = GuaranteeID
}) ->
    #{
        <<"guaranteeID">> => GuaranteeID,
        <<"settlementID">> => SettlementID,
        <<"currency">> => decode_currency(Currency)
    }.

decode_account_state(#payproc_AccountState{
    account_id = AccountID,
    own_amount = OwnAmount,
    available_amount = AvailableAmount,
    currency = Currency
}) ->
    #{
        <<"id">> => AccountID,
        <<"ownAmount">> => OwnAmount,
        <<"availableAmount">> => AvailableAmount,
        <<"currency">> => decode_currency(Currency)
    }.

decode_user_interaction({redirect, BrowserRequest}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => decode_browser_request(BrowserRequest)
    }.

decode_browser_request({get_request, #'BrowserGetRequest'{
    uri = UriTemplate
}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => UriTemplate
    };

decode_browser_request({post_request, #'BrowserPostRequest'{
    uri = UriTemplate,
    form = UserInteractionForm
}}) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => UriTemplate,
        <<"form">> => decode_user_interaction_form(UserInteractionForm)
    }.

decode_user_interaction_form(Form) ->
    maps:fold(
        fun(K, V, Acc) ->
            F = #{
                <<"key">> => K,
                <<"template">> => V
            },
            [F | Acc]
        end,
        [],
        Form
    ).

decode_geo_location_info(#geo_ip_LocationInfo{
    city_geo_id = CityID,
    country_geo_id = CountryID
}) ->
    #{
        <<"cityGeoID">> => CityID,
        <<"countryGeoID">> => CountryID
    };

decode_geo_location_info(undefined) ->
    undefined.

decode_currency(#domain_Currency{
    symbolic_code = SymbolicCode
}) ->
    SymbolicCode;

decode_currency(#domain_CurrencyRef{
    symbolic_code = SymbolicCode
}) ->
    SymbolicCode.

decode_event_filter({invoice, #webhooker_InvoiceEventFilter{
    shop_id = ShopID,
    types   = EventTypes
}}) ->
    genlib_map:compact(#{
        <<"topic">>      => <<"InvoicesTopic">>,
        <<"shopID">>     => ShopID,
        <<"eventTypes">> => lists:flatmap(
            fun (V) -> decode_event_type(invoice, V) end, ordsets:to_list(EventTypes)
        )
    }).

decode_event_type(
    invoice,
    {created, #webhooker_InvoiceCreated{}}
) ->
    [<<"InvoiceCreated">>];
decode_event_type(
    invoice,
    {status_changed, #webhooker_InvoiceStatusChanged{value = undefined}}
) ->
    % TODO seems unmaintainable
    [decode_invoice_status_event_type(V) || V <- [
        ?invpaid(),
        ?invcancelled(),
        ?invfulfilled()
    ]];
decode_event_type(
    invoice,
    {status_changed, #webhooker_InvoiceStatusChanged{value = Value}}
) ->
    [decode_invoice_status_event_type(Value)];
decode_event_type(
    invoice,
    {payment, {created, #webhooker_InvoicePaymentCreated{}}}
) ->
    [<<"PaymentStarted">>];
decode_event_type(
    invoice,
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = undefined}}}
) ->
    % TODO seems unmaintainable
    [decode_payment_status_event_type(V) || V <- [
        ?pmtprocessed(),
        ?pmtcaptured(),
        ?pmtcancelled(),
        ?pmtfailed()
    ]];
decode_event_type(
    invoice,
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = Value}}}
) ->
    [decode_payment_status_event_type(Value)].

decode_invoice_status_event_type(?invpaid())      -> <<"InvoicePaid">>;
decode_invoice_status_event_type(?invcancelled()) -> <<"InvoiceCancelled">>;
decode_invoice_status_event_type(?invfulfilled()) -> <<"InvoiceFulfilled">>.

decode_payment_status_event_type(?pmtprocessed()) -> <<"PaymentProcessed">>;
decode_payment_status_event_type(?pmtcaptured())  -> <<"PaymentCaptured">>;
decode_payment_status_event_type(?pmtcancelled()) -> <<"PaymentCancelled">>;
decode_payment_status_event_type(?pmtfailed())    -> <<"PaymentFailed">>.

decode_webhook(#webhooker_Webhook{
    id           = ID,
    party_id     = _PartyID,
    event_filter = EventFilter,
    url          = URL,
    pub_key      = PubKey,
    enabled      = Enabled
}) ->
    {ok, WebhookID} = decode_webhook_id(ID),
    #{
        <<"id">>        => WebhookID,
        <<"active">>    => Enabled,
        <<"scope">>     => decode_event_filter(EventFilter),
        <<"url">>       => URL,
        <<"publicKey">> => PubKey
    }.

decode_webhook_id(WebhookID) when is_integer(WebhookID) ->
    {ok, integer_to_binary(WebhookID)}.

is_blocked({blocked, _}) ->
    true;
is_blocked({unblocked, _}) ->
    false.

is_suspended({suspended, _}) ->
    true;
is_suspended({active, _}) ->
    false.

encode_invoice_params(InvoiceParams) ->
    InvoiceContext = jsx:encode(genlib_map:get(<<"metadata">>, InvoiceParams)),
    #payproc_InvoiceParams{
        party_id = genlib_map:get(<<"partyID">>, InvoiceParams),
        details = encode_invoice_details(InvoiceParams),
        cost = encode_cash(InvoiceParams),
        due = capi_utils:to_universal_time(genlib_map:get(<<"dueDate">>, InvoiceParams)),
        context  = #'Content'{
            type = <<"application/json">>,
            data = InvoiceContext
        },
        shop_id = genlib_map:get(<<"shopID">>, InvoiceParams)
    }.

encode_invoice_details(Params) ->
    #domain_InvoiceDetails{
        product = genlib_map:get(<<"product">>, Params),
        description = genlib_map:get(<<"description">>, Params)
    }.

encode_cash(Params) ->
    #domain_Cash{
        amount = genlib_map:get(<<"amount">>, Params),
        currency = encode_currency(genlib_map:get(<<"currency">>, Params))
    }.

encode_shop_params(Params) ->
    #payproc_ShopParams{
        location =  encode_shop_location(genlib_map:get(<<"location">>, Params)),
        details = encode_shop_details(genlib_map:get(<<"details">>, Params)),
        contract_id = genlib_map:get(<<"contractID">>, Params),
        payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params)
    }.

encode_shop_details(undefined) ->
    undefined;

encode_shop_details(Details = #{
    <<"name">> := Name
}) ->
    #domain_ShopDetails{
        name = Name,
        description = genlib_map:get(<<"description">>, Details)
    }.

encode_shop_location(#{
    <<"locationType">> := <<"ShopLocationUrl">>,
    <<"url">> := Url
}) ->
    {url, Url}.

encode_category_ref(undefined) ->
    undefined;

encode_category_ref(Ref) ->
    #domain_CategoryRef{
        id = Ref
    }.

encode_claim_changeset(Changeset) when is_list(Changeset)->
    lists:map(fun encode_party_modification/1, Changeset).

encode_party_modification(#{<<"partyModificationType">> := Type} = Modification) ->
    case Type of
        <<"ContractModification">> ->
            {contract_modification, encode_contract_modification(Modification)};
        <<"ShopModification">> ->
            {shop_modification, encode_shop_modification(Modification)}
    end.

encode_contract_modification(#{<<"contractID">> := ContractID} = Modification) ->
    EncodedMod = case maps:get(<<"contractModificationType">>, Modification) of
        <<"ContractCreation">> ->
            {creation, #payproc_ContractParams{
                contractor = encode_contractor(maps:get(<<"contractor">>, Modification))
            }};
        <<"ContractTermination">> ->
            {termination, #payproc_ContractTermination{
                reason = encode_reason(maps:get(<<"reason">>, Modification))
            }};
        <<"ContractLegalAgreementBinding">> ->
            {legal_agreement_binding, encode_legal_agreement(maps:get(<<"legalAgreement">>, Modification))};
        <<"ContractAdjustmentCreation">> ->
        % FIXME need swag supprot for template ref
        %     {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
        %         adjustment_id = maps:get(<<"adjustmentID">>, Modification),
        %         modification = {creation, #payproc_ContractAdjustmentParams{
        %             template = NOT_SUPPORTED
        %         }}
        %     }};
            erlang:throw({encode_contract_modification, adjustment_creation_not_supported});
        <<"ContractPayoutToolCreation">> ->
            {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                payout_tool_id = maps:get(<<"payoutToolID">>, Modification),
                modification = {creation, encode_payout_tool_params(Modification)}
            }}
    end,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = EncodedMod
    }.

encode_shop_modification(#{<<"shopID">> := ShopID} = Modification) ->
    EncodedMod = case maps:get(<<"shopModificationType">>, Modification) of
        <<"ShopCreation">> ->
            {creation, encode_shop_params(Modification)};
        <<"ShopAccountCreation">> ->
            {shop_account_creation, #payproc_ShopAccountParams{
                currency = encode_currency(maps:get(<<"currency">>, Modification))
            }};
        <<"ShopCategoryChange">> ->
            {category_modification, encode_category_ref(maps:get(<<"categoryID">>, Modification))};
        <<"ShopLocationChange">> ->
            {location_modification, encode_shop_location(maps:get(<<"location">>, Modification))};
        <<"ShopDetailsChange">> ->
            {details_modification, encode_shop_details(maps:get(<<"details">>, Modification))};
        <<"ShopContractBinding">> ->
            {contract_modification, #payproc_ShopContractModification{
                contract_id = maps:get(<<"contractID">>, Modification),
                payout_tool_id = maps:get(<<"payoutToolID">>, Modification)
            }}
    end,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = EncodedMod
    }.

encode_reason(undefined) ->
    undefined;
encode_reason(#{<<"reason">> := Reason}) ->
    Reason.

encode_legal_agreement(#{<<"id">> := ID, <<"signedAt">> := SignedAt}) ->
    #domain_LegalAgreement{
        signed_at = SignedAt,
        legal_agreement_id = ID
    }.

encode_payout_tool_params(#{
    <<"currency">> := Currency,
    <<"details">> := Details
}) ->
    #payproc_PayoutToolParams{
        currency = encode_currency(Currency),
        tool_info = encode_payout_tool_info(Details)
    }.

encode_payout_tool_info(#{<<"type">> := <<"PayoutToolBankAccount">>} = Tool) ->
   {bank_account, encode_bank_account(maps:get(<<"bankAccount">>, Tool))}.

encode_bank_account(#{
    <<"account">> := Account,
    <<"bankName">> := BankName,
    <<"bankPostAccount">> := BankPostAccount,
    <<"bankBik">> := BankBik
}) ->
    #domain_BankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
    }.

encode_contractor(#{<<"contractorType">> := <<"LegalEntity">>} = Contractor) ->
    {legal_entity, encode_legal_entity(Contractor)};

encode_contractor(#{<<"contractorType">> := <<"RegisteredUser">>} = Contractor) ->
    {registered_user, encode_registered_user(Contractor)}.

encode_legal_entity(#{
    <<"entityType">> := <<"RussianLegalEntity">>
} = Entity) ->
    {russian_legal_entity , #domain_RussianLegalEntity{
        registered_name = maps:get(<<"registeredName">>, Entity),
        registered_number = maps:get(<<"registeredNumber">>, Entity),
        inn = maps:get(<<"inn">>, Entity),
        actual_address = maps:get(<<"actualAddress">>, Entity),
        post_address = maps:get(<<"postAddress">>, Entity),
        representative_position = maps:get(<<"representativePosition">>, Entity),
        representative_full_name = maps:get(<<"representativeFullName">>, Entity),
        representative_document = maps:get(<<"representativeDocument">>, Entity),
        bank_account = encode_bank_account(maps:get(<<"bankAccount">>, Entity))
    }}.

encode_registered_user(#{<<"email">> := Email}) ->
    #domain_RegisteredUser{email = Email}.

encode_bank_card(Encoded) ->
    #{
        <<"token">> := Token,
        <<"payment_system">> := PaymentSystem,
        <<"bin">> := Bin,
        <<"masked_pan">> := MaskedPan
    } = try capi_utils:base64url_to_map(Encoded)
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end,
    {bank_card, #domain_BankCard{
        'token'  = Token,
        'payment_system' = binary_to_existing_atom(PaymentSystem, utf8),
        'bin' = Bin,
        'masked_pan' = MaskedPan
    }}.

encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{
        symbolic_code = SymbolicCode
    }.

encode_event_filter(#{
    <<"topic">>      := <<"InvoicesTopic">>,
    <<"shopID">>     := ShopID,
    <<"eventTypes">> := EventTypes
}) ->
    {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ShopID,
        types   = ordsets:from_list([
            encode_event_type(invoices, V) || V <- EventTypes
        ])
    }}.

encode_event_type(invoices, <<"InvoiceCreated">>) ->
    {created, #webhooker_InvoiceCreated{}};
encode_event_type(invoices, <<"InvoicePaid">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invpaid()}};
encode_event_type(invoices, <<"InvoiceCancelled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invcancelled()}};
encode_event_type(invoices, <<"InvoiceFulfilled">>) ->
    {status_changed, #webhooker_InvoiceStatusChanged{value = ?invfulfilled()}};
encode_event_type(invoices, <<"PaymentStarted">>) ->
    {payment, {created, #webhooker_InvoicePaymentCreated{}}};
encode_event_type(invoices, <<"PaymentProcessed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtprocessed()}}};
encode_event_type(invoices, <<"PaymentCaptured">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcaptured()}}};
encode_event_type(invoices, <<"PaymentCancelled">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtcancelled()}}};
encode_event_type(invoices, <<"PaymentFailed">>) ->
    {payment, {status_changed, #webhooker_InvoicePaymentStatusChanged{value = ?pmtfailed()}}}.

encode_stat_request(Dsl) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl));

encode_stat_request(Dsl) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl
    }.

encode_webhook_id(WebhookID) ->
    try
        ID = binary_to_integer(WebhookID),
        {ok, ID}
    catch
        error:badarg ->
            error
    end.
