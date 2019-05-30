-module(capi_handler_decoder_invoicing).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-export([decode_user_interaction_form/1]).
-export([decode_user_interaction/1]).
-export([decode_payment/3]).
-export([decode_refund/2]).
-export([decode_invoice/1]).
-export([decode_invoice_cart/1]).
-export([decode_invoice_line_tax_mode/1]).
-export([decode_payment_status/2]).
-export([decode_payment_operation_failure/2]).
-export([decode_refund_status/2]).
-export([decode_recurrent_parent/1]).
-export([decode_make_recurrent/1]).

-export([construct_payment_methods/3]).
-export([make_invoice_and_token/2]).
-export([make_invoice_and_token/3]).

-type processing_context() :: capi_handler:processing_context().

%%

-spec decode_user_interaction({atom(), _}) ->
    capi_handler_decoder_utils:decode_data().

decode_user_interaction({payment_terminal_reciept, TerminalReceipt}) ->
    #{
        <<"interactionType">> => <<"PaymentTerminalReceipt">>,
        <<"shortPaymentID" >> => TerminalReceipt#'PaymentTerminalReceipt'.short_payment_id,
        <<"dueDate"        >> => TerminalReceipt#'PaymentTerminalReceipt'.due
    };
decode_user_interaction({redirect, BrowserRequest}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => decode_browser_request(BrowserRequest)
    }.

decode_browser_request({get_request, #'BrowserGetRequest'{uri = UriTemplate}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => UriTemplate
    };
decode_browser_request({post_request, #'BrowserPostRequest'{uri = UriTemplate, form = UserInteractionForm}}) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => UriTemplate,
        <<"form">> => decode_user_interaction_form(UserInteractionForm)
    }.

-spec decode_user_interaction_form(map()) ->
    capi_handler_decoder_utils:decode_data().

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

-spec decode_payment(binary(), capi_handler_encoder:encode_data(), processing_context()) ->
    capi_handler_decoder_utils:decode_data().

decode_payment(InvoiceID, Payment, Context) ->
    #domain_Cash{
        amount   = Amount,
        currency = Currency
    } = Payment#domain_InvoicePayment.cost,
    capi_handler_utils:merge_and_compact(#{
        <<"id"           >> => Payment#domain_InvoicePayment.id,
        <<"externalID"   >> => Payment#domain_InvoicePayment.external_id,
        <<"invoiceID"    >> => InvoiceID,
        <<"createdAt"    >> => Payment#domain_InvoicePayment.created_at,
        % TODO whoops, nothing to get it from yet
        <<"flow"         >> => decode_flow(Payment#domain_InvoicePayment.flow),
        <<"amount"       >> => Amount,
        <<"currency"     >> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"payer"        >> => decode_payer(Payment#domain_InvoicePayment.payer),
        <<"makeRecurrent">> => decode_make_recurrent(Payment#domain_InvoicePayment.make_recurrent),
        <<"metadata"     >> => capi_handler_decoder_utils:decode_context(Payment#domain_InvoicePayment.context)
    }, decode_payment_status(Payment#domain_InvoicePayment.status, Context)).

decode_payer({customer, #domain_CustomerPayer{customer_id = ID}}) ->
    #{
        <<"payerType" >> => <<"CustomerPayer">>,
        <<"customerID">> => ID
    };
decode_payer({recurrent, #domain_RecurrentPayer{recurrent_parent = RecurrentParent, contact_info = ContactInfo}}) ->
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"contactInfo">> => capi_handler_decoder_party:decode_contact_info(ContactInfo),
        <<"recurrentParentPayment">> => decode_recurrent_parent(RecurrentParent)
    };
decode_payer({payment_resource, #domain_PaymentResourcePayer{resource = Resource, contact_info = ContactInfo}}) ->
    maps:merge(
        #{
            <<"payerType"  >> => <<"PaymentResourcePayer">>,
            <<"contactInfo">> => capi_handler_decoder_party:decode_contact_info(ContactInfo)
        },
        capi_handler_decoder_party:decode_disposable_payment_resource(Resource)
    ).

-spec decode_payment_status({atom(), _}, processing_context()) ->
    capi_handler_decoder_utils:decode_data().

decode_payment_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentFailed{failure = OperationFailure} ->
                decode_payment_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

-spec decode_payment_operation_failure({atom(), _}, processing_context()) ->
    capi_handler_decoder_utils:decode_data().

decode_payment_operation_failure({operation_timeout, _}, _) ->
    payment_error(<<"timeout">>);
decode_payment_operation_failure({failure, Failure}, Context) ->
    case capi_auth:get_consumer(capi_auth:get_claims(capi_handler_utils:get_auth_context(Context))) of
        client ->
            payment_error(payproc_errors:match('PaymentFailure', Failure, fun payment_error_client_maping/1));
        merchant ->
            % чтобы не городить ещё один обход дерева как в payproc_errors проще отформатировать в текст,
            % а потом уже в json
            decode_payment_operation_failure_(
                binary:split(erlang:list_to_binary(payproc_errors:format_raw(Failure)), <<":">>, [global])
            )
    end.

decode_payment_operation_failure_([H|T]) ->
    R = payment_error(H),
    case T of
        [] -> R;
        _  -> R#{<<"subError">> => decode_payment_operation_failure_(T)}
    end.

decode_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};

decode_flow({hold, #domain_InvoicePaymentFlowHold{on_hold_expiration = OnHoldExpiration, held_until = HeldUntil}}) ->
    #{
        <<"type"            >> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil"       >> => HeldUntil
    }.

-spec decode_make_recurrent(undefined | boolean()) ->
    boolean().

decode_make_recurrent(undefined) ->
    false;
decode_make_recurrent(Value) when is_boolean(Value) ->
    Value.

-spec decode_recurrent_parent(capi_handler_encoder:encode_data()) ->
    capi_handler_decoder_utils:decode_data().

decode_recurrent_parent(#domain_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    };
decode_recurrent_parent(#merchstat_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    }.

payment_error(Code) ->
    #{<<"code">> => Code}.

%% client error mapping
%% @see https://github.com/petrkozorezov/swag/blob/master/spec/definitions/PaymentError.yaml
-spec payment_error_client_maping(capi_handler_encoder:encode_data()) ->
    binary().
payment_error_client_maping({preauthorization_failed, _})->
    <<"PreauthorizationFailed">>;
payment_error_client_maping({authorization_failed, {account_blocked, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {rejected_by_issuer, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {payment_tool_rejected, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_not_found, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_limit_exceeded, _}}) ->
    <<"AccountLimitsExceeded">>;
payment_error_client_maping({authorization_failed, {insufficient_funds, _}}) ->
    <<"InsufficientFunds">>;
payment_error_client_maping(_) ->
    <<"PaymentRejected">>.

-spec decode_refund(capi_handler_encoder:encode_data(), processing_context()) ->
    capi_handler_decoder_utils:decode_data().

decode_refund(Refund, Context) ->
    #domain_Cash{amount = Amount, currency = Currency} = Refund#domain_InvoicePaymentRefund.cash,
    capi_handler_utils:merge_and_compact(
        #{
            <<"id"       >> => Refund#domain_InvoicePaymentRefund.id,
            <<"createdAt">> => Refund#domain_InvoicePaymentRefund.created_at,
            <<"reason"   >> => Refund#domain_InvoicePaymentRefund.reason,
            <<"amount"   >> => Amount,
            <<"currency" >> => capi_handler_decoder_utils:decode_currency(Currency)
        },
        decode_refund_status(Refund#domain_InvoicePaymentRefund.status, Context)
    ).

-spec decode_refund_status({atom(), _}, processing_context()) ->
    capi_handler_decoder_utils:decode_data().

decode_refund_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentRefundFailed{failure = OperationFailure} ->
                capi_handler_decoder_utils:decode_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

-spec decode_invoice(capi_handler_encoder:encode_data()) ->
    capi_handler_decoder_utils:decode_data().

decode_invoice(Invoice) ->
    #domain_Cash{amount = Amount, currency = Currency} = Invoice#domain_Invoice.cost,
    #domain_InvoiceDetails{product = Product, description = Description, cart = Cart} =
        Invoice#domain_Invoice.details,
    capi_handler_utils:merge_and_compact(#{
        <<"id"               >> => Invoice#domain_Invoice.id,
        <<"externalID"       >> => Invoice#domain_Invoice.external_id,
        <<"shopID"           >> => Invoice#domain_Invoice.shop_id,
        <<"createdAt"        >> => Invoice#domain_Invoice.created_at,
        <<"dueDate"          >> => Invoice#domain_Invoice.due,
        <<"amount"           >> => Amount,
        <<"currency"         >> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"metadata"         >> => capi_handler_decoder_utils:decode_context(Invoice#domain_Invoice.context),
        <<"product"          >> => Product,
        <<"description"      >> => Description,
        <<"cart"             >> => decode_invoice_cart(Cart),
        <<"invoiceTemplateID">> => Invoice#domain_Invoice.template_id
    }, decode_invoice_status(Invoice#domain_Invoice.status)).

decode_invoice_status({Status, StatusInfo}) ->
    Reason =
        case StatusInfo of
            #domain_InvoiceCancelled{details = Details} -> Details;
            #domain_InvoiceFulfilled{details = Details} -> Details;
            _ -> undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"reason">> => Reason
    }.

-spec decode_invoice_cart(capi_handler_encoder:encode_data() | undefined) ->
    capi_handler_decoder_utils:decode_data() | undefined.

decode_invoice_cart(#domain_InvoiceCart{lines = Lines}) ->
    [decode_invoice_line(L) || L <- Lines];
decode_invoice_cart(undefined) ->
    undefined.

decode_invoice_line(InvoiceLine = #domain_InvoiceLine{quantity = Quantity, price = #domain_Cash{amount = Price}}) ->
    genlib_map:compact(#{
        <<"product" >> => InvoiceLine#domain_InvoiceLine.product,
        <<"quantity">> => Quantity,
        <<"price"   >> => Price,
        <<"cost"    >> => Price * Quantity,
        <<"taxMode" >> => decode_invoice_line_tax_mode(InvoiceLine#domain_InvoiceLine.metadata)
    }).

-spec decode_invoice_line_tax_mode(map()) ->
    capi_handler_decoder_utils:decode_data() | undefined.

decode_invoice_line_tax_mode(#{<<"TaxMode">> := {str, TM}}) ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    #{
       <<"type">> => <<"InvoiceLineTaxVAT">>,
       <<"rate">> => TM
    };
decode_invoice_line_tax_mode(_) ->
    undefined.

-spec construct_payment_methods(atom(), list(), processing_context()) ->
    {ok, list()} | woody:result().

construct_payment_methods(ServiceName, Args, Context) ->
    case compute_terms(ServiceName, Args, Context) of
        {ok, #domain_TermSet{payments = undefined}} ->
            {ok, []};
        {ok, #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = PaymentMethodRefs
            }
        }} ->
            {ok, decode_payment_methods(PaymentMethodRefs)};
        Error ->
            Error
    end.

decode_payment_methods(undefined) ->
    [];
decode_payment_methods({value, PaymentMethodRefs}) ->
    PaymentMethods = [ID || #domain_PaymentMethodRef{id = ID} <- PaymentMethodRefs],
    lists:foldl(
        fun(Method, Acc) ->
            {_, MethodTerms} = lists:unzip(proplists:lookup_all(Method, PaymentMethods)),
            decode_payment_method(Method, MethodTerms) ++ Acc
        end,
        [],
        proplists:get_keys(PaymentMethods)
    ).

decode_payment_method(empty_cvv_bank_card, PaymentSystems) ->
    [#{<<"method">> => <<"BankCard">>, <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems)}];
decode_payment_method(bank_card, PaymentSystems) ->
    [#{<<"method">> => <<"BankCard">>, <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems)}];
decode_payment_method(payment_terminal, Providers) ->
    [#{<<"method">> => <<"PaymentTerminal">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}];
decode_payment_method(digital_wallet, Providers) ->
    [#{<<"method">> => <<"DigitalWallet">>, <<"providers">> => lists:map(fun genlib:to_binary/1, Providers)}];
decode_payment_method(tokenized_bank_card, TokenizedBankCards) ->
    decode_tokenized_bank_cards(TokenizedBankCards);
decode_payment_method(crypto_wallet, CryptoCurrencies) ->
    [#{<<"method">> => <<"CryptoWallet">>,
        <<"cryptoCurrency">> => lists:map(fun capi_handler_decoder_utils:convert_crypto_currency/1, CryptoCurrencies)}].

decode_tokenized_bank_cards(TokenizedBankCards) ->
    PropTokenizedBankCards = [
        {TP, PS} || #domain_TokenizedBankCard{payment_system = PS, token_provider = TP} <- TokenizedBankCards
    ],
    lists:map(
        fun(TokenProvider) ->
            {_, PaymentSystems} = lists:unzip(proplists:lookup_all(TokenProvider, PropTokenizedBankCards)),
            decode_tokenized_bank_card(TokenProvider, PaymentSystems)
        end,
        proplists:get_keys(PropTokenizedBankCards)
    ).

decode_tokenized_bank_card(TokenProvider, PaymentSystems) ->
    #{
        <<"method">> => <<"BankCard">>,
        <<"paymentSystems">> => lists:map(fun genlib:to_binary/1, PaymentSystems),
        <<"tokenProviders">> => [genlib:to_binary(TokenProvider)]
    }.

compute_terms(ServiceName, Args, Context) ->
    capi_handler_utils:service_call_with([user_info], {ServiceName, 'ComputeTerms', Args}, Context).

-spec make_invoice_and_token(capi_handler_encoder:encode_data(), binary()) ->
    capi_handler_decoder_utils:decode_data().

make_invoice_and_token(Invoice, PartyID) ->
    make_invoice_and_token(Invoice, PartyID, #{}).

-spec make_invoice_and_token(capi_handler_encoder:encode_data(), binary(), map()) ->
    capi_handler_decoder_utils:decode_data().

make_invoice_and_token(Invoice, PartyID, ExtraProperties) ->
    #{
        <<"invoice"           >> => decode_invoice(Invoice),
        <<"invoiceAccessToken">> => capi_handler_utils:issue_access_token(
            PartyID,
            {invoice, Invoice#domain_Invoice.id},
            ExtraProperties
        )
    }.
