-module(capi_handler_invoices).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreateInvoice' = OperationID, Req, Context) ->
    InvoiceParams = maps:get('InvoiceParams', Req),
    UserID = capi_handler_utils:get_user_id(Context),
    %% Now hellgate checks user's possibility use party.
    %% After integration with bouncer, we have to remove validation in hellgate.
    PartyID = maps:get(<<"partyID">>, InvoiceParams, UserID),
    Authorize = fun() ->
        ShopID = maps:get(<<"shopID">>, InvoiceParams),
        Prototypes = [
            {operation, #{id => OperationID, party => PartyID, shop => ShopID}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        try
            ExtraProperties = capi_handler_utils:get_extra_properties(Context),
            case create_invoice(PartyID, InvoiceParams, Context, OperationID) of
                {ok, #'payproc_Invoice'{invoice = Invoice}} ->
                    {ok,
                        {201, #{},
                            capi_handler_decoder_invoicing:make_invoice_and_token(
                                Invoice,
                                PartyID,
                                ExtraProperties
                            )}};
                {exception, Exception} ->
                    case Exception of
                        #'payproc_InvalidUser'{} ->
                            {ok, logic_error(invalidPartyID, <<"Party not found">>)};
                        #'InvalidRequest'{errors = Errors} ->
                            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                            {ok, logic_error(invalidRequest, FormattedErrors)};
                        #payproc_ShopNotFound{} ->
                            {ok, logic_error(invalidShopID, <<"Shop not found">>)};
                        #payproc_InvalidPartyStatus{} ->
                            {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                        #payproc_InvalidShopStatus{} ->
                            {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                        #payproc_InvoiceTermsViolated{} ->
                            {ok, logic_error(invoiceTermsViolated, <<"Invoice parameters violate contract terms">>)}
                    end
            end
        catch
            invoice_cart_empty ->
                {ok, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)};
            invalid_invoice_cost ->
                {ok, logic_error(invalidInvoiceCost, <<"Invalid invoice amount">>)};
            {external_id_conflict, InvoiceID, ExternalID, _Schema} ->
                {ok, logic_error(externalIDConflict, {InvoiceID, ExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateInvoiceAccessToken' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    ResultInvoice = maybe_result(capi_handler_utils:get_invoice_by_id(InvoiceID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultInvoice, general_error(404, <<"Invoice not found">>)),
        #'payproc_Invoice'{invoice = Invoice} = ResultInvoice,
        ExtraProperties = capi_handler_utils:get_extra_properties(Context),
        #'domain_Invoice'{owner_id = PartyID} = Invoice,
        Response = capi_handler_utils:issue_access_token(
            PartyID,
            {invoice, InvoiceID},
            ExtraProperties
        ),
        {ok, {201, #{}, Response}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoiceByID' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    ResultInvoice = maybe_result(capi_handler_utils:get_invoice_by_id(InvoiceID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultInvoice, general_error(404, <<"Invoice not found">>)),
        #'payproc_Invoice'{invoice = Invoice} = ResultInvoice,
        {ok, {200, #{}, capi_handler_decoder_invoicing:decode_invoice(Invoice)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoiceByExternalID' = OperationID, Req, Context) ->
    ExternalID = maps:get(externalID, Req),
    {InvoiceID, ResultInvoice} =
        case get_invoice_by_external_id(ExternalID, Context) of
            {ok, Result} ->
                Result;
            {error, internal_id_not_found} ->
                {undefined, undefined};
            {exception, _Exception} ->
                {undefined, undefined}
        end,
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultInvoice, general_error(404, <<"Invoice not found">>)),
        #'payproc_Invoice'{invoice = Invoice} = ResultInvoice,
        {ok, {200, #{}, capi_handler_decoder_invoicing:decode_invoice(Invoice)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('FulfillInvoice' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => InvoiceID}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        CallArgs = {InvoiceID, maps:get(<<"reason">>, maps:get('Reason', Req))},
        Call = {invoicing, 'Fulfill', CallArgs},
        case capi_handler_utils:service_call_with([user_info], Call, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidInvoiceStatus{} ->
                        {ok, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)};
                    #payproc_InvalidPartyStatus{} ->
                        {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                    #payproc_InvalidShopStatus{} ->
                        {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                    #payproc_InvalidUser{} ->
                        {ok, general_error(404, <<"Invoice not found">>)};
                    #payproc_InvoiceNotFound{} ->
                        {ok, general_error(404, <<"Invoice not found">>)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('RescindInvoice' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => InvoiceID}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        CallArgs = {InvoiceID, maps:get(<<"reason">>, maps:get('Reason', Req))},
        Call = {invoicing, 'Rescind', CallArgs},
        case capi_handler_utils:service_call_with([user_info], Call, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidInvoiceStatus{} ->
                        {ok, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)};
                    #payproc_InvoicePaymentPending{} ->
                        ErrorResp = logic_error(invoicePaymentPending, <<"Invoice payment pending">>),
                        {ok, ErrorResp};
                    #payproc_InvalidPartyStatus{} ->
                        {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                    #payproc_InvalidShopStatus{} ->
                        {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                    #payproc_InvalidUser{} ->
                        {ok, general_error(404, <<"Invoice not found">>)};
                    #payproc_InvoiceNotFound{} ->
                        {ok, general_error(404, <<"Invoice not found">>)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoiceEvents' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => InvoiceID}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        Result =
            capi_handler_utils:collect_events(
                maps:get(limit, Req),
                genlib_map:get(eventID, Req),
                fun(Range) ->
                    capi_handler_utils:service_call_with(
                        [user_info],
                        {invoicing, 'GetEvents', {maps:get(invoiceID, Req), Range}},
                        Context
                    )
                end,
                fun decode_invoice_event/2,
                Context
            ),
        case Result of
            {ok, Events} when is_list(Events) ->
                {ok, {200, #{}, Events}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidUser{} ->
                        {ok, general_error(404, <<"Invoice not found">>)};
                    #payproc_InvoiceNotFound{} ->
                        {ok, general_error(404, <<"Invoice not found">>)};
                    #payproc_EventNotFound{} ->
                        {ok, general_error(404, <<"Event not found">>)};
                    #'InvalidRequest'{errors = Errors} ->
                        FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                        {ok, logic_error(invalidRequest, FormattedErrors)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoicePaymentMethods' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    ResultInvoice = maybe_result(capi_handler_utils:get_invoice_by_id(InvoiceID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = capi_auth:authorize_operation(OperationID, Prototypes, Context, Req),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultInvoice, general_error(404, <<"Invoice not found">>)),
        Party = capi_utils:unwrap(capi_handler_utils:get_party(Context)),
        Revision = Party#domain_Party.revision,
        Args = {InvoiceID, {revision, Revision}},
        case capi_handler_decoder_invoicing:construct_payment_methods(invoicing, Args, Context) of
            {ok, PaymentMethods0} when is_list(PaymentMethods0) ->
                PaymentMethods1 = capi_utils:deduplicate_payment_methods(PaymentMethods0),
                TokenProviderData = prepare_token_provider_data(ResultInvoice, Context),
                PaymentMethods = mixin_token_provider_data(PaymentMethods1, TokenProviderData),
                {ok, {200, #{}, PaymentMethods}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidUser{} ->
                        {ok, general_error(404, <<"Invoice not found">>)};
                    #payproc_InvoiceNotFound{} ->
                        {ok, general_error(404, <<"Invoice not found">>)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

create_invoice(PartyID, InvoiceParams, Context, BenderPrefix) ->
    #{woody_context := WoodyCtx} = Context,
    ExternalID = maps:get(<<"externalID">>, InvoiceParams, undefined),
    IdempotentKey = {BenderPrefix, PartyID, ExternalID},
    Identity = {schema, capi_feature_schemas:invoice(), InvoiceParams},
    InvoiceID = capi_bender:try_gen_snowflake(IdempotentKey, Identity, WoodyCtx),
    Call = {invoicing, 'Create', {encode_invoice_params(InvoiceID, PartyID, InvoiceParams)}},
    capi_handler_utils:service_call_with([user_info], Call, Context).

encode_invoice_params(ID, PartyID, InvoiceParams) ->
    Amount = genlib_map:get(<<"amount">>, InvoiceParams),
    Currency = genlib_map:get(<<"currency">>, InvoiceParams),
    Cart = genlib_map:get(<<"cart">>, InvoiceParams),
    #payproc_InvoiceParams{
        id = ID,
        party_id = PartyID,
        details = encode_invoice_details(InvoiceParams),
        cost = encode_invoice_cost(Amount, Currency, Cart),
        due = capi_handler_utils:get_time(<<"dueDate">>, InvoiceParams),
        context = capi_handler_encoder:encode_invoice_context(InvoiceParams),
        shop_id = genlib_map:get(<<"shopID">>, InvoiceParams),
        external_id = genlib_map:get(<<"externalID">>, InvoiceParams, undefined)
    }.

encode_invoice_cost(Amount, Currency, Cart) when Amount =/= undefined, Cart =/= undefined ->
    case get_invoice_cart_amount(Cart) of
        Amount ->
            capi_handler_encoder:encode_cash(Amount, Currency);
        _ ->
            throw(invalid_invoice_cost)
    end;
encode_invoice_cost(undefined, Currency, Cart) when Cart =/= undefined ->
    capi_handler_encoder:encode_cash(get_invoice_cart_amount(Cart), Currency);
encode_invoice_cost(Amount, Currency, undefined) when Amount =/= undefined ->
    capi_handler_encoder:encode_cash(Amount, Currency);
encode_invoice_cost(_, _, _) ->
    throw(invalid_invoice_cost).

get_invoice_cart_amount(Cart) ->
    lists:foldl(
        fun(Line, Acc) ->
            P = genlib_map:get(<<"price">>, Line),
            Q = genlib_map:get(<<"quantity">>, Line),
            Acc + (P * Q)
        end,
        0,
        Cart
    ).

encode_invoice_details(Params) ->
    #domain_InvoiceDetails{
        product = genlib_map:get(<<"product">>, Params),
        description = genlib_map:get(<<"description">>, Params),
        cart = capi_handler_encoder:encode_invoice_cart(Params),
        bank_account = capi_handler_encoder:encode_invoice_bank_account(Params)
    }.

%%

decode_invoice_event(Event, Context) ->
    %%@TODO deal with Party source
    #payproc_Event{payload = {invoice_changes, InvoiceChanges}, source = {invoice_id, InvoiceID}} = Event,
    case decode_invoice_changes(InvoiceID, InvoiceChanges, Context) of
        [_Something | _] = Changes ->
            {true, #{
                <<"id">> => Event#payproc_Event.id,
                <<"createdAt">> => Event#payproc_Event.created_at,
                <<"changes">> => Changes
            }};
        [] ->
            false
    end.

decode_invoice_changes(InvoiceID, InvoiceChanges, Context) ->
    F = fun(Change, Acc) ->
        case decode_invoice_change(InvoiceID, Change, Context) of
            #{} = Decoded ->
                Acc ++ [Decoded];
            undefined ->
                Acc
        end
    end,
    lists:foldl(F, [], InvoiceChanges).

decode_invoice_change(_, {invoice_created, #payproc_InvoiceCreated{invoice = Invoice}}, _Context) ->
    #{
        <<"changeType">> => <<"InvoiceCreated">>,
        <<"invoice">> => capi_handler_decoder_invoicing:decode_invoice(Invoice)
    };
decode_invoice_change(_, {invoice_status_changed, #payproc_InvoiceStatusChanged{status = {Status, _}}}, _Context) ->
    #{
        <<"changeType">> => <<"InvoiceStatusChanged">>,
        <<"status">> => genlib:to_binary(Status)
    };
decode_invoice_change(InvoiceID, {invoice_payment_change, PaymentChange}, Context) ->
    #payproc_InvoicePaymentChange{id = PaymentID, payload = Change} = PaymentChange,
    decode_payment_change(InvoiceID, PaymentID, Change, Context);
decode_invoice_change(_, _, _) ->
    undefined.

decode_payment_change(InvoiceID, _PaymentID, {invoice_payment_started, PaymentStarted}, Context) ->
    #payproc_InvoicePaymentStarted{payment = Payment} = PaymentStarted,
    #{
        <<"changeType">> => <<"PaymentStarted">>,
        <<"payment">> => capi_handler_decoder_invoicing:decode_payment(InvoiceID, Payment, Context)
    };
decode_payment_change(
    _InvoiceID,
    PaymentID,
    {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
        payload = {session_interaction_requested, InteractionRequested}
    }},
    _Context
) ->
    #payproc_SessionInteractionRequested{interaction = Interaction} = InteractionRequested,
    #{
        <<"changeType">> => <<"PaymentInteractionRequested">>,
        <<"paymentID">> => PaymentID,
        <<"userInteraction">> => capi_handler_decoder_invoicing:decode_user_interaction(Interaction)
    };
decode_payment_change(_InvoiceID, PaymentID, {invoice_payment_status_changed, PaymentStatusChanged}, Context) ->
    #payproc_InvoicePaymentStatusChanged{status = Status} = PaymentStatusChanged,
    capi_handler_utils:merge_and_compact(
        #{
            <<"changeType">> => <<"PaymentStatusChanged">>,
            <<"paymentID">> => PaymentID
        },
        capi_handler_decoder_invoicing:decode_payment_status(Status, Context)
    );
decode_payment_change(InvoiceID, PaymentID, {invoice_payment_refund_change, PaymentRefundChange}, Context) ->
    #payproc_InvoicePaymentRefundChange{
        id = RefundID,
        payload = Change
    } = PaymentRefundChange,
    decode_refund_change(InvoiceID, PaymentID, RefundID, Change, Context);
decode_payment_change(_, _, _, _) ->
    undefined.

decode_refund_change(InvoiceID, PaymentID, _RefundID, {invoice_payment_refund_created, Created}, Context) ->
    #payproc_InvoicePaymentRefundCreated{refund = Refund} = Created,
    #{
        <<"changeType">> => <<"RefundStarted">>,
        <<"paymentID">> => PaymentID,
        <<"refund">> => decode_refund_for_event(Refund, InvoiceID, PaymentID, Context)
    };
decode_refund_change(_, PaymentID, RefundID, {invoice_payment_refund_status_changed, StatusChanged}, Context) ->
    #payproc_InvoicePaymentRefundStatusChanged{status = Status} = StatusChanged,
    capi_handler_utils:merge_and_compact(
        #{
            <<"changeType">> => <<"RefundStatusChanged">>,
            <<"paymentID">> => PaymentID,
            <<"refundID">> => RefundID
        },
        capi_handler_decoder_invoicing:decode_refund_status(Status, Context)
    );
decode_refund_change(_, _, _, _, _) ->
    undefined.

decode_refund_for_event(#domain_InvoicePaymentRefund{cash = #domain_Cash{}} = Refund, _, _, Context) ->
    capi_handler_decoder_invoicing:decode_refund(Refund, Context);
decode_refund_for_event(#domain_InvoicePaymentRefund{cash = undefined} = Refund, InvoiceID, PaymentID, Context) ->
    % Need to fix it!
    {ok, #payproc_InvoicePayment{payment = #domain_InvoicePayment{cost = Cash}}} =
        capi_handler_utils:get_payment_by_id(InvoiceID, PaymentID, Context),
    capi_handler_decoder_invoicing:decode_refund(Refund#domain_InvoicePaymentRefund{cash = Cash}, Context).

get_invoice_by_external_id(ExternalID, #{woody_context := WoodyContext} = Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    InvoiceKey = {'CreateInvoice', PartyID, ExternalID},
    case capi_bender:get_internal_id(InvoiceKey, WoodyContext) of
        {ok, InvoiceID, _CtxData} ->
            case capi_handler_utils:get_invoice_by_id(InvoiceID, Context) of
                {ok, Invoice} ->
                    {ok, {InvoiceID, Invoice}};
                Exception ->
                    Exception
            end;
        Error ->
            Error
    end.

maybe_result({ok, Value}) ->
    Value;
maybe_result(_) ->
    undefined.

prepare_token_provider_data(Invoice, Context) ->
    %#payproc_Invoice{invoice = #domain_Invoice{id = InvoiceID}} = Invoice,
    #payproc_Invoice{invoice = #domain_Invoice{shop_id = ShopID}} = Invoice,
    Shop = maybe_result(capi_handler_utils:get_shop_by_id(ShopID, Context)),
    #domain_Shop{details = #domain_ShopDetails{name = ShopName}} = Shop,
    #domain_Shop{contract_id = ContractID} = Shop,
    Contract = maybe_result(capi_handler_utils:get_contract_by_id(ContractID, Context)),
    #domain_Contract{payment_institution = PiRef} = Contract,
    Pi = maybe_result(capi_domain:get_payment_institution_by_ref(PiRef, Context)),
    #domain_PaymentInstitution{realm = Realm} = Pi,
    RealmMode = genlib:to_binary(Realm),
    MerchantID = capi_handler_utils:wrap_merchant_id(RealmMode, ShopID),
    #{
        <<"merchantID">> => MerchantID,
        <<"merchantName">> => ShopName,
        <<"realm">> => RealmMode
    }.

mixin_token_provider_data(PaymentMethods, TokenProviderData) ->
    lists:map(
        fun
            (#{<<"method">> := <<"BankCard">>} = PaymentMethod) ->
                PaymentMethod#{<<"tokenProviderData">> => TokenProviderData};
            (PaymentMethod) ->
                PaymentMethod
        end,
        PaymentMethods
    ).
