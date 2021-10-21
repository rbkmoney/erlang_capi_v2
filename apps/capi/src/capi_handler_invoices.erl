-module(capi_handler_invoices).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2, map_service_result/1]).

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
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        try
            Allocation = maps:get(<<"allocation">>, InvoiceParams, undefined),
            ok = validate_allocation(Allocation),
            case create_invoice(PartyID, InvoiceParams, Context, OperationID) of
                {ok, #'payproc_Invoice'{invoice = Invoice}} ->
                    {ok, {201, #{}, capi_handler_decoder_invoicing:make_invoice_and_token(Invoice, Context)}};
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
                            {ok, logic_error(invoiceTermsViolated, <<"Invoice parameters violate contract terms">>)};
                        #payproc_AllocationNotAllowed{} ->
                            {ok, logic_error(allocationNotPermitted, <<"Not allowed">>)};
                        #payproc_AllocationExceededPaymentAmount{} ->
                            {ok, logic_error(invalidAllocation, <<"Exceeded payment amount">>)};
                        #payproc_AllocationInvalidTransaction{} = InvalidTransaction ->
                            Message = capi_allocation:transaction_error(InvalidTransaction),
                            {ok, logic_error(invalidAllocation, Message)}
                    end
            end
        catch
            throw:invoice_cart_empty ->
                {ok, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)};
            throw:invalid_invoice_cost ->
                {ok, logic_error(invalidInvoiceCost, <<"Invalid invoice amount">>)};
            throw:{external_id_conflict, InvoiceID, ExternalID, _Schema} ->
                {ok, logic_error(externalIDConflict, {InvoiceID, ExternalID})};
            throw:allocation_wrong_cart ->
                {ok, logic_error(invalidAllocation, <<"Wrong cart">>)};
            throw:allocation_duplicate ->
                {ok, logic_error(invalidAllocation, <<"Duplicate shop">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateInvoiceAccessToken' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    ResultInvoice = map_service_result(capi_handler_utils:get_invoice_by_id(InvoiceID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultInvoice, general_error(404, <<"Invoice not found">>)),
        Invoice = ResultInvoice#payproc_Invoice.invoice,
        Response = capi_handler_utils:issue_access_token(Invoice, Context),
        {ok, {201, #{}, Response}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoiceByID' = OperationID, Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    ResultInvoice = map_service_result(capi_handler_utils:get_invoice_by_id(InvoiceID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context)),
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
        Resolution = mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context)),
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
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
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
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
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
        Resolution = mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context)),
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
    ResultInvoice = map_service_result(capi_handler_utils:get_invoice_by_id(InvoiceID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => ResultInvoice}}
        ],
        Resolution = mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultInvoice, general_error(404, <<"Invoice not found">>)),
        PartyID = ResultInvoice#payproc_Invoice.invoice#domain_Invoice.owner_id,
        % В данном контексте - Party не может не существовать
        {ok, Party} = capi_party:get_party(PartyID, Context),
        Args = {InvoiceID, {revision, Party#domain_Party.revision}},
        case capi_handler_utils:get_payment_methods(invoicing, Args, Context) of
            {ok, PaymentMethodRefs} ->
                #payproc_Invoice{invoice = Invoice} = ResultInvoice,
                PaymentMethods0 = capi_handler_decoder_invoicing:decode_payment_methods(PaymentMethodRefs),
                PaymentMethods1 = capi_utils:deduplicate_payment_methods(PaymentMethods0),
                PaymentMethods = capi_handler_utils:emplace_token_provider_data(Invoice, PaymentMethods1, Context),
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

validate_allocation(Allocation) ->
    case capi_allocation:validate(Allocation) of
        ok -> ok;
        Error -> throw(Error)
    end.

create_invoice(PartyID, InvoiceParams, Context, BenderPrefix) ->
    #{woody_context := WoodyCtx} = Context,
    ExternalID = maps:get(<<"externalID">>, InvoiceParams, undefined),
    IdempotentKey = {BenderPrefix, PartyID, ExternalID},
    Identity = capi_bender:make_identity(invoice, InvoiceParams),
    InvoiceID = capi_bender:try_gen_snowflake(IdempotentKey, Identity, WoodyCtx),
    Call = {invoicing, 'Create', {encode_invoice_params(InvoiceID, PartyID, InvoiceParams)}},
    capi_handler_utils:service_call_with([user_info], Call, Context).

encode_invoice_params(ID, PartyID, InvoiceParams) ->
    Amount = genlib_map:get(<<"amount">>, InvoiceParams),
    Currency = genlib_map:get(<<"currency">>, InvoiceParams),
    Cart = genlib_map:get(<<"cart">>, InvoiceParams),
    ClientInfo = genlib_map:get(<<"clientInfo">>, InvoiceParams),
    Allocation = genlib_map:get(<<"allocation">>, InvoiceParams),
    #payproc_InvoiceParams{
        id = ID,
        party_id = PartyID,
        details = encode_invoice_details(InvoiceParams),
        cost = encode_invoice_cost(Amount, Currency, Cart),
        due = capi_handler_utils:get_time(<<"dueDate">>, InvoiceParams),
        context = capi_handler_encoder:encode_invoice_context(InvoiceParams),
        shop_id = genlib_map:get(<<"shopID">>, InvoiceParams),
        external_id = genlib_map:get(<<"externalID">>, InvoiceParams, undefined),
        client_info = encode_client_info(ClientInfo),
        allocation = capi_allocation:encode(Allocation, PartyID)
    }.

encode_client_info(undefined) ->
    undefined;
encode_client_info(#{<<"trustLevel">> := TrustLevel}) ->
    #domain_InvoiceClientInfo{
        trust_level = encode_trust_level(TrustLevel)
    }.

encode_trust_level(<<"unknown">>) ->
    unknown;
encode_trust_level(<<"wellKnown">>) ->
    well_known.

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

mask_invoice_notfound(Resolution) ->
    % ED-206
    % When bouncer says "forbidden" we can't really tell the difference between "forbidden because
    % of no such invoice", "forbidden because client has no access to it" and "forbidden because
    % client has no permission to act on it". From the point of view of existing integrations this
    % is not great, so we have to mask specific instances of missing authorization as if specified
    % invoice is nonexistent.
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Invoice not found">>)).
