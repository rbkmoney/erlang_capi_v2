-module(capi_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").
-include_lib("payout_manager_proto/include/payouts_payout_manager_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().
-type payment_tool_context() :: bouncer_context_v1_thrift:'ContextPaymentTool'().
-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([payment_tool_context/0]).
-export_type([fragments/0]).

-type prototypes() :: [
    {operation, prototype_operation()}
    | {payproc, prototype_payproc()}
    | {payouts, prototype_payouts()}
    | {reports, prototype_reports()}
    | {webhooks, prototype_webhooks()}
    | {payment_tool, prototype_payment_tool()}
].

-type prototype_operation() :: #{
    id => swag_server:operation_id(),
    party => entity_id(),
    shop => entity_id(),
    contract => entity_id(),
    invoice => entity_id(),
    payment => entity_id(),
    refund => entity_id(),
    invoice_template => entity_id(),
    customer => entity_id(),
    binding => entity_id(),
    report => report_id(),
    file => entity_id(),
    webhook => entity_id(),
    claim => entity_id(),
    payout => entity_id()
}.

-type prototype_payment_tool() :: #{
    payment_tool_context => payment_tool_context()
}.

-type prototype_payproc() :: #{
    invoice => invoice_id() | invoice() | undefined,
    invoice_template => invoice_template_id() | invoice_template() | undefined,
    customer => customer_id() | customer() | undefined
}.

-type prototype_payouts() :: #{
    payout => payout_id() | payout() | undefined,
    contract => entity_id()
}.

-type prototype_webhooks() :: #{
    webhook => webhook_id() | webhook() | undefined
}.

-type prototype_reports() :: #{
    report => report_id() | report() | undefined
}.

-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type invoice() :: dmsl_domain_thrift:'Invoice'().

-type invoice_template_id() :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type invoice_template() :: dmsl_domain_thrift:'InvoiceTemplate'().

-type customer_id() :: dmsl_payment_processing_thrift:'CustomerID'().
-type customer() :: dmsl_payment_processing_thrift:'Customer'().

-type webhook_id() :: dmsl_webhooker_thrift:'WebhookID'().
-type webhook() :: dmsl_webhooker_thrift:'Webhook'().

-type report_id() :: reporter_reports_thrift:'ReportID'().
-type report() :: reporter_reports_thrift:'Report'().

-type payout_id() :: payouts_payout_manager_thrift:'PayoutID'().
-type payout() :: payouts_payout_manager_thrift:'Payout'().

-type entity_id() :: binary().

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).
-export_type([prototype_payproc/0]).
-export_type([prototype_payouts/0]).
-export_type([prototype_webhooks/0]).
-export_type([prototype_reports/0]).
-export_type([prototype_payment_tool/0]).

-export([new/0]).
-export([build/3]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(capi, deployment, undefined)}
    }).

-spec build(prototypes(), fragments(), woody_context:ctx()) -> fragments().
build(Prototypes, {Acc0, External}, WoodyCtx) ->
    Acc1 = lists:foldl(fun({T, Params}, Acc) -> build(T, Params, Acc, WoodyCtx) end, Acc0, Prototypes),
    {Acc1, External}.

build(operation, Params = #{id := OperationID}, Acc, _WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        capi = #bctx_v1_ContextCommonAPI{
            op = #bctx_v1_CommonAPIOperation{
                id = operation_id_to_binary(OperationID),
                party = maybe_entity(party, Params),
                shop = maybe_entity(shop, Params),
                contract = maybe_entity(contract, Params),
                invoice = maybe_entity(invoice, Params),
                payment = maybe_entity(payment, Params),
                refund = maybe_entity(refund, Params),
                invoice_template = maybe_entity(invoice_template, Params),
                customer = maybe_entity(customer, Params),
                binding = maybe_entity(binding, Params),
                report = maybe_entity(report, Params),
                file = maybe_entity(file, Params),
                webhook = maybe_entity(webhook, Params),
                claim = maybe_entity(claim, Params),
                payout = maybe_entity(payout, Params)
            }
        }
    };
build(payment_tool, Params = #{}, Acc, _WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        payment_tool = maps:get(payment_tool_context, Params, undefined)
    };
build(payproc, Params = #{}, Acc, WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        payment_processing = #bctx_v1_ContextPaymentProcessing{
            invoice = maybe_with(
                invoice,
                Params,
                fun(V) -> build_invoice_ctx(V, WoodyCtx) end
            ),
            invoice_template = maybe_with(
                invoice_template,
                Params,
                fun(V) -> build_invoice_template_ctx(V, WoodyCtx) end
            ),
            customer = maybe_with(
                customer,
                Params,
                fun(V) -> build_customer_ctx(V, WoodyCtx) end
            )
        }
    };
build(webhooks, Params = #{}, Acc, WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        webhooks = #bctx_v1_ContextWebhooks{
            webhook = maybe_with(
                webhook,
                Params,
                fun(V) -> build_webhook_ctx(V, WoodyCtx) end
            )
        }
    };
build(reports, Params = #{}, Acc, WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        reports = #bctx_v1_ContextReports{
            report = maybe_with(
                report,
                Params,
                fun(V) -> build_report_ctx(V, WoodyCtx) end
            )
        }
    };
build(payouts, Params = #{}, Acc, WoodyCtx) ->
    Payout0 = maybe_with(payout, Params, fun(V) ->
        build_payout_ctx(V, WoodyCtx)
    end),
    Payout = maybe(Payout0, fun(_Payout0) ->
        Payout0#bctx_v1_Payout{
            contract = maybe_entity(contract, Params)
        }
    end),
    Acc#bctx_v1_ContextFragment{
        payouts = #bctx_v1_ContextPayouts{
            payout = Payout
        }
    }.

%%

build_invoice_ctx(ID, WoodyCtx) when is_binary(ID) ->
    maybe_with_woody_result(
        invoicing,
        'Get',
        {_UserInfo = undefined, ID, #payproc_EventRange{}},
        WoodyCtx,
        fun build_invoice_ctx/1
    );
build_invoice_ctx(Invoice, _WoodyCtx) ->
    build_invoice_ctx(Invoice).

build_invoice_ctx(#payproc_Invoice{invoice = Invoice, payments = Payments}) ->
    #bctx_v1_Invoice{
        id = Invoice#domain_Invoice.id,
        party = build_entity(Invoice#domain_Invoice.owner_id),
        shop = build_entity(Invoice#domain_Invoice.shop_id),
        payments = build_set(lists:map(fun build_payment_ctx/1, Payments))
    }.

build_payment_ctx(#payproc_InvoicePayment{payment = Payment, refunds = Refunds}) ->
    #bctx_v1_Payment{
        id = Payment#domain_InvoicePayment.id,
        refunds = build_set(lists:map(fun build_refund_ctx/1, Refunds))
    }.

build_refund_ctx(#payproc_InvoicePaymentRefund{refund = Refund}) ->
    build_entity(Refund#domain_InvoicePaymentRefund.id).

build_invoice_template_ctx(ID, WoodyCtx) when is_binary(ID) ->
    maybe_with_woody_result(
        invoice_templating,
        'Get',
        {_UserInfo = undefined, ID},
        WoodyCtx,
        fun build_invoice_template_ctx/1
    );
build_invoice_template_ctx(InvoiceTemplate, _WoodyCtx) ->
    build_invoice_template_ctx(InvoiceTemplate).

build_invoice_template_ctx(#domain_InvoiceTemplate{id = ID, owner_id = OwnerID, shop_id = ShopID}) ->
    #bctx_v1_InvoiceTemplate{
        id = ID,
        party = build_entity(OwnerID),
        shop = build_entity(ShopID)
    }.

build_customer_ctx(ID, WoodyCtx) when is_binary(ID) ->
    maybe_with_woody_result(
        customer_management,
        'Get',
        {ID, #payproc_EventRange{}},
        WoodyCtx,
        fun build_customer_ctx/1
    );
build_customer_ctx(Customer, _WoodyCtx) ->
    build_customer_ctx(Customer).

build_customer_ctx(#payproc_Customer{id = ID, owner_id = OwnerID, shop_id = ShopID, bindings = Bindings}) ->
    #bctx_v1_Customer{
        id = ID,
        party = build_entity(OwnerID),
        shop = build_entity(ShopID),
        bindings = build_set(lists:map(fun build_binding_ctx/1, Bindings))
    }.

build_binding_ctx(#payproc_CustomerBinding{id = ID}) ->
    build_entity(ID).

%%

build_payout_ctx(ID, WoodyCtx) when is_binary(ID) ->
    maybe_with_woody_result(payouts, 'GetPayout', {ID}, WoodyCtx, fun build_payout_ctx/1);
build_payout_ctx(Payout, _WoodyCtx) ->
    build_payout_ctx(Payout).

build_payout_ctx(#payouts_Payout{
    payout_id = ID,
    party_id = PartyID,
    shop_id = ShopID
}) ->
    #bctx_v1_Payout{
        id = ID,
        party = build_entity(PartyID),
        shop = build_entity(ShopID)
    }.

%%

build_webhook_ctx(ID, WoodyCtx) when is_integer(ID) ->
    maybe_with_woody_result(webhook_manager, 'Get', {ID}, WoodyCtx, fun build_webhook_ctx/1);
build_webhook_ctx(Webhook, _WoodyCtx) ->
    build_webhook_ctx(Webhook).

build_webhook_ctx(#webhooker_Webhook{id = ID, party_id = PartyID, event_filter = Filter}) ->
    #bctx_v1_Webhook{
        id = integer_to_binary(ID),
        party = build_entity(PartyID),
        filter = build_webhook_filter(Filter)
    }.

build_webhook_filter({Type, Filter}) ->
    build_webhook_filter_details(
        Filter,
        #bctx_v1_WebhookFilter{topic = erlang:atom_to_binary(Type, utf8)}
    ).

build_webhook_filter_details(#webhooker_PartyEventFilter{}, Ctx) ->
    Ctx;
build_webhook_filter_details(#webhooker_InvoiceEventFilter{shop_id = ShopID}, Ctx) ->
    Ctx#bctx_v1_WebhookFilter{shop = maybe(ShopID, fun build_entity/1)};
build_webhook_filter_details(#webhooker_CustomerEventFilter{shop_id = ShopID}, Ctx) ->
    Ctx#bctx_v1_WebhookFilter{shop = maybe(ShopID, fun build_entity/1)};
build_webhook_filter_details(#webhooker_WalletEventFilter{}, Ctx) ->
    Ctx.

%%

build_report_ctx(ID, WoodyCtx) when is_integer(ID) ->
    maybe_with_woody_result(reporting, 'GetReport', {ID}, WoodyCtx, fun build_report_ctx/1);
build_report_ctx(Report, _WoodyCtx) ->
    build_report_ctx(Report).

build_report_ctx(#reports_Report{report_id = ID, party_id = PartyID, shop_id = ShopID, files = Files}) ->
    #bctx_v1_Report{
        id = integer_to_binary(ID),
        party = build_entity(PartyID),
        shop = maybe(ShopID, fun build_entity/1),
        files = build_set(lists:map(fun build_report_file_ctx/1, Files))
    }.

build_report_file_ctx(#reports_FileMeta{file_id = ID}) ->
    build_entity(ID).

%%

maybe(undefined, _Then) ->
    undefined;
maybe(V, Then) ->
    Then(V).

maybe_with(Name, Params, Then) ->
    maybe(maps:get(Name, Params, undefined), Then).

maybe_with_woody_result(ServiceName, Function, Args, WoodyCtx, Then) ->
    % TODO
    % - Probably need to share this code w/ handlers.
    % - This caches nicely, and we probably should enable it to combat increased latency.
    case capi_woody_client:call_service(ServiceName, Function, Args, WoodyCtx) of
        {ok, Result} ->
            Then(Result);
        {exception, _} ->
            undefined
    end.

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).

maybe_entity(Name, Params) ->
    maybe_with(Name, Params, fun build_entity/1).

build_entity(ID) when is_binary(ID) ->
    #bouncer_base_Entity{id = ID};
build_entity(ID) when is_integer(ID) ->
    #bouncer_base_Entity{id = integer_to_binary(ID)}.

build_set(L) when is_list(L) ->
    ordsets:from_list(L).
