-module(capi_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(capi_test_proxy).

-export([get_service_spec/0]).

%%

-spec get_service_spec() ->
    {Path :: string(), Service :: {module(), atom()}}.

get_service_spec() ->
    {"/test/proxy/provider/dummy", {cp_proxy_provider_thrift, 'ProviderProxy'}}.

%%

-include_lib("cp_proto/include/cp_proxy_provider_thrift.hrl").

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), #{}) ->
    {ok, term()}.

handle_function(
    'ProcessPayment',
    [#prxprv_Context{
        session = #prxprv_Session{target = Target, state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    _Context,
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts);

handle_function(
    'HandlePaymentCallback',
    [_Payload, #prxprv_Context{
        session = #prxprv_Session{target = _Target, state = _State},
        payment_info = PaymentInfo,
        options = _
    }],
    _Context,
    _Opts
) ->
    {ok, respond(<<"sure">>, finish(PaymentInfo))}.

process_payment({captured, #domain_InvoicePaymentCaptured{}}, _, PaymentInfo, _) ->
    {ok, finish(PaymentInfo)};

process_payment({cancelled, #domain_InvoicePaymentCancelled{}}, _, PaymentInfo, _) ->
    {ok, finish(PaymentInfo)};

process_payment({refunded, #domain_InvoicePaymentRefunded{}}, _, PaymentInfo, _) ->
    {ok, finish(PaymentInfo)};

process_payment({processed, #domain_InvoicePaymentProcessed{}}, _, PaymentInfo, _) ->
    {ok, finish(PaymentInfo)}.

finish(#prxprv_PaymentInfo{payment = Payment}) ->
    #prxprv_ProxyResult{
        intent = {finish, #'FinishIntent'{status = {success, #'Success'{}}}},
        trx    = #domain_TransactionInfo{id = Payment#prxprv_InvoicePayment.id, extra = #{}}
    }.

respond(Response, Result) ->
    #prxprv_CallbackResult{
        response = Response,
        result = Result
    }.
