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

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), #{}) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function(
    'ProcessPayment',
    {#prxprv_Context{
        session = #prxprv_Session{target = Target, state = State},
        payment = PaymentInfo,
        options = _
    }},
    Context,
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts, Context);

handle_function(
    'HandlePaymentCallback',
    {_Payload, #prxprv_Context{
        session = #prxprv_Session{target = _Target, state = _State},
        payment = PaymentInfo,
        options = _
    }},
    Context,
    _Opts
) ->
    {{ok, respond(<<"sure">>, finish(PaymentInfo))}, Context}.

process_payment({captured, #domain_InvoicePaymentCaptured{}}, _, PaymentInfo, _, Context) ->
    {{ok, finish(PaymentInfo)}, Context};

process_payment({processed, #domain_InvoicePaymentProcessed{}}, _, PaymentInfo, _, Context) ->
    {{ok, finish(PaymentInfo)}, Context}.

finish(#prxprv_PaymentInfo{payment = Payment}) ->
    #prxprv_ProxyResult{
        intent = {finish, #'FinishIntent'{status = {ok, #'Ok'{}}}},
        trx    = #domain_TransactionInfo{id = Payment#prxprv_InvoicePayment.id}
    }.

respond(Response, Result) ->
    #prxprv_CallbackResult{
        response = Response,
        result = Result
    }.
