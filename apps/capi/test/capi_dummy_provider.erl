-module(capi_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(capi_test_proxy).

-export([get_service_spec/0]).

%%

-define(REC_TOKEN, <<"rec_token">>).

-spec get_service_spec() ->
    {Path :: string(), Service :: {module(), atom()}}.

get_service_spec() ->
    {"/test/proxy/provider/dummy", {dmsl_proxy_provider_thrift, 'ProviderProxy'}}.

%%

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), #{}) ->
    {ok, term()}.

handle_function(
    'ProcessPayment',
    [#prxprv_PaymentContext{
        session = #prxprv_Session{target = Target, state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    _Context,
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts);

handle_function(
    'GenerateToken',
    [#prxprv_RecurrentTokenContext{
        session = #prxprv_RecurrentTokenSession{state = _State},
        token_info = TokenInfo,
        options = _
    }],
    _Context,
    _Opts
) ->
    token_finish(TokenInfo, ?REC_TOKEN);

handle_function(
    'HandleRecurrentTokenCallback',
    [_Payload, #prxprv_RecurrentTokenContext{
        session = #prxprv_RecurrentTokenSession{state = _State},
        token_info = TokenInfo,
        options = _
    }],
    _Context,
    _Opts
) ->
    {ok, token_respond(?REC_TOKEN, token_finish(TokenInfo, ?REC_TOKEN))};

handle_function(
    'HandlePaymentCallback',
    [_Payload, #prxprv_PaymentContext{
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
    #prxprv_PaymentProxyResult{
        intent = {finish, #'FinishIntent'{status = {success, #'Success'{}}}},
        trx    = #domain_TransactionInfo{id = Payment#prxprv_InvoicePayment.id, extra = #{}}
    }.

token_finish(#prxprv_RecurrentTokenInfo{payment_tool = PaymentTool}, Token) ->
    #prxprv_RecurrentTokenProxyResult{
        intent = {finish, #'prxprv_RecurrentTokenFinishIntent'{
            status = {success, #'prxprv_RecurrentTokenSuccess'{token = Token}}
        }},
        token  = Token,
        trx    = #domain_TransactionInfo{id = PaymentTool#prxprv_RecurrentPaymentTool.id, extra = #{}}
    }.

respond(Response, Result) ->
    #prxprv_PaymentCallbackResult{
        response = Response,
        result = Result
    }.

token_respond(Response, CallbackResult) ->
    #prxprv_RecurrentTokenCallbackResult{
        response   = Response,
        result     = CallbackResult
    }.
