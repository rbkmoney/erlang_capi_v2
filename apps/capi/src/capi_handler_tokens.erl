-module(capi_handler_tokens).

-behaviour(capi_handler).
-export([process_request/4]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context(),
    Handlers    :: list(module())
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('CreatePaymentResource', Req, Context, _) ->
    Params = maps:get('PaymentResourceParams', Req),
    ClientInfo = enrich_client_info(maps:get(<<"clientInfo">>, Params), Context),
    try
        Data = maps:get(<<"paymentTool">>, Params), % "V" ????
        {PaymentTool, PaymentSessionID} =
            case Data of
                #{<<"paymentToolType">> := <<"CardData"           >>} -> process_card_data(Data, Context);
                #{<<"paymentToolType">> := <<"PaymentTerminalData">>} -> process_payment_terminal_data(Data);
                #{<<"paymentToolType">> := <<"DigitalWalletData"  >>} -> process_digital_wallet_data(Data);
                #{<<"paymentToolType">> := <<"TokenizedCardData"  >>} -> process_tokenized_card_data(Data, Context)
            end,
        PaymentResource =
            #domain_DisposablePaymentResource{
                payment_tool = PaymentTool,
                payment_session_id = PaymentSessionID,
                client_info = encode_client_info(ClientInfo)
            },
        {ok, {201, [], decode_disposable_payment_resource(PaymentResource)}}
    catch
        Result -> Result
    end;

%%

process_request(OperationID, Req, Context, Handlers) ->
    capi_handler:process_request(OperationID, Req, Context, Handlers).
