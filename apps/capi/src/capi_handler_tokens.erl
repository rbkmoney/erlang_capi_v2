-module(capi_handler_tokens).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_tool_provider_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).

-define(CAPI_NS, <<"com.rbkmoney.capi">>).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('CreatePaymentResource', Req, Context) ->
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
                client_info = capi_handler_encoder:encode_client_info(ClientInfo)
            },
        {ok, {201, [], capi_handler_decoder_party:decode_disposable_payment_resource(PaymentResource)}}
    catch
        Result -> Result
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

enrich_client_info(ClientInfo, Context) ->
    ClientInfo#{<<"ip">> => prepare_client_ip(Context)}.

prepare_client_ip(Context) ->
    #{ip_address := IP} = get_peer_info(Context),
    genlib:to_binary(inet:ntoa(IP)).

get_peer_info(#{swagger_context := #{peer := Peer}}) ->
    Peer.

process_card_data(Data, Context) ->
    put_card_data_to_cds(
        encode_card_data(Data),
        encode_session_data(Data),
        Context
    ).

encode_card_data(CardData) ->
    {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, CardData)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, CardData)),
    #'CardData'{
        pan  = CardNumber,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = genlib_map:get(<<"cardHolder">>, CardData)
    }.

parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year = case genlib:to_int(Year0) of
        Y when Y < 100 ->
            2000 + Y;
        Y ->
            Y
    end,
    {genlib:to_int(Month), Year}.

encode_session_data(CardData) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = genlib_map:get(<<"cvv">>, CardData)
        }}
    }.

put_card_data_to_cds(CardData, SessionData, Context) ->
    BinData = lookup_bank_info(CardData#'CardData'.pan, Context),
    Call = {cds_storage, 'PutCardData', [CardData, SessionData]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, #'PutCardDataResult'{session_id = SessionID, bank_card = BankCard}} ->
            {{bank_card, expand_card_info(BankCard, BinData)}, SessionID};
        {exception, Exception} ->
            case Exception of
                #'InvalidCardData'{} ->
                    throw({ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Card data is invalid">>)}});
                #'KeyringLocked'{} ->
                    % TODO
                    % It's better for the cds to signal woody-level unavailability when the
                    % keyring is locked, isn't it? It could always mention keyring lock as a
                    % reason in a woody error definition.
                    throw({error, capi_handler_utils:server_error(503)})
            end
    end.

lookup_bank_info(Pan, Context) ->
    RequestVersion = {'last', #binbase_Last{}},
    Call = {binbase, 'Lookup', [Pan, RequestVersion]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, #'binbase_ResponseData'{bin_data = BinData, version = Version}} ->
            {BinData, Version};
        {exception, #'binbase_BinNotFound'{}} ->
            throw({ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Card data is invalid">>)}})
    end.

expand_card_info(BankCard, {BinData, Version}) ->
    try
        BankCard#'domain_BankCard'{
            payment_system = encode_binbase_payment_system(BinData#'binbase_BinData'.payment_system),
            issuer_country = capi_handler_encoder:encode_residence(BinData#'binbase_BinData'.iso_country_code),
            bank_name = BinData#'binbase_BinData'.bank_name,
            metadata = #{
                ?CAPI_NS =>
                    {obj, #{
                        {str, <<"version">>} => {i, Version}
                    }
                }
            }
        }
    catch
        throw:{encode_binbase_payment_system, invalid_payment_system} ->
            throw({ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Unsupported card">>)}});
        throw:{encode_residence, invalid_residence} ->
            throw({ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Unsupported card">>)}})
    end.

encode_binbase_payment_system(<<"VISA">>)                      -> visa;
encode_binbase_payment_system(<<"VISA/DANKORT">>)              -> visa;         % supposedly 🤔
encode_binbase_payment_system(<<"MASTERCARD">>)                -> mastercard;
% encode_binbase_payment_system(<<"???">>)                       -> visaelectron;
encode_binbase_payment_system(<<"MAESTRO">>)                   -> maestro;
% encode_binbase_payment_system(<<"???">>)                       -> forbrugsforeningen;
encode_binbase_payment_system(<<"DANKORT">>)                   -> dankort;
encode_binbase_payment_system(<<"AMERICAN EXPRESS">>)          -> amex;
encode_binbase_payment_system(<<"DINERS CLUB INTERNATIONAL">>) -> dinersclub;
encode_binbase_payment_system(<<"DISCOVER">>)                  -> discover;
encode_binbase_payment_system(<<"UNIONPAY">>)                  -> unionpay;
encode_binbase_payment_system(<<"JCB">>)                       -> jcb;
encode_binbase_payment_system(<<"NSPK MIR">>)                  -> nspkmir;
encode_binbase_payment_system(_) ->
    throw({encode_binbase_payment_system, invalid_payment_system}).

process_payment_terminal_data(Data) ->
    PaymentTerminal =
        #domain_PaymentTerminal{
            terminal_type = binary_to_existing_atom(genlib_map:get(<<"provider">>, Data), utf8)
        },
    {{payment_terminal, PaymentTerminal}, <<>>}.

process_digital_wallet_data(Data) ->
    DigitalWallet = case Data of
        #{<<"digitalWalletType">> := <<"DigitalWalletQIWI">>} ->
            #domain_DigitalWallet{
                provider = qiwi,
                id       = maps:get(<<"phoneNumber">>, Data)
            }
    end,
    {{digital_wallet, DigitalWallet}, <<>>}.

process_tokenized_card_data(Data, Context) ->
    Call = {get_token_provider_service_name(Data), 'Unwrap', [encode_wrapped_payment_tool(Data)]},
    UnwrappedPaymentTool = case capi_handler_utils:service_call(Call, Context) of
        {ok, Tool} ->
            Tool;
        {exception, #'InvalidRequest'{}} ->
            throw({ok, {400, [], capi_handler_utils:logic_error(invalidRequest, <<"Tokenized card data is invalid">>)}})
    end,
    process_put_card_data_result(
        put_card_data_to_cds(
            encode_tokenized_card_data(UnwrappedPaymentTool),
            encode_tokenized_session_data(UnwrappedPaymentTool),
            Context
        ),
        UnwrappedPaymentTool
    ).

get_token_provider_service_name(Data) ->
    case Data of
        #{<<"provider">> := <<"ApplePay">>} ->
            payment_tool_provider_apple_pay;
        #{<<"provider">> := <<"GooglePay">>} ->
            payment_tool_provider_google_pay;
        #{<<"provider">> := <<"SamsungPay">>} ->
            payment_tool_provider_samsung_pay
    end.

process_put_card_data_result(
    {{bank_card, BankCard}, SessionID},
    #paytoolprv_UnwrappedPaymentTool{
        card_info = #paytoolprv_CardInfo{
            payment_system = PaymentSystem,
            last_4_digits  = Last4
        },
        payment_data = PaymentData,
        details = PaymentDetails
    }
) ->
    {
        {bank_card, BankCard#domain_BankCard{
            payment_system = PaymentSystem,
            masked_pan     = capi_utils:define(Last4, BankCard#domain_BankCard.masked_pan),
            token_provider = get_payment_token_provider(PaymentDetails, PaymentData)
        }},
        SessionID
    }.

get_payment_token_provider(_PaymentDetails, {card, _}) ->
    % TODO
    % We deliberately hide the fact that we've got that payment tool from the likes of Google Chrome browser
    % in order to make our internal services think of it as if it was good ol' plain bank card. Without a
    % CVV though. A better solution would be to distinguish between a _token provider_ and an _origin_.
    undefined;

get_payment_token_provider({apple, _}, _PaymentData) ->
    applepay;
get_payment_token_provider({google, _}, _PaymentData) ->
    googlepay;
get_payment_token_provider({samsung, _}, _PaymentData) ->
    samsungpay.

encode_wrapped_payment_tool(Data) ->
    #paytoolprv_WrappedPaymentTool{
        request = encode_payment_request(Data)
    }.

encode_payment_request(#{<<"provider" >> := <<"ApplePay">>} = Data) ->
    {apple, #paytoolprv_ApplePayRequest{
        merchant_id = maps:get(<<"merchantID">>, Data),
        payment_token = capi_handler_encoder:encode_content(json, maps:get(<<"paymentToken">>, Data))
    }};
encode_payment_request(#{<<"provider" >> := <<"GooglePay">>} = Data) ->
    {google, #paytoolprv_GooglePayRequest{
        gateway_merchant_id = maps:get(<<"gatewayMerchantID">>, Data),
        payment_token = capi_handler_encoder:encode_content(json, maps:get(<<"paymentToken">>, Data))
    }};
encode_payment_request(#{<<"provider" >> := <<"SamsungPay">>} = Data) ->
    {samsung, #paytoolprv_SamsungPayRequest{
        service_id = genlib_map:get(<<"serviceID">>, Data),
        reference_id = genlib_map:get(<<"referenceID">>, Data)
    }}.

encode_tokenized_card_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {tokenized_card, #paytoolprv_TokenizedCard{
        dpan = DPAN,
        exp_date = #paytoolprv_ExpDate{
            month = Month,
            year = Year
        }
    }},
    card_info = #paytoolprv_CardInfo{
        cardholder_name = CardholderName
    }
}) ->
    #'CardData'{
        pan  = DPAN,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName
    };
encode_tokenized_card_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {card, #paytoolprv_Card{
        pan = PAN,
        exp_date = #paytoolprv_ExpDate{
            month = Month,
            year = Year
        }
    }},
    card_info = #paytoolprv_CardInfo{
        cardholder_name = CardholderName
    }
}) ->
    #'CardData'{
        pan  = PAN,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName
    }.

encode_tokenized_session_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {tokenized_card, #paytoolprv_TokenizedCard{
        auth_data = {auth_3ds, #paytoolprv_Auth3DS{
            cryptogram = Cryptogram,
            eci = ECI
        }}
    }}
}) ->
    #'SessionData'{
        auth_data = {auth_3ds, #'Auth3DS'{
            cryptogram = Cryptogram,
            eci = ECI
        }}
    };
encode_tokenized_session_data(#paytoolprv_UnwrappedPaymentTool{
    payment_data = {card, #paytoolprv_Card{}}
}) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            %% TODO dirty hack for test GooglePay card data
            value = <<"">>
        }}
    }.
