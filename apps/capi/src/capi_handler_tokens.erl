-module(capi_handler_tokens).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_tool_provider_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [logic_error/2]).

-define(CAPI_NS, <<"com.rbkmoney.capi">>).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('CreatePaymentResource' = OperationID, Req, Context) ->
    Params = maps:get('PaymentResourceParams', Req),
    ClientInfo = enrich_client_info(maps:get(<<"clientInfo">>, Params), Context),

    try
        Data = maps:get(<<"paymentTool">>, Params), % "V" ????
        PartyID = capi_handler_utils:get_party_id(Context),
        ExternalID = maps:get(<<"externalID">>, Params, undefined),
        IdempotentKey = capi_bender:get_idempotent_key(OperationID, PartyID, ExternalID),
        IdempotentParams = {ExternalID, IdempotentKey},
        {PaymentTool, PaymentSessionID} =
            case Data of
                #{<<"paymentToolType">> := <<"CardData"           >>} ->
                    process_card_data(Data, IdempotentParams, Context);
                #{<<"paymentToolType">> := <<"PaymentTerminalData">>} ->
                    process_payment_terminal_data(Data);
                #{<<"paymentToolType">> := <<"DigitalWalletData"  >>} ->
                    process_digital_wallet_data(Data);
                #{<<"paymentToolType">> := <<"TokenizedCardData"  >>} ->
                    process_tokenized_card_data(Data, IdempotentParams, Context);
                #{<<"paymentToolType">> := <<"CryptoWalletData"   >>} ->
                    process_crypto_wallet_data(Data)
            end,
        PaymentResource =
            #domain_DisposablePaymentResource{
                payment_tool = PaymentTool,
                payment_session_id = PaymentSessionID,
                client_info = capi_handler_encoder:encode_client_info(ClientInfo)
            },
        {ok, {201, #{}, capi_handler_decoder_party:decode_disposable_payment_resource(PaymentResource)}}
    catch
        Result -> Result
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

enrich_client_info(ClientInfo, Context) ->
    Claims = capi_handler_utils:get_auth_context(Context),
    IP = case capi_auth:get_claim(<<"ip_replacement_allowed">>, Claims, false) of
        true ->
            UncheckedIP = maps:get(<<"ip">>, ClientInfo, prepare_client_ip(Context)),
            validate_ip(UncheckedIP);
        false ->
            prepare_client_ip(Context);
        Value ->
            _ = logger:notice("Unexpected ip_replacement_allowed value: ~p", [Value]),
            prepare_client_ip(Context)
    end,
    ClientInfo#{<<"ip">> => IP}.

validate_ip(IP) ->
    % placeholder so far.
    % we could want to check whether client's ip is valid and corresponds IPv4 inside IPv6 standart
    IP.

prepare_client_ip(Context) ->
    #{ip_address := IP} = get_peer_info(Context),
    genlib:to_binary(inet:ntoa(IP)).

get_peer_info(#{swagger_context := #{peer := Peer}}) ->
    Peer.

process_card_data(Data, IdempotentParams, Context) ->
    SessionData = encode_session_data(Data),
    CardData = encode_card_data(Data),
    put_card_data_to_cds(CardData, SessionData, IdempotentParams, Context).

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
            % dirty hack for cds support empty cvv bank cards
            value = maps:get(<<"cvv">>, CardData, <<"">>)
        }}
    }.

put_card_to_cds(CardData, SessionData, Context) ->
    BinData = lookup_bank_info(CardData#'CardData'.pan, Context),
    Call = {cds_storage, 'PutCard', [CardData]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, #'PutCardResult'{bank_card = BankCard}} ->
            {bank_card, expand_card_info(
                BankCard,
                BinData,
                undef_cvv(SessionData)
            )};
        {exception, #'InvalidCardData'{}} ->
            throw({ok, logic_error(invalidRequest, <<"Card data is invalid">>)})
    end.

put_session_to_cds(SessionID, SessionData, Context) ->
    Call = {cds_storage, 'PutSession', [SessionID, SessionData]},
    {ok, ok} = capi_handler_utils:service_call(Call, Context),
    ok.

put_card_data_to_cds(CardData, SessionData, {ExternalID, IdempotentKey}, Context) ->
    #{woody_context := WoodyCtx} = Context,
    BankCard = put_card_to_cds(CardData, SessionData, Context),
    {bank_card, #domain_BankCard{token = Token}} = BankCard,
    RandomID = gen_random_id(),
    Hash = erlang:phash2(Token),
    case capi_bender:gen_by_constant(IdempotentKey, RandomID, Hash, WoodyCtx) of
        {ok, SessionID} ->
            ok = put_session_to_cds(SessionID, SessionData, Context),
            {BankCard, SessionID};
        {error, {external_id_conflict, _}} ->
            throw({ok, logic_error(externalIDConflict, ExternalID)})
    end.

lookup_bank_info(Pan, Context) ->
    RequestVersion = {'last', #binbase_Last{}},
    Call = {binbase, 'Lookup', [Pan, RequestVersion]},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, #'binbase_ResponseData'{bin_data = BinData, version = Version}} ->
            {BinData, Version};
        {exception, #'binbase_BinNotFound'{}} ->
            throw({ok, logic_error(invalidRequest, <<"Card data is invalid">>)})
    end.

undef_cvv(#'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = <<"">>
        }}
    }) ->
    true;
undef_cvv(#'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = _Value
        }}
    }) ->
    false;
undef_cvv(#'SessionData'{}) ->
    undefined.

expand_card_info(BankCard, {BinData, Version}, HaveCVV) ->
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
            },
            is_cvv_empty = HaveCVV
        }
    catch
        throw:{encode_binbase_payment_system, invalid_payment_system} ->
            throw({ok, logic_error(invalidRequest, <<"Unsupported card">>)});
        throw:{encode_residence, invalid_residence} ->
            throw({ok, logic_error(invalidRequest, <<"Unsupported card">>)})
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

process_crypto_wallet_data(Data) ->
    #{<<"cryptoCurrency">> := CryptoCurrency} = Data,
    {{crypto_currency, capi_handler_decoder_utils:convert_crypto_currency_from_swag(CryptoCurrency)}, <<>>}.

process_tokenized_card_data(Data, IdempotentParams, Context) ->
    Call = {get_token_provider_service_name(Data), 'Unwrap', [encode_wrapped_payment_tool(Data)]},
    UnwrappedPaymentTool = case capi_handler_utils:service_call(Call, Context) of
        {ok, Tool} ->
            Tool;
        {exception, #'InvalidRequest'{}} ->
            throw({ok, logic_error(invalidRequest, <<"Tokenized card data is invalid">>)})
    end,
    process_put_card_data_result(
        put_card_data_to_cds(
            encode_tokenized_card_data(UnwrappedPaymentTool),
            encode_tokenized_session_data(UnwrappedPaymentTool),
            IdempotentParams,
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
            token_provider = get_payment_token_provider(PaymentDetails, PaymentData),
            %% Не учитываем наличие cvv для токенизированных карт, даже если проводим их как обычные.
            is_cvv_empty   = undefined
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

gen_random_id() ->
    Random = crypto:strong_rand_bytes(16),
    genlib_format:format_int_base(binary:decode_unsigned(Random), 62).
