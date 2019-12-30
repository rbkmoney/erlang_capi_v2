-module(capi_crypto).

-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").

-type encrypted_token()     :: binary().
-type payment_tool()        :: dmsl_domain_thrift:'PaymentTool'().
-type payment_tool_token()  :: dmsl_payment_tool_token_thrift:'PaymentToolToken'().

-export([create_encrypted_payment_tool_token/2]).
-export([decrypt_payment_tool_token/1]).

-spec create_encrypted_payment_tool_token(binary(), payment_tool()) ->
    encrypted_token().

create_encrypted_payment_tool_token(IdempotentKey, PaymentTool) ->
    PaymentToolToken = encode_payment_tool_token(PaymentTool),
    EncryptionParams = create_encryption_params(IdempotentKey),
    ThriftType = {struct, union, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
    {ok, EncodedToken} = lechiffre:encode(ThriftType, PaymentToolToken, EncryptionParams),
    TokenVersion = payment_tool_token_version(),
    <<TokenVersion/binary, "/", EncodedToken/binary>>.

-spec decrypt_payment_tool_token(binary()) ->
    capi_handler_encoder:encode_data().

decrypt_payment_tool_token(Token) ->
    Ver = payment_tool_token_version(),
    Size = byte_size(Ver),
    case Token of
        <<Ver:Size/binary, "/", EncryptedToken/binary>> ->
            ThriftType = {struct, union, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
            case lechiffre:decode(ThriftType, EncryptedToken) of
                {ok, {bank_card_payload, Payload}} ->
                   {bank_card, Payload#ptt_BankCardPayload.bank_card};
                {ok, {payment_terminal_payload, Payload}} ->
                   {payment_terminal, Payload#ptt_PaymentTerminalPayload.payment_terminal};
                {ok, {digital_wallet_payload, Payload}} ->
                   {digital_wallet, Payload#ptt_DigitalWalletPayload.digital_wallet};
                {ok, {crypto_currency_payload, Payload}} ->
                   {crypto_currency, Payload#ptt_CryptoCurrencyPayload.crypto_currency};
                {ok, {mobile_commerce_payload, Payload}} ->
                   {mobile_commerce, Payload#ptt_MobileCommercePayload.mobile_commerce}
            end;
        _ ->
            decrypt_deprecated_token(Token)
    end.

%% Internal

decrypt_deprecated_token(Token) ->
    try
        capi_handler_encoder:encode_payment_tool(capi_utils:base64url_to_map(Token))
    catch
        error:badarg ->
            erlang:throw(invalid_token)
    end.

payment_tool_token_version() ->
    <<"v1">>.

create_encryption_params(IdempotentKey) ->
    #{iv => lechiffre:compute_iv(IdempotentKey)}.

-spec encode_payment_tool_token(payment_tool()) ->
    payment_tool_token().

encode_payment_tool_token({bank_card, BankCard}) ->
    {bank_card_payload, #ptt_BankCardPayload{
        bank_card = BankCard
    }};
encode_payment_tool_token({payment_terminal, PaymentTerminal}) ->
    {payment_terminal_payload, #ptt_PaymentTerminalPayload{
        payment_terminal = PaymentTerminal
    }};
encode_payment_tool_token({digital_wallet, DigitalWallet}) ->
    {digital_wallet_payload, #ptt_DigitalWalletPayload{
        digital_wallet = DigitalWallet
    }};
encode_payment_tool_token({crypto_currency, CryptoCurrency}) ->
    {crypto_currency_payload, #ptt_CryptoCurrencyPayload{
        crypto_currency = CryptoCurrency
    }};
encode_payment_tool_token({mobile_commerce, MobileCommerce}) ->
    {mobile_commerce_payload, #ptt_MobileCommercePayload {
        mobile_commerce = MobileCommerce
    }}.
