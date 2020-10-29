-module(capi_crypto).

-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").

-type encrypted_token() :: binary().
-type payment_tool() :: dmsl_domain_thrift:'PaymentTool'().
-type payment_tool_token() :: dmsl_payment_tool_token_thrift:'PaymentToolToken'().
-type payment_tool_token_payload() :: dmsl_payment_tool_token_thrift:'PaymentToolTokenPayload'().

-export_type([encrypted_token/0]).

-export([create_encrypted_payment_tool_token/2]).
-export([decrypt_payment_tool_token/1]).

-spec create_encrypted_payment_tool_token(payment_tool(), woody:deadline()) -> encrypted_token().
create_encrypted_payment_tool_token(PaymentTool, ValidUntil) ->
    PaymentToolToken = encode_payment_tool_token(PaymentTool, ValidUntil),
    ThriftType = {struct, union, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
    {ok, EncodedToken} = lechiffre:encode(ThriftType, PaymentToolToken),
    TokenVersion = payment_tool_token_version(),
    <<TokenVersion/binary, ".", EncodedToken/binary>>.

-spec decrypt_payment_tool_token(encrypted_token()) ->
    {ok, {payment_tool(), woody:deadline()}}
    | unrecognized
    | {error, lechiffre:decoding_error()}.
decrypt_payment_tool_token(Token) ->
    Ver = payment_tool_token_version(),
    Size = byte_size(Ver),
    case Token of
        <<Ver:Size/binary, ".", EncryptedPaymentToolToken/binary>> ->
            decrypt_token(EncryptedPaymentToolToken);
        <<"v1.", EncryptedPaymentToolToken/binary>> ->
            decrypt_token_v1(EncryptedPaymentToolToken);
        _ ->
            unrecognized
    end.

%% Internal

payment_tool_token_version() ->
    <<"v2">>.

decrypt_token(EncryptedPaymentToolToken) ->
    ThriftType = {struct, struct, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
    case lechiffre:decode(ThriftType, EncryptedPaymentToolToken) of
        {ok, PaymentToolToken} ->
            PaymentTool = decode_payment_tool_token_payload(PaymentToolToken#ptt_PaymentToolToken.payload),
            ValidUntil = decode_deadline(PaymentToolToken#ptt_PaymentToolToken.valid_until),
            {ok, {PaymentTool, ValidUntil}};
        {error, _} = Error ->
            Error
    end.

decrypt_token_v1(EncryptedPaymentToolToken) ->
    ThriftType = {struct, union, {dmsl_payment_tool_token_thrift, 'PaymentToolTokenPayload'}},
    case lechiffre:decode(ThriftType, EncryptedPaymentToolToken) of
        {ok, PaymentToolTokenPayload} ->
            {ok, {decode_payment_tool_token_payload(PaymentToolTokenPayload), undefined}};
        {error, _} = Error ->
            Error
    end.

-spec encode_deadline(woody:deadline()) -> binary() | undefined.
encode_deadline(undefined) ->
    undefined;
encode_deadline(Deadline) ->
    woody_deadline:to_binary(Deadline).

-spec encode_payment_tool_token(payment_tool(), woody:deadline()) -> payment_tool_token().
encode_payment_tool_token(PaymentTool, ValidUntil) ->
    #ptt_PaymentToolToken{
        payload = encode_payment_tool_token_payload(PaymentTool),
        valid_until = encode_deadline(ValidUntil)
    }.

-spec encode_payment_tool_token_payload(payment_tool()) -> payment_tool_token_payload().
encode_payment_tool_token_payload({bank_card, BankCard}) ->
    {bank_card_payload, #ptt_BankCardPayload{
        bank_card = BankCard
    }};
encode_payment_tool_token_payload({payment_terminal, PaymentTerminal}) ->
    {payment_terminal_payload, #ptt_PaymentTerminalPayload{
        payment_terminal = PaymentTerminal
    }};
encode_payment_tool_token_payload({digital_wallet, DigitalWallet}) ->
    {digital_wallet_payload, #ptt_DigitalWalletPayload{
        digital_wallet = DigitalWallet
    }};
encode_payment_tool_token_payload({crypto_currency, CryptoCurrency}) ->
    {crypto_currency_payload, #ptt_CryptoCurrencyPayload{
        crypto_currency = CryptoCurrency
    }};
encode_payment_tool_token_payload({mobile_commerce, MobileCommerce}) ->
    {mobile_commerce_payload, #ptt_MobileCommercePayload{
        mobile_commerce = MobileCommerce
    }}.

-spec decode_deadline(binary()) -> woody:deadline() | undefined.
decode_deadline(undefined) ->
    undefined;
decode_deadline(Deadline) ->
    woody_deadline:from_binary(Deadline).

-spec decode_payment_tool_token_payload(payment_tool_token_payload()) -> payment_tool().
decode_payment_tool_token_payload(PaymentToolToken) ->
    case PaymentToolToken of
        {bank_card_payload, Payload} ->
            {bank_card, Payload#ptt_BankCardPayload.bank_card};
        {payment_terminal_payload, Payload} ->
            {payment_terminal, Payload#ptt_PaymentTerminalPayload.payment_terminal};
        {digital_wallet_payload, Payload} ->
            {digital_wallet, Payload#ptt_DigitalWalletPayload.digital_wallet};
        {crypto_currency_payload, Payload} ->
            {crypto_currency, Payload#ptt_CryptoCurrencyPayload.crypto_currency};
        {mobile_commerce_payload, Payload} ->
            {mobile_commerce, Payload#ptt_MobileCommercePayload.mobile_commerce}
    end.
