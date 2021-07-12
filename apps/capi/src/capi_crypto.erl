-module(capi_crypto).

-include_lib("damsel/include/dmsl_payment_tool_token_thrift.hrl").

-type token() :: binary().
-type token_data() :: #{
    payment_tool := payment_tool(),
    valid_until := deadline(),
    token_link => token_link()
}.
-type token_link() :: dmsl_payment_tool_token_thrift:'PaymentToolTokenLink'() | undefined.
-type payment_tool() :: dmsl_domain_thrift:'PaymentTool'().
-type payment_tool_token() :: dmsl_payment_tool_token_thrift:'PaymentToolToken'().
-type payment_tool_token_payload() :: dmsl_payment_tool_token_thrift:'PaymentToolTokenPayload'().
-type deadline() :: capi_utils:deadline().

-export_type([token/0]).
-export_type([token_data/0]).
-export_type([token_link/0]).

-export([encode_token/1]).
-export([decode_token/1]).

-spec encode_token(token_data()) -> token().
encode_token(TokenData) ->
    PaymentToolToken = encode_payment_tool_token(TokenData),
    ThriftType = {struct, struct, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
    {ok, EncodedToken} = lechiffre:encode(ThriftType, PaymentToolToken),
    TokenVersion = token_version(),
    <<TokenVersion/binary, ".", EncodedToken/binary>>.

-spec decode_token(token()) -> {ok, token_data()} | unrecognized | {error, lechiffre:decoding_error()}.
decode_token(Token) ->
    Ver = token_version(),
    Size = byte_size(Ver),
    case Token of
        <<Ver:Size/binary, $., EncryptedPaymentToolToken/binary>> ->
            decrypt_token(EncryptedPaymentToolToken);
        _ ->
            unrecognized
    end.

%% Internal

token_version() ->
    <<"v2">>.

decrypt_token(EncryptedPaymentToolToken) ->
    ThriftType = {struct, struct, {dmsl_payment_tool_token_thrift, 'PaymentToolToken'}},
    case lechiffre:decode(ThriftType, EncryptedPaymentToolToken) of
        {ok, PaymentToolToken} ->
            {ok, #{
                payment_tool => decode_payment_tool_token_payload(PaymentToolToken#ptt_PaymentToolToken.payload),
                valid_until => decode_deadline(PaymentToolToken#ptt_PaymentToolToken.valid_until),
                token_link => PaymentToolToken#ptt_PaymentToolToken.token_link
            }};
        {error, _} = Error ->
            Error
    end.

-spec encode_payment_tool_token(token_data()) -> payment_tool_token().
encode_payment_tool_token(#{payment_tool := PaymentTool} = TokenData) ->
    #ptt_PaymentToolToken{
        payload = encode_payment_tool_token_payload(PaymentTool),
        valid_until = encode_deadline(maps:get(valid_until, TokenData)),
        token_link = maps:get(token_link, TokenData, undefined)
    }.

-spec encode_deadline(deadline()) -> binary() | undefined.
encode_deadline(undefined) ->
    undefined;
encode_deadline(Deadline) ->
    capi_utils:deadline_to_binary(Deadline).

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
encode_payment_tool_token_payload({crypto_currency_deprecated, CryptoCurrency}) ->
    {crypto_currency_payload, #ptt_CryptoCurrencyPayload{
        crypto_currency_deprecated = CryptoCurrency
    }};
encode_payment_tool_token_payload({mobile_commerce, MobileCommerce}) ->
    {mobile_commerce_payload, #ptt_MobileCommercePayload{
        mobile_commerce = MobileCommerce
    }}.

-spec decode_deadline(binary()) -> deadline() | undefined.
decode_deadline(undefined) ->
    undefined;
decode_deadline(Deadline) ->
    capi_utils:deadline_from_binary(Deadline).

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
            {crypto_currency_deprecated, Payload#ptt_CryptoCurrencyPayload.crypto_currency_deprecated};
        {mobile_commerce_payload, Payload} ->
            {mobile_commerce, Payload#ptt_MobileCommercePayload.mobile_commerce}
    end.
