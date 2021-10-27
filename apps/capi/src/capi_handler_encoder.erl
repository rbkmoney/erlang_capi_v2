-module(capi_handler_encoder).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").

-export([encode_contact_info/1]).
-export([encode_client_info/1]).
-export([encode_payment_tool/1]).
-export([encode_cash/1]).
-export([encode_cash/2]).
-export([encode_currency/1]).
-export([encode_invoice_cart/1]).
-export([encode_invoice_cart/2]).
-export([encode_invoice_bank_account/1]).
-export([encode_stat_request/1]).
-export([encode_invoice_context/1]).
-export([encode_payment_context/1]).
-export([encode_invoice_line_meta/1]).
-export([encode_residence/1]).
-export([encode_content/2]).
-export([encode_stat_request/2]).

-export_type([encode_data/0]).

-type request_data() :: capi_handler:request_data().
-type encode_data() :: tuple().

-spec encode_contact_info(request_data()) -> encode_data().
encode_contact_info(ContactInfo) ->
    #domain_ContactInfo{
        phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
        email = genlib_map:get(<<"email">>, ContactInfo)
    }.

-spec encode_client_info(request_data()) -> encode_data().
encode_client_info(ClientInfo) ->
    #domain_ClientInfo{
        fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
        ip_address = maps:get(<<"ip">>, ClientInfo)
    }.

-spec encode_payment_tool(request_data()) -> encode_data().
encode_payment_tool(PaymentTool) ->
    case PaymentTool of
        #{<<"type">> := <<"bank_card">>} = Encoded ->
            encode_bank_card(Encoded);
        #{<<"type">> := <<"payment_terminal">>} = Encoded ->
            encode_payment_terminal(Encoded);
        #{<<"type">> := <<"digital_wallet">>} = Encoded ->
            encode_digital_wallet(Encoded);
        #{<<"type">> := <<"crypto_wallet">>} = Encoded ->
            encode_crypto_wallet(Encoded);
        #{<<"type">> := <<"mobile_commerce">>} = Encoded ->
            encode_mobile_commerce(Encoded)
    end.

encode_bank_card(BankCard) ->
    {PaymentSystemDeprecated, PaymentSystem} =
        infer_legacy_and_dictionary_ids(
            payment_system_legacy,
            maps:get(<<"payment_system">>, BankCard)
        ),

    {PaymentTokenProvider, LegacyTokenProvider} =
        infer_legacy_and_dictionary_ids(
            payment_token_legacy,
            genlib_map:get(<<"token_provider">>, BankCard)
        ),

    {bank_card, #domain_BankCard{
        token = maps:get(<<"token">>, BankCard),
        payment_system = PaymentSystem,
        payment_system_deprecated = PaymentSystemDeprecated,
        bin = maps:get(<<"bin">>, BankCard, <<>>),
        last_digits = maps:get(<<"masked_pan">>, BankCard),
        payment_token = PaymentTokenProvider,
        token_provider_deprecated = LegacyTokenProvider,
        issuer_country = encode_residence(genlib_map:get(<<"issuer_country">>, BankCard)),
        bank_name = genlib_map:get(<<"bank_name">>, BankCard),
        metadata = encode_bank_card_metadata(genlib_map:get(<<"metadata">>, BankCard)),
        is_cvv_empty = encode_bank_card_cvv_flag(genlib_map:get(<<"is_cvv_empty">>, BankCard)),
        tokenization_method = genlib_map:get(<<"tokenization_method">>, BankCard)
    }}.

encode_bank_card_cvv_flag(undefined) ->
    undefined;
encode_bank_card_cvv_flag(Flag) when is_binary(Flag) ->
    erlang:binary_to_existing_atom(Flag, utf8).

encode_bank_card_metadata(undefined) ->
    undefined;
encode_bank_card_metadata(Meta) ->
    maps:map(fun(_, Data) -> capi_msgp_marshalling:marshal(Data) end, Meta).

-spec encode_residence(binary() | undefined) -> atom().
encode_residence(undefined) ->
    undefined;
encode_residence(Residence) when is_binary(Residence) ->
    case capi_domain:encode_enum('CountryCode', string:lowercase(Residence)) of
        {ok, EncodedResidence} -> EncodedResidence;
        {error, _} -> throw({encode_residence, invalid_residence})
    end.

encode_payment_terminal(#{<<"terminal_type">> := Type}) ->
    {LegacyTerminalProvider, PaymentService} =
        infer_legacy_and_dictionary_ids(terminal_provider_legacy, Type),
    {payment_terminal, #domain_PaymentTerminal{
        payment_service = PaymentService,
        terminal_type_deprecated = LegacyTerminalProvider
    }}.

encode_digital_wallet(#{<<"provider">> := Provider, <<"id">> := ID} = Wallet) ->
    {LegacyDigitalWalletProvider, PaymentService} =
        infer_legacy_and_dictionary_ids(payment_service_legacy, Provider),
    {digital_wallet, #domain_DigitalWallet{
        payment_service = PaymentService,
        provider_deprecated = LegacyDigitalWalletProvider,
        id = ID,
        token = maps:get(<<"token">>, Wallet, undefined)
    }}.

encode_crypto_wallet(#{<<"crypto_currency">> := CryptoCurrency0}) ->
    CryptoCurrency =
        convert_crypto_currency_from_swag(CryptoCurrency0),

    case infer_legacy_and_dictionary_ids(payment_service_legacy, CryptoCurrency) of
        {LegacyCryptoCurrency, undefined} ->
            {crypto_currency_deprecated, LegacyCryptoCurrency};
        {_, CryptoCurrencyRef} ->
            {crypto_currency, CryptoCurrencyRef}
    end.

convert_crypto_currency_from_swag(<<"bitcoinCash">>) ->
    <<"bitcoin_cash">>;
convert_crypto_currency_from_swag(CryptoCurrency) when is_binary(CryptoCurrency) ->
    CryptoCurrency.

encode_mobile_commerce(#{<<"phoneNumber">> := PhoneNumber, <<"operator">> := Operator}) ->
    #{<<"cc">> := Cc, <<"ctn">> := Ctn} = PhoneNumber,
    {LegacyOperator, OperatorRef} = infer_legacy_and_dictionary_ids(mobile_operator_legacy, Operator),
    {mobile_commerce, #domain_MobileCommerce{
        operator = OperatorRef,
        operator_deprecated = LegacyOperator,
        phone = #domain_MobilePhone{
            cc = Cc,
            ctn = Ctn
        }
    }}.

-spec encode_cash(request_data()) -> encode_data().
encode_cash(Params) ->
    Amount = genlib_map:get(<<"amount">>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_cash(Amount, Currency).

-spec encode_cash(integer(), binary()) -> encode_data().
encode_cash(Amount, Currency) ->
    #domain_Cash{
        amount = Amount,
        currency = encode_currency(Currency)
    }.

-spec encode_currency(binary()) -> encode_data().
encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{symbolic_code = SymbolicCode}.

-spec encode_invoice_cart(request_data()) -> encode_data().
encode_invoice_cart(Params) ->
    Cart = genlib_map:get(<<"cart">>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_invoice_cart(Cart, Currency).

-spec encode_invoice_cart(list(), binary()) -> encode_data().
encode_invoice_cart(Cart, Currency) when Cart =/= undefined, Cart =/= [] ->
    #domain_InvoiceCart{
        lines = [encode_invoice_line(Line, Currency) || Line <- Cart]
    };
encode_invoice_cart([], _) ->
    throw(invoice_cart_empty);
encode_invoice_cart(undefined, _) ->
    undefined.

encode_invoice_line(Line, Currency) ->
    Metadata = encode_invoice_line_meta(Line),
    Price = encode_cash(genlib_map:get(<<"price">>, Line), Currency),
    #domain_InvoiceLine{
        product = genlib_map:get(<<"product">>, Line),
        quantity = genlib_map:get(<<"quantity">>, Line),
        price = Price,
        metadata = Metadata
    }.

-spec encode_invoice_line_meta(request_data()) -> #{binary() => {str, _}}.

-define(DEFAULT_INVOICE_LINE_META, #{}).

encode_invoice_line_meta(Line) ->
    case genlib_map:get(<<"taxMode">>, Line) of
        TaxMode when TaxMode =/= undefined ->
            TM = encode_invoice_line_tax_mode(TaxMode),
            #{<<"TaxMode">> => {str, TM}};
        undefined ->
            ?DEFAULT_INVOICE_LINE_META
    end.

encode_invoice_line_tax_mode(#{<<"type">> := <<"InvoiceLineTaxVAT">>} = TaxMode) ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    genlib_map:get(<<"rate">>, TaxMode).

-spec encode_invoice_bank_account(request_data()) -> dmsl_domain_thrift:'InvoiceBankAccount'() | undefined.
encode_invoice_bank_account(Params) ->
    do_encode_invoice_bank_account(genlib_map:get(<<"bankAccount">>, Params)).

do_encode_invoice_bank_account(#{<<"accountType">> := <<"InvoiceRussianBankAccount">>} = Account) ->
    {russian, #domain_InvoiceRussianBankAccount{
        account = maps:get(<<"account">>, Account),
        bank_bik = maps:get(<<"bankBik">>, Account)
    }};
do_encode_invoice_bank_account(undefined) ->
    undefined.

-define(DEFAULT_INVOICE_META, #{}).

-spec encode_invoice_context(request_data()) -> encode_data().
encode_invoice_context(Params) ->
    encode_invoice_context(Params, ?DEFAULT_INVOICE_META).

encode_invoice_context(Params, DefaultMeta) ->
    Context = genlib_map:get(<<"metadata">>, Params, DefaultMeta),
    encode_content(json, Context).

-spec encode_payment_context(request_data()) -> encode_data() | undefined.
encode_payment_context(#{<<"metadata">> := Context}) ->
    encode_content(json, Context);
encode_payment_context(#{}) ->
    undefined.

-spec encode_content(json, term()) -> encode_data().
encode_content(json, Data) ->
    #'Content'{
        type = <<"application/json">>,
        data = jsx:encode(Data)
    }.

-spec encode_stat_request(map() | binary()) -> encode_data().
encode_stat_request(Dsl) ->
    encode_stat_request(Dsl, undefined).

-spec encode_stat_request(map() | binary(), binary() | undefined) -> encode_data().
encode_stat_request(Dsl, ContinuationToken) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl), ContinuationToken);
encode_stat_request(Dsl, ContinuationToken) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl,
        continuation_token = ContinuationToken
    }.

infer_legacy_and_dictionary_ids(ObjectVariant, ID) ->
    {dmsl_domain_thrift, LegacyIDType} =
        capi_domain:extract_type(
            capi_domain:fetch_type_info(
                'DomainObject',
                [
                    {variant, ObjectVariant},
                    {field, ref},
                    {field, id}
                ]
            )
        ),

    case capi_domain:encode_enum(LegacyIDType, ID) of
        {ok, LegacyID} ->
            DictionaryID = map_to_dictionary_id(ObjectVariant, LegacyID),
            {LegacyID, DictionaryID};
        {error, _} ->
            DictionaryID = ID,
            {undefined, DictionaryID}
    end.

map_to_dictionary_id(ObjectVariant, LegacyID) ->
    try
        capi_domain:map_to_dictionary_id(ObjectVariant, LegacyID)
    catch
        error:{no_mapping, _} -> undefined
    end.
