-module(capi_handler_decoder_utils).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-export([decode_map/2]).
-export([decode_currency/1]).
-export([decode_business_schedule_ref/1]).
-export([decode_last_digits/1]).
-export([decode_masked_pan/2]).
-export([decode_operation_failure/2]).
-export([decode_category_ref/1]).
-export([decode_context/1]).
-export([decode_optional/2]).
-export([convert_crypto_currency_from_swag/1]).
-export([convert_crypto_currency_to_swag/1]).

-export_type([decode_data/0]).

-type decode_data() :: #{binary() => term()}.

-spec decode_map(map(), fun((_) -> any())) ->
    [any()].

decode_map(Items, Fun) ->
    lists:map(Fun, maps:values(Items)).

-spec decode_currency(capi_handler_encoder:encode_data()) ->
    binary().

decode_currency(#domain_Currency   {symbolic_code = SymbolicCode}) -> SymbolicCode;
decode_currency(#domain_CurrencyRef{symbolic_code = SymbolicCode}) -> SymbolicCode.

-spec decode_business_schedule_ref(capi_handler_encoder:encode_data()) ->
    binary() | undefined.

decode_business_schedule_ref(#domain_BusinessScheduleRef{id = ID}) when ID /= undefined ->
    ID;
decode_business_schedule_ref(undefined) ->
    undefined.

-define(PAN_LENGTH, 16).

-spec decode_masked_pan(binary(), binary()) ->
    binary().

decode_masked_pan(Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, ?PAN_LENGTH - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec decode_last_digits(binary()) ->
    binary().

decode_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_last_digits(MaskedPan) ->
    MaskedPan.

-spec decode_operation_failure(_, _) ->
    decode_data().

decode_operation_failure({operation_timeout, _}, _) ->
    logic_error(timeout, <<"timeout">>);
decode_operation_failure({failure, #domain_Failure{code = Code, reason = Reason}}, _) ->
    logic_error(Code, Reason).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

-spec decode_category_ref(capi_handler_encoder:encode_data()) ->
    integer().

decode_category_ref(#domain_CategoryRef{id = CategoryRef}) ->
    CategoryRef.

-spec decode_context(capi_handler_encoder:encode_data()) ->
    decode_data() | undefined.

decode_context(#'Content'{type = <<"application/json">>, data = InvoiceContext}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);
decode_context(undefined) ->
    undefined.

-spec decode_optional(any() | undefined, fun((any()) -> decode_data())) ->
    decode_data().

decode_optional(Arg, DecodeFun) when Arg /= undefined ->
    DecodeFun(Arg);
decode_optional(undefined, _) ->
    undefined.

-spec convert_crypto_currency_from_swag(binary()) -> atom().

convert_crypto_currency_from_swag(<<"bitcoinCash">>) ->
    bitcoin_cash;
convert_crypto_currency_from_swag(CryptoCurrency) when is_binary(CryptoCurrency) ->
    binary_to_existing_atom(CryptoCurrency, utf8).

-spec convert_crypto_currency_to_swag(atom()) -> binary().

convert_crypto_currency_to_swag(bitcoin_cash) ->
    <<"bitcoinCash">>;
convert_crypto_currency_to_swag(CryptoCurrency) when is_atom(CryptoCurrency) ->
    atom_to_binary(CryptoCurrency, utf8).

