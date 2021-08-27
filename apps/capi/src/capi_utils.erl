-module(capi_utils).

-type deadline() :: woody:deadline().

-export_type([deadline/0]).

-export([deadline_to_binary/1]).
-export([deadline_from_binary/1]).
-export([deadline_from_timeout/1]).
-export([deadline_is_reached/1]).

-export([base64url_to_map/1]).
-export([map_to_base64url/1]).

-export([parse_deadline/1]).

-export([to_universal_time/1]).

-export([redact/2]).

-export([unwrap/1]).
-export([define/2]).

-export([deduplicate_payment_methods/1]).

-export([get_unique_id/0]).

-export([maybe/2]).

% 1 min
-define(MAX_REQUEST_DEADLINE_TIME, timer:minutes(1)).

-spec deadline_to_binary(deadline()) -> binary() | undefined.
deadline_to_binary(undefined) ->
    undefined;
deadline_to_binary(Deadline) ->
    woody_deadline:to_binary(Deadline).

-spec deadline_from_binary(binary()) -> deadline() | undefined.
deadline_from_binary(undefined) ->
    undefined;
deadline_from_binary(Binary) ->
    woody_deadline:from_binary(Binary).

-spec deadline_from_timeout(timeout()) -> deadline().
deadline_from_timeout(Timeout) ->
    woody_deadline:from_timeout(Timeout).

-spec deadline_is_reached(deadline()) -> boolean().
deadline_is_reached(Deadline) ->
    woody_deadline:is_reached(Deadline).

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) when is_binary(Base64) ->
    try
        {ok, Json} = jose_base64url:decode(Base64),
        jsx:decode(Json, [return_maps])
    catch
        Class:Reason ->
            _ = logger:debug("decoding base64 ~p to map failed with ~p:~p", [Base64, Class, Reason]),
            erlang:error(badarg)
    end.

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    try
        jose_base64url:encode(jsx:encode(Map))
    catch
        Class:Reason ->
            _ = logger:debug("encoding map ~p to base64 failed with ~p:~p", [Map, Class, Reason]),
            erlang:error(badarg)
    end.

-spec redact(Subject :: binary(), Pattern :: binary()) -> Redacted :: binary().
redact(Subject, Pattern) ->
    case re:run(Subject, Pattern, [global, {capture, all_but_first, index}]) of
        {match, Captures} ->
            lists:foldl(fun redact_match/2, Subject, Captures);
        nomatch ->
            Subject
    end.

redact_match({S, Len}, Subject) ->
    <<Pre:S/binary, _:Len/binary, Rest/binary>> = Subject,
    <<Pre/binary, (binary:copy(<<"*">>, Len))/binary, Rest/binary>>;
redact_match([Capture], Message) ->
    redact_match(Capture, Message).

-spec to_universal_time(Timestamp :: binary()) -> TimestampUTC :: binary().
to_universal_time(Timestamp) ->
    Micros = genlib_rfc3339:parse(Timestamp, microsecond),
    genlib_rfc3339:format_relaxed(Micros, microsecond).

-spec unwrap(ok | {ok, Value} | {error, _Error}) -> Value | no_return().
unwrap(ok) ->
    ok;
unwrap({ok, Value}) ->
    Value;
unwrap({error, Error}) ->
    erlang:error({unwrap_error, Error}).

-spec define(undefined | T, T) -> T.
define(undefined, V) ->
    V;
define(V, _Default) ->
    V.

-spec parse_deadline
    (binary()) -> {ok, woody:deadline()} | {error, bad_deadline};
    (undefined) -> {ok, undefined}.
parse_deadline(undefined) ->
    {ok, undefined};
parse_deadline(DeadlineStr) ->
    Parsers = [
        fun try_parse_woody_default/1,
        fun try_parse_relative/1
    ],
    try_parse_deadline(DeadlineStr, Parsers).

%%
%% Internals
%%
try_parse_deadline(_DeadlineStr, []) ->
    {error, bad_deadline};
try_parse_deadline(DeadlineStr, [P | Parsers]) ->
    case P(DeadlineStr) of
        {ok, _Deadline} = Result ->
            Result;
        {error, bad_deadline} ->
            try_parse_deadline(DeadlineStr, Parsers)
    end.

try_parse_woody_default(DeadlineStr) ->
    try
        Deadline = woody_deadline:from_binary(to_universal_time(DeadlineStr)),
        NewDeadline = clamp_max_request_deadline(woody_deadline:to_timeout(Deadline)),
        {ok, woody_deadline:from_timeout(NewDeadline)}
    catch
        error:{bad_deadline, _Reason} ->
            {error, bad_deadline};
        error:{badmatch, _} ->
            {error, bad_deadline};
        error:deadline_reached ->
            {error, bad_deadline}
    end.

try_parse_relative(DeadlineStr) ->
    %% deadline string like '1ms', '30m', '2.6h' etc
    case re:split(DeadlineStr, <<"^(\\d+\\.\\d+|\\d+)([a-z]+)$">>) of
        [<<>>, NumberStr, Unit, <<>>] ->
            Number = genlib:to_float(NumberStr),
            try_parse_relative(Number, Unit);
        _Other ->
            {error, bad_deadline}
    end.

try_parse_relative(Number, Unit) ->
    case unit_factor(Unit) of
        {ok, Factor} ->
            Timeout = erlang:round(Number * Factor),
            {ok, woody_deadline:from_timeout(clamp_max_request_deadline(Timeout))};
        {error, _Reason} ->
            {error, bad_deadline}
    end.

unit_factor(<<"ms">>) ->
    {ok, 1};
unit_factor(<<"s">>) ->
    {ok, 1000};
unit_factor(<<"m">>) ->
    {ok, 1000 * 60};
unit_factor(_Other) ->
    {error, unknown_unit}.

clamp_max_request_deadline(Value) when is_integer(Value) ->
    MaxDeadline = genlib_app:env(capi, max_request_deadline, ?MAX_REQUEST_DEADLINE_TIME),
    case Value > MaxDeadline of
        true ->
            MaxDeadline;
        false ->
            Value
    end.

%% TODO probably shlyop
-spec deduplicate_payment_methods(list()) -> list().
deduplicate_payment_methods(Methods) ->
    F = fun(Value, AccIn) ->
        EqFun = fun(V) ->
            payment_methods_equivalent(V, Value)
        end,
        case lists:partition(EqFun, AccIn) of
            {[Alike], NotAlike} ->
                [merge_payment_methods(Value, Alike) | NotAlike];
            {[], _} ->
                [Value | AccIn]
        end
    end,
    lists:foldr(F, [], Methods).

% payment methods are considered equivalent if they have the same method and token provider
% in case there no token provider method equality is enough
payment_methods_equivalent(#{<<"method">> := M} = M1, #{<<"method">> := M} = M2) ->
    maps:get(<<"tokenProviders">>, M1, undefined) =:= maps:get(<<"tokenProviders">>, M2, undefined);
payment_methods_equivalent(_, _) ->
    false.

merge_payment_methods(#{<<"method">> := M1} = Method, #{<<"method">> := M2} = Alike) when M1 =:= M2 ->
    do_merge_payment_methods(method_mergable_field(M1), Method, Alike).

method_mergable_field(<<"BankCard">>) ->
    <<"paymentSystems">>;
method_mergable_field(<<"PaymentTerminal">>) ->
    <<"providers">>;
method_mergable_field(<<"DigitalWallet">>) ->
    <<"providers">>;
method_mergable_field(<<"CryptoWallet">>) ->
    <<"cryptoCurrencies">>;
method_mergable_field(<<"MobileCommerce">>) ->
    <<"operators">>.

do_merge_payment_methods(MergableField, Method1, Method2) ->
    Merged = lists:umerge(maps:get(MergableField, Method1), maps:get(MergableField, Method2)),
    Method1#{MergableField => Merged}.

-spec get_unique_id() -> binary().
get_unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

-spec maybe(T | undefined, fun((T) -> R)) -> R | undefined.
maybe(undefined, _Fun) ->
    undefined;
maybe(V, Fun) ->
    Fun(V).

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.

to_universal_time_test() ->
    ?assertEqual(<<"2017-04-19T13:56:07Z">>, to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-spec redact_test() -> _.
redact_test() ->
    P1 = <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>,
    ?assertEqual(<<"+7******3210">>, redact(<<"+79876543210">>, P1)),
    ?assertEqual(<<"+1*11">>, redact(<<"+1111">>, P1)).

-spec parse_deadline_test() -> _.
parse_deadline_test() ->
    Deadline = woody_deadline:from_timeout(3000),
    BinDeadline = woody_deadline:to_binary(Deadline),
    {ok, {_, _}} = parse_deadline(BinDeadline),
    ?assertEqual({error, bad_deadline}, parse_deadline(<<"2017-04-19T13:56:07.53Z">>)),
    {ok, {_, _}} = parse_deadline(<<"15s">>),
    {ok, {_, _}} = parse_deadline(<<"15m">>),
    {error, bad_deadline} = parse_deadline(<<"15h">>).

-spec no_deduplication_test() -> _.
no_deduplication_test() ->
    Methods = [
        #{
            <<"method">> => "BankCard",
            <<"paymentSystems">> => [
                <<"mastercard">>,
                <<"visa">>
            ],
            <<"tokenProviders">> => [<<"applepay">>]
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => [
                <<"mastercard">>,
                <<"visa">>
            ],
            <<"tokenProviders">> => [<<"googlepay">>]
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => [
                <<"mastercard">>,
                <<"visa">>
            ],
            <<"tokenProviders">> => [<<"samsungpay">>]
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => [
                <<"mastercard">>,
                <<"visa">>,
                <<"visaelectron">>
            ]
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => [
                <<"mastercard">>,
                <<"visa">>
            ],
            <<"tokenProviders">> => [<<"yandexpay">>]
        }
    ],
    Methods = deduplicate_payment_methods(Methods).

-spec merge_deduplication_card_test() -> _.
merge_deduplication_card_test() ->
    Systems1 = [
        <<"mastercard">>,
        <<"visa">>,
        <<"visaelectron">>
    ],
    Systems2 = [
        <<"maestro">>,
        <<"mastercard">>,
        <<"nspkmir">>,
        <<"visa">>
    ],
    SamsungPayMethod = #{
        <<"method">> => <<"BankCard">>,
        <<"PaymentSystems">> => Systems1,
        <<"tokenProviders">> => [<<"samsungpay">>]
    },
    Methods = [
        SamsungPayMethod,
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => Systems1
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => Systems2
        }
    ],
    Merged = lists:umerge(Systems1, Systems2),
    [SamsungPayMethod, #{<<"paymentSystems">> := Merged}] = deduplicate_payment_methods(Methods).

-spec complicated_merge_card_test() -> _.
complicated_merge_card_test() ->
    Systems1 = [
        <<"mastercard">>,
        <<"visa">>,
        <<"visaelectron">>
    ],
    Systems2 = [
        <<"maestro">>,
        <<"mastercard">>,
        <<"nspkmir">>,
        <<"visa">>
    ],
    Systems3 = [
        <<"nonexistent">>,
        <<"fictional">>,
        <<"fake">>
    ],
    Methods = [
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => Systems1
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => Systems2
        },
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => Systems3
        }
    ],
    Merged = lists:umerge(Systems1, lists:umerge(Systems2, Systems3)),
    [#{<<"paymentSystems">> := Merged}] = deduplicate_payment_methods(Methods).

-spec merge_terminal_test() -> _.
merge_terminal_test() ->
    Providers1 = [
        <<"eurosvet">>,
        <<"fake">>
    ],
    Providers2 = [
        <<"asian">>,
        <<"fake">>
    ],
    Providers3 = [
        <<"american">>
    ],
    Methods = [
        #{
            <<"method">> => <<"PaymentTerminal">>,
            <<"providers">> => Providers1
        },
        #{
            <<"method">> => <<"PaymentTerminal">>,
            <<"providers">> => Providers2
        },
        #{
            <<"method">> => <<"PaymentTerminal">>,
            <<"providers">> => Providers3
        }
    ],
    Merged = lists:umerge(Providers1, lists:umerge(Providers2, Providers3)),
    [#{<<"providers">> := Merged}] = deduplicate_payment_methods(Methods).

-spec merge_digital_wallet_test() -> _.
merge_digital_wallet_test() ->
    Providers1 = [
        <<"eurosvet">>,
        <<"fake">>
    ],
    Providers2 = [
        <<"asian">>,
        <<"fake">>
    ],
    Providers3 = [
        <<"american">>
    ],
    Methods = [
        #{
            <<"method">> => <<"DigitalWallet">>,
            <<"providers">> => Providers1
        },
        #{
            <<"method">> => <<"DigitalWallet">>,
            <<"providers">> => Providers2
        },
        #{
            <<"method">> => <<"DigitalWallet">>,
            <<"providers">> => Providers3
        }
    ],
    Merged = lists:umerge(Providers1, lists:umerge(Providers2, Providers3)),
    [#{<<"providers">> := Merged}] = deduplicate_payment_methods(Methods).

-spec merge_crypto_wallet_test() -> _.
merge_crypto_wallet_test() ->
    Currencies1 = [
        <<"bitcoin">>,
        <<"litecoin">>
    ],
    Currencies2 = [
        <<"bitcoin">>,
        <<"bla-bla-coin">>
    ],
    Currencies3 = [
        <<"amecoin">>
    ],
    Methods = [
        #{
            <<"method">> => <<"CryptoWallet">>,
            <<"cryptoCurrencies">> => Currencies1
        },
        #{
            <<"method">> => <<"CryptoWallet">>,
            <<"cryptoCurrencies">> => Currencies2
        },
        #{
            <<"method">> => <<"CryptoWallet">>,
            <<"cryptoCurrencies">> => Currencies3
        }
    ],
    Merged = lists:umerge(Currencies1, lists:umerge(Currencies2, Currencies3)),
    [#{<<"cryptoCurrencies">> := Merged}] = deduplicate_payment_methods(Methods).

-spec merge_mobile_commerce_test() -> _.
merge_mobile_commerce_test() ->
    Operators1 = [
        <<"gibdd">>,
        <<"ufms">>
    ],
    Operators2 = [
        <<"fbi">>,
        <<"fsb">>
    ],
    Operators3 = [
        <<"nsa">>
    ],
    Methods = [
        #{
            <<"method">> => <<"MobileCommerce">>,
            <<"operators">> => Operators1
        },
        #{
            <<"method">> => <<"MobileCommerce">>,
            <<"operators">> => Operators2
        },
        #{
            <<"method">> => <<"MobileCommerce">>,
            <<"operators">> => Operators3
        }
    ],
    Merged = lists:umerge(Operators1, lists:umerge(Operators2, Operators3)),
    [#{<<"operators">> := Merged}] = deduplicate_payment_methods(Methods).

-endif.
