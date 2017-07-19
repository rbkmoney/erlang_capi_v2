-module(capi_utils).

-export([logtag_process/2]).
-export([base64url_to_map/1]).
-export([to_universal_time/1]).

-spec logtag_process(atom(), any()) -> ok.

logtag_process(Key, Value) when is_atom(Key) ->
    % TODO preformat into binary?
    lager:md(orddict:store(Key, Value, lager:md())).

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) ->
    try jsx:decode(base64url:decode(Base64), [return_maps])
    catch
        Class:Reason ->
            _ = lager:debug("decoding base64 ~p failed with ~p:~p", [Base64, Class, Reason]),
            erlang:error(badarg)
    end.

-spec to_universal_time(undefined | binary()) -> undefined | binary().
to_universal_time(Tz = undefined) ->
    Tz;
to_universal_time(Tz) ->
    {ok, {Date, Time, Usec, TzOffset}} = rfc3339:parse(Tz),
    TzSec = calendar:datetime_to_gregorian_seconds({Date, Time}),
    %% The following crappy code is a dialyzer workaround
    %% for the wrong rfc3339:parse/1 spec.
    {UtcDate, UtcTime} = calendar:gregorian_seconds_to_datetime(
        case TzOffset of
            _ when is_integer(TzOffset) ->
                TzSec - (60*TzOffset);
            _ ->
                TzSec
        end),
    {ok, Utc} = rfc3339:format({UtcDate, UtcTime, Usec, 0}),
    Utc.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.
to_universal_time_test() ->
    ?assertEqual(undefined,                         to_universal_time(undefined)),
    ?assertEqual(<<"2017-04-19T13:56:07Z">>,        to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530000Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-endif. %%TEST