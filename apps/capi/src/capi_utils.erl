-module(capi_utils).

-export([logtag_process/2]).
-export([base64url_to_map/1]).

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
