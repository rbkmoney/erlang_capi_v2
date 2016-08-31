-module(capi_utils).

-export([get_hostname_ip/1]).
-export([logtag_process/2]).

-spec get_hostname_ip(Hostname | IP) -> IP when
    Hostname :: string(),
    IP :: inet:ip_address().

get_hostname_ip(Host) ->
    % TODO: respect preferred address family
    case inet:getaddr(Host, inet) of
        {ok, IP} ->
            IP;
        {error, Error} ->
            exit(Error)
    end.

-spec logtag_process(atom(), any()) -> ok.

logtag_process(Key, Value) when is_atom(Key) ->
    % TODO preformat into binary?
    lager:md(orddict:store(Key, Value, lager:md())).
