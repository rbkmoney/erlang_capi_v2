-module(capi_utils).

-export([get_hostname_ip/1]).

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
