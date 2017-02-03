-module(capi_test_proxy).

-type ip() :: string().

-callback get_service_spec() -> {Path :: string(), Service :: {module(), atom()}}.

-export([get_child_spec/4]).
-export([get_child_spec/5]).
-export([get_url/3]).

%%

-spec get_child_spec(Name :: atom(), module(), ip(), inet:port_number()) ->
    supervisor:child_spec().

get_child_spec(Name, Module, Host, Port) ->
    get_child_spec(Name, Module, Host, Port, #{}).

-spec get_child_spec(Name :: atom(), module(), ip(), inet:port_number(), #{}) ->
    supervisor:child_spec().

get_child_spec(Name, Module, IPStr, Port, Args) ->
    {ok, IP} = inet:parse_address(IPStr),
    {Path, Service} = Module:get_service_spec(),
    woody_server:child_spec(
        {Module, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => capi_woody_event_handler,
            handlers => [{Path, {Service, {Module, Args}}}]
        }
    ).

-spec get_url(module(), ip(), inet:port_number()) ->
    supervisor:child_spec().

get_url(Module, Host, Port) ->
    {Path, _Service} = Module:get_service_spec(),
    iolist_to_binary(["http://", Host, ":", integer_to_list(Port), Path]).
