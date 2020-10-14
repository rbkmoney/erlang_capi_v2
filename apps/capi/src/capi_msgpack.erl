%%%
%%% Msgpack manipulation employed by damsel interfaces.

%%% Copy from machinery

-module(capi_msgpack).

-include_lib("damsel/include/dmsl_msgpack_thrift.hrl").

%% API

-export([wrap/1]).
-export([unwrap/1]).

-type t() :: dmsl_msgpack_thrift:'Value'().

-export_type([t/0]).

%%

-spec wrap
    (null) -> t();
    (boolean()) -> t();
    (integer()) -> t();
    (float()) -> t();
    %% string
    (binary()) -> t();
    %% binary
    ({binary, binary()}) -> t();
    ([t()]) -> t();
    (#{t() => t()}) -> t().
wrap(null) ->
    {nl, #msgpack_Nil{}};
wrap(V) when is_boolean(V) ->
    {b, V};
wrap(V) when is_integer(V) ->
    {i, V};
wrap(V) when is_float(V) ->
    V;
wrap(V) when is_binary(V) ->
    % Assuming well-formed UTF-8 bytestring.
    {str, V};
wrap({binary, V}) when is_binary(V) ->
    {bin, V};
wrap(V) when is_list(V) ->
    {arr, [wrap(ListItem) || ListItem <- V]};
wrap(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{wrap(Key) => wrap(Value)} end, #{}, V)}.

-spec unwrap(t()) ->
    null
    | boolean()
    | integer()
    | float()
    %% string
    | binary()
    %% binary
    | {binary, binary()}
    | [t()]
    | #{t() => t()}.
unwrap({nl, #msgpack_Nil{}}) ->
    null;
unwrap({b, V}) when is_boolean(V) ->
    V;
unwrap({i, V}) when is_integer(V) ->
    V;
unwrap({flt, V}) when is_float(V) ->
    V;
unwrap({str, V}) when is_binary(V) ->
    % Assuming well-formed UTF-8 bytestring.
    V;
unwrap({bin, V}) when is_binary(V) ->
    {binary, V};
unwrap({arr, V}) when is_list(V) ->
    [unwrap(ListItem) || ListItem <- V];
unwrap({obj, V}) when is_map(V) ->
    maps:fold(fun(Key, Value, Map) -> Map#{unwrap(Key) => unwrap(Value)} end, #{}, V).
