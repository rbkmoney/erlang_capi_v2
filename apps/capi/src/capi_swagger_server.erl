-module(capi_swagger_server).

-behaviour(supervisor).

-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).

-export([child_spec/1]).
-export([start_link/1]).

-export([init/1]).

-export([response_hook/4]).


-type params() :: module().

-spec child_spec(params()) -> supervisor:child_spec().

child_spec(Params) ->
    #{id => {?MODULE, swagger}, start => {?MODULE, start_link, [Params]}, type => supervisor}.

-spec start_link(params()) ->
    {ok, pid()}.

start_link(Params) ->
    supervisor:start_link(?MODULE, Params).

-spec init(params()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(LogicHandler) ->
    {ok, {
        #{strategy => rest_for_one},
        [child_spec_(LogicHandler)]
    }}.

-spec response_hook(cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
    cowboy_req:req().

response_hook(500, Headers, _, Req) ->
    Body = get_oops_body(),
    Headers1 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(byte_size(Body))}),
    {ok, Req1} = cowboy_req:reply(500, Headers1, Body, Req),
    Req1;
response_hook(_, _, _, Req) ->
    Req.

%%

child_spec_(LogicHandler) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(LogicHandler),
    AcceptorsPool = genlib_app:env(?MODULE, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    ranch:child_spec(?MODULE, AcceptorsPool,
        Transport, TransportOpts, cowboy_protocol, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    Port     = genlib_app:env(capi, port, 8080),
    {ranch_tcp, [{ip, IP}, {port, Port}]}.

get_cowboy_config(LogicHandler) ->
    Dispatch = cowboy_router:compile(swagger_router:get_paths(LogicHandler)),
    [
        {env, [
            {dispatch, Dispatch},
            {cors_policy, capi_cors_policy}
        ]},
        {middlewares, [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ]},
        {onresponse, fun ?MODULE:response_hook/4}
    ].

get_oops_body() ->
    <<
        "█▄░░░░░░░░░░░░░░░░░░░░░░░░▄▄███\n"
        "███▄░░░░░░░░░░░░░░░░░░░░▄██████\n"
        "█████▄░░░░░░░░░░░░░░░░░▄███████\n"
        "███████▄░░░░▄▄▄▄▄░░░░▄█████████\n"
        "█████████▄▀▀░░░░░▀▀▀▄██████████\n"
        "▀█████▀░░░░░░░░░░░░░░▀████████░\n"
        "░▀██▀░░░░░░░░░░░░░░░░░░░▀████▌░\n"
        "░░██░░░░░░░░░░░░░░░░░░░░░░███░░\n"
        "░░█▀░░░░░░░░░░░░░░░░░░░░░░░██░░\n"
        "░░█░░▄████▄░░░░░▄████▄░░░░░░█░░\n"
        "░░█░░█▐▄█▐█░░░░░█▐▄█▐█░░░░░░█▄░\n"
        "░░█░░██▄▄██░░░░░██▄▄██░░░░░░░█░\n"
        "░▐▌░░░░░░░░░░░░░░░░░░░░░░░░░░▐▌\n"
        "░▐▌░░░░░░░▀▄▄▄▄▀░░░░░░░░░░░░░▐▌\n"
        "░▐▌░░░░░░░░░▐▌░░░░░░░░░░░░░░░▐▌\n"
        "░▐▌░░░░░░░▄▀▀▀▀▄░░░░░░░░░░░░░▐▌\n"
        "░░█▄░░░░░▀░░░░░░▀░░░░░░░░░░░░█▌\n"
        "░░▐█▀▄▄░░░░░░░░░░░░░░░░░░▄▄▀▀░█\n"
        "░▐▌░░░░▀▀▄▄░░░░░░░░▄▄▄▄▀▀░░░░░█\n"
        "░█░░░░░░░░░▀▀▄▄▄▀▀▀░░░░░░░░░░░█\n"
        "▐▌░░░░░░░░░░░░░░░░░░░░░░░░░░░░█\n"
        "▐▌░░░░░░░░░░░░░░░░░░░░░░░░░░░░█\n"
    >>.
