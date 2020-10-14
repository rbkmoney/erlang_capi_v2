-module(capi_api_key_blacklist).

-behaviour(gen_server).

%% interface

-export([check/1]).
-export([update/0]).
-export([start_link/0]).
-export([child_spec/0]).

%% gen_server callbacks

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%

-define(TABLE, ?MODULE).
-define(SERVER, ?MODULE).

-define(DEFAULT_INTERVAL, 50000).
-define(DEFAULT_DIR, "var/blacklisted_keys").
-define(DEFAULT_SETTINGS, #{
    update_interval => ?DEFAULT_INTERVAL,
    blacklisted_keys_dir => ?DEFAULT_DIR
}).

-record(state, {
    timer = undefined :: undefined | reference()
}).

-record(rec, {
    key :: key()
}).

-type state() :: #state{}.
-type key() :: swag_server:api_key().

%%

-spec check(key()) -> boolean().
check(Token) ->
    case ets:lookup(?TABLE, clean_token(Token)) of
        [#rec{}] ->
            true;
        [] ->
            false
    end.

-spec update() -> ok.
update() ->
    gen_server:call(?SERVER, update).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        type => worker,
        restart => permanent
    }.

%%

-spec init(any()) -> {ok, state(), 0}.
init(_Args) ->
    EtsOpts = [
        named_table,
        set,
        protected,
        {read_concurrency, true},
        {keypos, #rec.key}
    ],
    ?TABLE = ets:new(?TABLE, EtsOpts),
    {ok, #state{}, 0}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(update, _From, State) ->
    {reply, update_blacklist(), restart_timer(State)};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(timeout, State) ->
    _Result = update_blacklist(),
    {noreply, restart_timer(State)};
handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {error, noimpl}.
code_change(_OldVsn, _State, _Extra) ->
    {error, noimpl}.

%%

-spec restart_timer(state()) -> state().
restart_timer(State = #state{timer = undefined}) ->
    start_timer(State);
restart_timer(State = #state{timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    start_timer(State#state{timer = undefined}).

-spec start_timer(state()) -> state().
start_timer(State = #state{timer = undefined}) ->
    Interval = get_update_interval(),
    State#state{timer = erlang:send_after(Interval, self(), timeout)}.

%%

update_blacklist() ->
    KeyFiles = get_key_files(),
    [put(F) || F <- KeyFiles],
    ok.

get_key_files() ->
    Path = get_dir(),
    true = filelib:is_dir(Path),
    filelib:wildcard(filename:join(Path, "*.key")).

put(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    true = ets:insert(?TABLE, #rec{key = clean_token(Binary)}),
    ok.

clean_token(<<"Bearer ", Token/binary>>) ->
    Token;
clean_token(Token) ->
    Token.

get_update_interval() ->
    maps:get(update_interval, get_settings(), ?DEFAULT_INTERVAL).

get_dir() ->
    maps:get(blacklisted_keys_dir, get_settings(), ?DEFAULT_DIR).

get_settings() ->
    genlib_app:env(capi, api_key_blacklist, ?DEFAULT_SETTINGS).
