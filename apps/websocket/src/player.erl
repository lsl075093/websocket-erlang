%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     玩家进程
%%% @end
%%%-------------------------------------------------------------------
-module(player).
-author("liushl").

-behaviour(gen_server).

-include("player.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Arg :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, Player} |
%%                     {ok, Player, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, Player :: #player{}} | {ok, Player :: #player{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([OpenId, Socket, ConnPid]) ->
    erlang:process_flag(trap_exit, true),
    erlang:display({?MODULE, ?LINE, start_a_player, Socket, self()}),
    {ok, #player{
        oppen_id = OpenId,
        socket = Socket,
        conn_pid = ConnPid
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    Player :: #player{}) ->
    {reply, Reply :: term(), NewPlayer :: #player{}} |
    {reply, Reply :: term(), NewPlayer :: #player{}, timeout() | hibernate} |
    {noreply, NewPlayer :: #player{}} |
    {noreply, NewPlayer :: #player{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewPlayer :: #player{}} |
    {stop, Reason :: term(), NewPlayer :: #player{}}).
handle_call(Msg, From, Player) ->
    try
        do_handle_call(Msg, From, Player)
    catch _:_Error ->
        io:format("do_handle_call Msg:~w, Error:~w, ~p", [Msg, _Error, erlang:get_stacktrace()]),
        {reply, error, Player}
    end.
do_handle_call(_Request, _From, Player) ->
    io:format("module:~p: handle_call unexpect message:~p", [?MODULE, _Request]),
    {reply, ok, Player}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), Player :: #player{}) ->
    {noreply, NewPlayer :: #player{}} |
    {noreply, NewPlayer :: #player{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewPlayer :: #player{}}).
handle_cast(Msg, Player) ->
    try
        do_handle_cast(Msg, Player)
    catch _:_Error ->
        io:format("do handle_cast Msg:~w, Error:~w, ~p", [Msg, _Error, erlang:get_stacktrace()]),
        {noreply, Player}
    end.
do_handle_cast(_Request, Player) ->
    io:format("module:~p: handle_cast unexpect message:~p", [?MODULE, _Request]),
    {noreply, Player}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, Player) -> {noreply, Player} |
%%                                   {noreply, Player, Timeout} |
%%                                   {stop, Reason, Player}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), Player :: #player{}) ->
    {noreply, NewPlayer :: #player{}} |
    {noreply, NewPlayer :: #player{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewPlayer :: #player{}}).
handle_info(Msg, Player) ->
    try
        do_handle_info(Msg, Player)
    catch _:_Error ->
        io:format("do handle_info Msg:~w, Error:~w, ~p", [Msg, _Error, erlang:get_stacktrace()]),
        {noreply, Player}
    end.

do_handle_info({'EXIT', ConnPid, normal}, #player{conn_pid = ConnPid} = Player) ->
    {stop, ok, Player};

do_handle_info(_Info, Player) ->
    io:format("module:~p: handle_info unexpect message:~p", [?MODULE, _Info]),
    {noreply, Player}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, Player) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    Player :: #player{}) ->
    term()).
terminate(_Reason, _Player) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process Player when code is changed
%%
%% @spec code_change(OldVsn, Player, Extra) -> {ok, NewPlayer}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, Player :: #player{},
    Extra :: term()) ->
    {ok, NewPlayer :: #player{}} | {error, Reason :: term()}).
code_change(_OldVsn, Player, _Extra) ->
    {ok, Player}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
