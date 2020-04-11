%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     分发任务
%%% @end
%%%-------------------------------------------------------------------
-module(router).
-author("liushl").

-compile(export_all).
%% API
-export([]).
-include("socket_conn.hrl").

router(#socket_conn{player_pid = Pid} = State, Bin) ->
    Str = unicode:characters_to_list(Bin),
    [CmdStr|Data] = string:tokens(Str, ","),
    Cmd = list_to_integer(CmdStr),
    CmdType = Cmd div 100,
    case router_by_cmd(CmdType) of
        {conn, Handler} ->
            self() ! read_next,
            Handler:handle_event(Cmd, State, Data);
        {player, Handler} when is_pid(Pid) ->
            gen_server:cast(Pid, {socket_event, Handler, Cmd, Data}),
            State;
        error ->
            State
    end;
router(State, _Bin) ->
    State.

router_by_cmd(0)   -> {conn, auth_code2session};
router_by_cmd(100) -> {conn, player_login};
router_by_cmd(200) -> {player, wuzi_go};
router_by_cmd(_) -> error.

    