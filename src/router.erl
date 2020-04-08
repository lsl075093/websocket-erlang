%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(router).
-author("liushl").

-compile(export_all).
%% API
-export([]).
-include("socket_conn.hrl").

router(#socket_conn{player_pid = Pid} = State, <<Cmd:32, Bin/binary>>) ->
    CmdType = Cmd div 100,
    case router_by_cmd(CmdType) of
        {conn, Handler} ->
            self() ! read_next,
            handle_by_cmd(State, Handler, Cmd, CmdType, Bin);
        {player, Handler} when is_pid(Pid) ->
            gen_server:cast(Pid, {socket_event, Handler, Cmd, Bin});
        error ->
            error
    end;
router(State, _Bin) ->
    State.

router_by_cmd(100) -> {conn, player_login};
router_by_cmd(200) -> {player, wuzi_go};
router_by_cmd(_) -> error.

handle_by_cmd(State, Handler, Cmd, CmdType, Bin) ->
    CmdMod = list_to_atom( lists:concat(["pt_", CmdType]) ),
    case CmdMod:read(Cmd, Bin) of
        {ok, Data} ->
            Handler:handle_event(Cmd, State, Data);
        _Other ->
            read_error
    end.
    