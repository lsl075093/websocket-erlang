%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     分发任务
%%% @end
%%%-------------------------------------------------------------------
-module(router).
-author("liushl").

%% API
-export([router/2]).
-include("socket_conn.hrl").

router(#socket_conn{player_pid = Pid} = State, <<Cmd:32, Bin/binary>>) ->
    CmdType = Cmd div 100,
    case router_by_cmd(CmdType) of
        {conn, Handler} ->
            Handler:handle_event(Cmd, State, Bin);
        {player, Handler} when is_pid(Pid) ->
            gen_server:cast(Pid, {socket_event, Handler, Cmd, Bin}),
            State;
        _ ->
            State
    end;
router(State, _Bin) ->
    State.

router_by_cmd(0)   -> {conn, auth_code2session};
router_by_cmd(100) -> {conn, player_login};
router_by_cmd(200) -> {player, wuzi_go};
router_by_cmd(_) -> error.

    