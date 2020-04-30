%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("liushl").
-record(player_wuzi, {
    room_id = 0
}).
-record(player, {
    oppen_id,
    socket,
    conn_pid,
    name,
    wuzi = #player_wuzi{}
}).

