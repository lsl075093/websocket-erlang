%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("liushl").

-record(socket_conn, {
    socket,
    data = <<>>,
    player_pid,
    msg_queue = queue:new()
}).