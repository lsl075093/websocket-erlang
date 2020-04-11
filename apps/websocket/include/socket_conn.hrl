%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("liushl").

-record(socket_conn, {
    socket,                     %%
    data = <<>>,                %%
    player_pid,
    open_id,
    msg_queue = queue:new()
}).