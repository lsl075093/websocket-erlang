%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     玩家登陆
%%% @end
%%%-------------------------------------------------------------------
-module(player_login).
-author("liushl").

-include("socket_conn.hrl").

%% API
-export([handle_event/3]).

%% 登陆
handle_event(10001, State, []) ->
    #socket_conn{open_id = OpenId, player_pid = PlayerPid, socket = Socket} = State,
    if
        OpenId =:= undefined ->
            {false, not_weixin_init};
        erlang:is_pid(PlayerPid) andalso erlang:is_process_alive(PlayerPid) ->
            {false, already_started};
        true ->
            case catch player:start_link([OpenId, Socket, self()]) of
                {ok, Pid} ->
                    State#socket_conn{player_pid = Pid};
                Error ->
                    erlang:display({?MODULE, ?LINE, Error}),
                    State
            end
    end;

handle_event(_, State, _Info) ->
    erlang:display({?MODULE, ?LINE, no_match, _Info}),
    State.