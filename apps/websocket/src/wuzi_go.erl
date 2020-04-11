%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     五子棋入口
%%% @end
%%%-------------------------------------------------------------------
-module(wuzi_go).
-author("liushl").

%% API
-export([handle_event/3]).

%% TODO 获得房间列表
%% 进入房间：如果没有房间会创建一个房间
%% 邀请玩家：TODO 是否需要，前端直接进入就行
%% 下棋
%% 重新开始
%% 离开房间
handle_event(20001, Player, []) ->
    Player;
handle_event(_, Player, _Info) ->
    erlang:display({?MODULE, ?LINE, no_match, _Info}),
    Player.