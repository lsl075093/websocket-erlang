%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     五子棋入口
%%% @end
%%%-------------------------------------------------------------------
-module(wuzi_go).
-author("liushl").
-include("player.hrl").
%% API
-export([handle_event/3, after_enter_room/2, player_enter_room/2, offline/1]).

%% TODO 获得房间列表
%% 进入房间：如果没有房间会创建一个房间
handle_event(20000, Player, Bin) ->
    {Name, Rest} = util:read_string_binary(Bin),
    {Url, _} = util:read_string_binary(Rest),
    #player{wuzi = #player_wuzi{room_id = RoomId}, oppen_id = RoleId} = Player,
    wuzi_mgr:send_role_info(RoomId, RoleId, Name, Url),
    Player;
handle_event(20001, Player, <<RoomId:32>>) ->
    wuzi_mgr:enter_room(Player#player.oppen_id, RoomId),
    Player;
%% 邀请玩家：TODO 是否需要，前端直接进入就行
%% 下棋
handle_event(20002, Player, <<X:8, Y:8>>) ->
    #player{wuzi = #player_wuzi{room_id = RoomId}, oppen_id = RoleId} = Player,
    wuzi_mgr:go(RoomId, RoleId, X, Y),
    Player;
%% 离开房间
handle_event(20004, Player, _) ->
    leave_room(Player);
handle_event(_, Player, _Info) ->
    erlang:display({?MODULE, ?LINE, no_match, _Info}),
    Player.

after_enter_room(Role, RoomId) ->
    gen_server:cast(util:get_player_pid(Role), {apply_cast, ?MODULE, player_enter_room, [RoomId]}).

player_enter_room(Player, RoomId) ->
    Wuzi = Player#player.wuzi,
    Player#player{wuzi = Wuzi#player_wuzi{room_id = RoomId}}.

offline(Player) ->
    leave_room(Player).

leave_room(Player) ->
    #player{wuzi = #player_wuzi{room_id = RoomId} = Wuzi, oppen_id = RoleId} = Player,
    case RoomId > 0 of
        true  ->
            wuzi_mgr:leave_room(RoleId, RoomId),
            Player#player{wuzi = Wuzi#player_wuzi{room_id = 0}};
        false ->
            Player
    end.

