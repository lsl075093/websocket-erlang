%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wuzi_mgr).
-author("liushl").

-behaviour(gen_server).
-compile(export_all).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(GAME_SIZE, 12).

-record(state, {
    id = 1
}).
-record(room, {
    id = 0      %% 房间号
    , master    %% 主人
    , rival     %% 对手
    , game      %% 棋局状态
    , who_go    %% 当前可以下棋的人
}).
-record(role, {
    open_id,
    nickname,
    url
}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
enter_room(RoleId, RoomId) ->
    gen_server:cast(?MODULE, {enter_room, RoomId, #role{open_id = RoleId}}).
send_role_info(RoomId, RoleId, Name, Url) ->
    gen_server:cast(?MODULE, {send_role_info, RoomId, RoleId, Name, Url}).
leave_room(RoleId, RoomId) ->
    gen_server:cast(?MODULE, {leave_room, RoleId, RoomId}).
go(RoomId, RoleId, X, Y) ->
    gen_server:cast(?MODULE, {go, RoomId, RoleId, X, Y}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Msg, From, State) ->
    try
        do_handle_call(Msg, From, State)
    catch _:_Error ->
        io:format("do_handle_call Msg:~w, Error:~w, ~p", [Msg, _Error, erlang:get_stacktrace()]),
        {reply, error, State}
    end.
do_handle_call(_Request, _From, State) ->
    io:format("module:~p: handle_call unexpect message:~p", [?MODULE, _Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Msg, State) ->
    try
        do_handle_cast(Msg, State)
    catch _:_Error ->
        io:format("do handle_cast Msg:~w, Error:~w, ~p", [Msg, _Error, erlang:get_stacktrace()]),
        {noreply, State}
    end.

%% 进入房间
do_handle_cast({enter_room, 0, Role}, State) ->
    RoomId = State#state.id,
    Room = new_room(RoomId, Role),
    set_room(Room),
    util:send_to_client(Role#role.open_id, <<20001:32, RoomId:32>>),
    wuzi_go:after_enter_room(Role#role.open_id, RoomId),
    {noreply, State#state{id = RoomId + 1}};
do_handle_cast({enter_room, RoomId, Role}, State) ->
    case get_room(RoomId) of
        #room{rival = undefined} = Room ->
            NewRoom = Room#room{rival = Role,
                game = init_game()
            },
            set_room(NewRoom),
            #room{
                master = #role{nickname = MasterName, url = MasterUrl, open_id = MasterId},
                rival  = #role{nickname = RivalName, url = RivalUrl, open_id = RivalId}
            } = NewRoom,
            util:send_to_client(MasterId, pack_20003(RivalName, RivalUrl)),
            util:send_to_client(RivalId, pack_20003(MasterName, MasterUrl)),
            wuzi_go:after_enter_room(Role#role.open_id, RoomId),
            ok;
        _ ->
            ok
    end,
    {noreply, State};

%% {send_role_info, RoomId, RoleId, Name, Url}
do_handle_cast({send_role_info, RoomId, RoleId, Name, Url}, State) ->
    case get_room(RoomId) of
        #room{rival = #role{open_id = RoleId} = Role} = Room ->
            NewRoom = Room#room{rival = Role#role{nickname = Name, url = Url}},
            set_room(NewRoom),
            #room{
                master = #role{open_id = MasterId}
            } = Room,
            util:send_to_client(MasterId, pack_20003(Name, Url)),
            ok;
        #room{master = #role{open_id = RoleId} = Role} = Room ->
            NewRoom = Room#room{master = Role#role{nickname = Name, url = Url}},
            erlang:display({?MODULE, ?LINE, Name, Url}),
            util:send_to_client(RoleId, pack_20003(Name, Url)),
            set_room(NewRoom);
        _ ->
            ok
    end,
    {noreply, State};
%% 离开房间
do_handle_cast({leave_room, RoleId, RoomId}, State) ->
    case get_room(RoomId) of
        #room{master = #role{open_id = RoleId}} ->
            erase(RoomId);
        #room{rival = #role{open_id = RoleId}} ->
            erase(RoomId);
        _ ->
            ok
    end,
    {noreply, State};

%% 下棋
do_handle_cast({go, RoomId, RoleId, X, Y}, State) ->
    case get_room(RoomId) of
        #room{master = #role{open_id = RoleId}, who_go = RoleId} = Room ->
            go(Room, RoleId, X, Y, get_go_value(Room, RoleId));
        #room{rival = #role{open_id = RoleId}, who_go = RoleId} = Room ->
            go(Room, RoleId, X, Y, get_go_value(Room, RoleId));
        #room{master = #role{open_id = Master}, rival = #role{open_id = Rival}} = Room
            when Master =/= undefined andalso Rival =/= undefined andalso
            (RoleId == Master orelse RoleId == Rival) ->
            go(Room, RoleId, X, Y, get_go_value(Room, RoleId));
        _ ->
            util:send_to_client(RoleId, <<20002:32, X:8, Y:8, 1:8>>),
            ok
    end,
    {noreply, State};
do_handle_cast(_Request, State) ->
    io:format("module:~p: handle_cast unexpect message:~p", [?MODULE, _Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Msg, State) ->
    try
        do_handle_info(Msg, State)
    catch _:_Error ->
        io:format("do handle_info Msg:~w, Error:~w, ~p", [Msg, _Error, erlang:get_stacktrace()]),
        {noreply, State}
    end.
do_handle_info(_Info, State) ->
    io:format("module:~p: handle_info unexpect message:~p", [?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) ->
    term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_room(RoomId) ->
    get(RoomId).

set_room(#room{id = RoomId} = Room) ->
    put(RoomId, Room).

new_room(RoomId, Master) ->
    #room{
        id = RoomId,
        master = Master
    }.

init_game() ->
    Row = erlang:make_tuple(?GAME_SIZE, 0),
    erlang:make_tuple(?GAME_SIZE, Row).

get_position(Game, X, Y) ->
    erlang:element(X, erlang:element(Y, Game)).

set_position(Game, X, Y, Value) ->
    Row = erlang:element(Y, Game),
    NewRow = erlang:setelement(X, Row, Value),
    erlang:setelement(Y, NewRow, Game).

check_success(Game, X, Y, Value) ->
    %% 上下方向
    case check_success_by_direction(down, Game, X, Y+1, Value, 0) + 1 +
        check_success_by_direction(up, Game, X+1, Y, Value, 0) of
        5 ->
            true;
        _ ->
            %% 左右方向
            case check_success_by_direction(left, Game, X-1, Y, Value, 0) + 1 +
                check_success_by_direction(right, Game, X+1, Y, Value, 0) of
                5 ->
                    true;
                _ ->
                    %% 右上-左下方向
                    case check_success_by_direction(right_up, Game, X+1, Y-1, Value, 0) + 1 +
                        check_success_by_direction(right_down, Game, X+1, Y+1, Value, 0) of
                        5 ->
                            true;
                        _ ->
                            %% 右下-左上方向
                            case check_success_by_direction(left_down, Game, X-1, Y+1, Value, 0) + 1 +
                                check_success_by_direction(left_up, Game, X-1, Y-1, Value, 0) of
                                5 ->
                                    true;
                                _ ->
                                    false
                            end
                    end
            end
    end.

check_success_by_direction(Direction, Game, X, Y, Value, N) ->
    case not_cross_the_border(X, Y) andalso
        get_position(Game, X, Y) =:= Value of
        true  ->
            case Direction of
                up ->
                    check_success_by_direction(Direction, Game, X, Y-1, Value, N+1);
                down ->
                    check_success_by_direction(Direction, Game, X, Y+1, Value, N+1);
                left ->
                    check_success_by_direction(Direction, Game, X-1, Y, Value, N+1);
                right ->
                    check_success_by_direction(Direction, Game, X+1, Y, Value, N+1);
                right_up ->
                    check_success_by_direction(Direction, Game, X+1, Y-1, Value, N+1);
                right_down ->
                    check_success_by_direction(Direction, Game, X+1, Y+1, Value, N+1);
                left_down ->
                    check_success_by_direction(Direction, Game, X-1, Y+1, Value, N+1);
                left_up ->
                    check_success_by_direction(Direction, Game, X-1, Y-1, Value, N+1)
            end;
        false ->
            N
    end.

not_cross_the_border(X, Y) ->
    X >= 1 andalso X =< ?GAME_SIZE andalso Y >= 1 andalso Y =< ?GAME_SIZE.

who_next_go(#room{master = #role{open_id = RoleId}, rival = #role{open_id = NextRole}}, RoleId) -> NextRole;
who_next_go(#room{master = #role{open_id = NextRole}, rival = #role{open_id = RoleId}}, RoleId) -> NextRole.

get_go_value(#room{master = #role{open_id = RoleId}}, RoleId) -> 1;
get_go_value(#room{rival = #role{open_id = RoleId}},  RoleId) -> 2.

go(Room, RoleId, X, Y, Value) ->
    #room{game = Game} = Room,
    case get_position(Game, X, Y) =:= 0 of
        true  ->
            NewGame = set_position(Game, X, Y, Value),
            NewRoom = Room#room{
                game = NewGame,
                who_go = who_next_go(Room, RoleId)
            },
            set_room(NewRoom),
            Bin = <<20002:32, X:8, Y:8, Value:8>>,
            util:send_to_client(NewRoom#room.master#role.open_id, Bin),
            util:send_to_client(NewRoom#room.rival#role.open_id, Bin),
            case check_success(Game, X, Y, Value) of
                true  ->
                    win;
                false ->
                    nothing
            end;
        false ->
            ok
    end.

pack_20003(Name, Url) ->
    NameBin = util:write_string(Name),
    UrlBin = util:write_string(Url),
    <<20003:32, NameBin/binary, UrlBin/binary>>.