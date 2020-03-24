%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(websocket).
-author("liushl").

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%% 握手协议内容
-record(websocket_handshake, {
    type
    , version
    , sec_websocket_extensions
    , sec_websocket_key
    , referer
    , accept_language
    , accept_encoding
    , sec_websocket_version
    , origin
    , upgrade
    , user_agent
    , cache_control
    , pragma
    , connection
    , host
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
-spec(start(Args :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start([Socket]) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init([Socket]) ->
    {ok, #state{socket = Socket}}.

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
handle_call(_Request, _From, State) ->
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
handle_cast(_Request, State) ->
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
handle_info({tcp, Socket, Bin}, #state{socket = Socket} = State) ->
    case decode(Bin) of
        close ->
            gen_tcp:shutdown(Socket, read_write);
        Msg ->
            SendBin = encode(Msg),
            gen_tcp:send(Socket, SendBin),
            io:format("~p,~p,~ts~n", [?MODULE, ?LINE, Msg])
    end,
    {noreply, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info(_Info, State) ->
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
%% 是否websocket握手协议
is_websocket_handshake(Socket, Bin) ->
    case handle_http(Bin) of
        {ok, Data} ->
            check_is_websocket_handshake(Socket, Data);
        _Other ->
            erlang:display({?MODULE, ?LINE, _Other}),
            false
    end.

handle_http(Bin) ->
    Option = [{max_uri, nolimit}, {max_header, 10240},
        {max_version, 8},
        {max_method, 200},
        {max_content_length, 100000000},
        {customize, httpd_custom}
    ],
    httpd_request:parse([Bin, Option]).

check_is_websocket_handshake(Socket, Data) ->
    Handshake = decode_handshake(Data),
    case check_http_version(Handshake)
        andalso check_websocket_version(Handshake)
        andalso check_upgrade_protocol(Handshake)
        andalso check_websocket_key(Handshake)
    of
        true  ->
            do_handshake(Socket, Handshake),
            true;
        false ->
            false
    end.

do_handshake(Socket, Handshake) ->
    Key = list_to_binary(Handshake#websocket_handshake.sec_websocket_key),
    Challenge = base64:encode(crypto:hash(sha,
        << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    Msg = io_lib:format("~s 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ~s\r\n\r\n",
        [Handshake#websocket_handshake.version, Challenge]),
    gen_tcp:send(Socket, Msg).

%% HTTP请求方式
check_http_version(#websocket_handshake{type = "GET", version = "HTTP/1.1"}) -> true;
check_http_version(#websocket_handshake{type = "CONNECT", version = "HTTP/2"}) -> true;
check_http_version(_) -> false.
%% websocket支持版本
check_websocket_version(#websocket_handshake{sec_websocket_version = "7"}) -> true;
check_websocket_version(#websocket_handshake{sec_websocket_version = "8"}) -> true;
check_websocket_version(#websocket_handshake{sec_websocket_version = "13"}) -> true;
check_websocket_version(_) -> false.
%% 是否切换协议
check_upgrade_protocol(#websocket_handshake{upgrade = "websocket", connection = "Upgrade"}) -> true;
check_upgrade_protocol(_) -> false.
%% key
check_websocket_key(#websocket_handshake{sec_websocket_key = Key}) when Key =/= undefined -> true;
check_websocket_key(_) -> false.

decode_handshake({Type, _, Version, {_, Info}, _}) ->
    decode_handshake(#websocket_handshake{type = Type, version = Version}, Info).
decode_handshake(State, [{K, V}|T]) ->
    NewState =
        case K of
            "sec-websocket-extensions" ->
                State#websocket_handshake{sec_websocket_extensions = V};
            "sec-websocket-key" ->
                State#websocket_handshake{sec_websocket_key = V};
            "referer" ->
                State#websocket_handshake{referer = V};
            "accept-language" ->
                State#websocket_handshake{accept_language = V};
            "accept-encoding" ->
                State#websocket_handshake{accept_encoding = V};
            "sec-websocket-version" ->
                State#websocket_handshake{sec_websocket_version = V};
            "origin" ->
                State#websocket_handshake{origin = V};
            "upgrade" ->
                State#websocket_handshake{upgrade = V};
            "user-agent" ->
                State#websocket_handshake{user_agent = V};
            "cache-control" ->
                State#websocket_handshake{cache_control = V};
            "pragma" ->
                State#websocket_handshake{pragma = V};
            "connection" ->
                State#websocket_handshake{connection = V};
            "host" ->
                State#websocket_handshake{host = V};
            _ ->
                State
        end,
    decode_handshake(NewState, T);
decode_handshake(State, []) -> State.


%%0                   1                   2                   3
%%0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%+-+-+-+-+-------+-+-------------+-------------------------------+
%%|F|R|R|R| opcode|M| Payload len |    Extended payload length    |
%%|I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
%%|N|V|V|V|       |S|             |   (if payload len==126/127)   |
%%| |1|2|3|       |K|             |                               |
%%+-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
%%|     Extended payload length continued, if payload len == 127  |
%%+ - - - - - - - - - - - - - - - +-------------------------------+
%%|                               |Masking-key, if MASK set to 1  |
%%+-------------------------------+-------------------------------+
%%| Masking-key (continued)       |          Payload Data         |
%%+-------------------------------- - - - - - - - - - - - - - - - +
%%:                     Payload Data continued ...                :
%%+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
%%|                     Payload Data continued ...                |
%%+---------------------------------------------------------------+
%%FIN：1个比特。
%%
%%如果是1，表示这是消息（message）的最后一个分片（fragment），如果是0，表示不是是消息（message）的最后一个分片（fragment）。
%%
%%RSV1, RSV2, RSV3：各占1个比特。
%%
%%一般情况下全为0。当客户端、服务端协商采用WebSocket扩展时，这三个标志位可以非0，且值的含义由扩展进行定义。如果出现非零的值，且并没有采用WebSocket扩展，连接出错。
%%
%%Opcode: 4个比特。
%%
%%操作代码，Opcode的值决定了应该如何解析后续的数据载荷（data payload）。如果操作代码是不认识的，那么接收端应该断开连接（fail the connection）。可选的操作代码如下：
%%
%%%x0：表示一个延续帧。当Opcode为0时，表示本次数据传输采用了数据分片，当前收到的数据帧为其中一个数据分片。
%%%x1：表示这是一个文本帧（frame）
%%%x2：表示这是一个二进制帧（frame）
%%%x3-7：保留的操作代码，用于后续定义的非控制帧。
%%%x8：表示连接断开。
%%%x9：表示这是一个ping操作。
%%%xA：表示这是一个pong操作。
%%%xB-F：保留的操作代码，用于后续定义的控制帧。
%%Mask: 1个比特。
%%
%%表示是否要对数据载荷进行掩码操作。从客户端向服务端发送数据时，需要对数据进行掩码操作；从服务端向客户端发送数据时，不需要对数据进行掩码操作。
%%
%%如果服务端接收到的数据没有进行过掩码操作，服务端需要断开连接。
%%
%%如果Mask是1，那么在Masking-key中会定义一个掩码键（masking key），并用这个掩码键来对数据载荷进行反掩码。所有客户端发送到服务端的数据帧，Mask都是1。
%%
%%掩码的算法、用途在下一小节讲解。
%%
%%Payload length：数据载荷的长度，单位是字节。为7位，或7+16位，或1+64位。
%%
%%假设数Payload length === x，如果
%%
%%x为0~126：数据的长度为x字节。
%%x为126：后续2个字节代表一个16位的无符号整数，该无符号整数的值为数据的长度。
%%x为127：后续8个字节代表一个64位的无符号整数（最高位为0），该无符号整数的值为数据的长度。
%%此外，如果payload length占用了多个字节的话，payload length的二进制表达采用网络序（big endian，重要的位在前）。
%%
%%Masking-key：0或4字节（32位）
%%
%%所有从客户端传送到服务端的数据帧，数据载荷都进行了掩码操作，Mask为1，且携带了4字节的Masking-key。如果Mask为0，则没有Masking-key。
%%
%%备注：载荷数据的长度，不包括mask key的长度。
%%
%%Payload data：(x+y) 字节
%%
%%载荷数据：包括了扩展数据、应用数据。其中，扩展数据x字节，应用数据y字节。
%%
%%扩展数据：如果没有协商使用扩展的话，扩展数据数据为0字节。所有的扩展都必须声明扩展数据的长度，或者可以如何计算出扩展数据的长度。此外，扩展如何使用必须在握手阶段就协商好。如果扩展数据存在，那么载荷数据长度必须将扩展数据的长度包含在内。
%%
%%应用数据：任意的应用数据，在扩展数据之后（如果存在扩展数据），占据了数据帧剩余的位置。载荷数据长度 减去 扩展数据长度，就得到应用数据的长度。
%%
%%3、掩码算法
%%掩码键（Masking-key）是由客户端挑选出来的32位的随机数。掩码操作不会影响数据载荷的长度。掩码、反掩码操作都采用如下算法：
%%
%%首先，假设：
%%
%%original-octet-i：为原始数据的第i字节。
%%transformed-octet-i：为转换后的数据的第i字节。
%%j：为i mod 4的结果。
%%masking-key-octet-j：为mask key第j字节。
%%算法描述为： original-octet-i 与 masking-key-octet-j 异或后，得到 transformed-octet-i。
decode(Bin) ->
    <<_Fin:1, _RSV:3, Opcode:4, _Mask:1, Payload:7, Rest/binary>> = Bin,
    case Opcode of
        8 -> close;
        _ ->
            case Payload of
                126 ->
                    decode1(Rest, 2*8, 0);
                127 ->
                    decode1(Rest, 8*8, 0);
                _ ->
                    decode1(Rest, 0, Payload)
            end
    end.
decode1(Bin, 0, L) ->
    <<MaskingKey:4/binary, DataBin:L/binary, _/binary>> = Bin,
    mask_data(MaskingKey, DataBin);
decode1(Bin, Continue, Length) ->
    <<Add:Continue, MaskingKey:4/binary, Rest/binary>> = Bin,
    L = Add + Length,
    <<DataBin:L/binary, _/binary>> = Rest,
    Data = mask_data(MaskingKey, DataBin),
    Data.

mask_data(MaskingKey, DataBin) ->
    <<Mask1:8, Mask2:8, Mask3:8, Mask4:8>> = MaskingKey,
    mask_data(DataBin, {Mask1, Mask2, Mask3, Mask4}, 0, []).

mask_data(<<Bin:8, Rest/binary>>, Keys, I, List) ->
    J = (I rem 4) + 1,
    Key = erlang:element(J, Keys),
    mask_data(Rest, Keys, I+1, [Bin bxor Key |List]);
mask_data(_, _, _, List) ->
    list_to_binary(lists:reverse(List)).

%% 打包
encode(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 1, Rsv = 0, OpCode = 1, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.