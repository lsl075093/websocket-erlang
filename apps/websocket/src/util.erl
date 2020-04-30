%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(util).
-author("liushl").

%% API
-compile(export_all).

player_process_name(Id) ->
    {player_pid, Id}.

conn_process_name(Id) ->
    {conn, Id}.

get_player_pid(Id) ->
    global:whereis_name(player_process_name(Id)).

get_conn_pid(Id) ->
    global:whereis_name(conn_process_name(Id)).

send_to_client(Id, Msg) ->
    websocket:send_to_client(get_conn_pid(Id), Msg).

send_to_socket(Socket, Msg) ->
    gen_tcp:send(Socket, websocket:encode(Msg)).

%% 分包
encode_begin(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 0, Rsv = 0, OpCode = 1, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.
encode_continue(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 0, Rsv = 0, OpCode = 0, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.
encode_end(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 1, Rsv = 0, OpCode = 0, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.


%% 打包字符串
write_string(S) when is_list(S)->
    SB = unicode:characters_to_binary(S, utf8),
    L = byte_size(SB),
    <<L:16, SB/binary>>;

write_string(S) when is_binary(S)->
    L = byte_size(S),
    <<L:16, S/binary>>;

write_string(S) when is_integer(S)->
    SS = integer_to_list(S),
    SB = list_to_binary(SS),
    L = byte_size(SB),
    <<L:16, SB/binary>>;

write_string(_S) ->
    <<0:16, <<>>/binary>>.

%% 读取字符串 返回的是binary()
read_string_binary(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {Str, Rest};
                _R1 ->
                    {<<>>,<<>>}
            end;
        _R1 ->
            {<<>>,<<>>}
    end.

%%读取字符串
read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {unicode:characters_to_list(Str, utf8), Rest};
                _R1 ->
                    {[],<<>>}
            end;
        _R1 ->
            {[],<<>>}
    end.