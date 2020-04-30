%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(websocket_fragment).
-author("liushl").

-record(fragment, {
    fin,
    rsv,
    data_type,
    mask_key,
    payload_bit = 0,
    payload,
    data,
    fragment_sign = unfragmented,
    fragment_list = []
}).

%% API
-export([decode/1,encode/1,
    encode_begin/1,
    encode_continue/1,
    encode_end/1,encode_close/0]).

decode(Bin) ->
    case decode(#fragment{}, Bin) of
        {error, Error, Fragments} ->
            {error, Error, Fragments#fragment.fragment_list};
        Fragments when is_record(Fragments, fragment)->
            Fragments#fragment.fragment_list;
        {close_socket, Fragments} ->
            {close_socket, Fragments#fragment.fragment_list}
    end.

decode(Fragment, <<>>) -> Fragment;
decode(Fragment, Bin)  -> decode_fin(Fragment, Bin).

decode_fin(Fragment, <<Fin:1, Bin/bitstring>>) ->
    decode_rsv(Fragment#fragment{fin = Fin}, Bin);
decode_fin(Fragment, _) ->
    {error, fin_error, Fragment}.

decode_rsv(Fragment, <<RSV:3, Bin/bitstring>>) ->
    case RSV =:= 0 of
        true  ->
            decode_opcode(Fragment#fragment{rsv = 0}, Bin);
        false ->
            {error, rsv_error, Fragment}
    end.

decode_opcode(Fragment, <<Opcode:4, Bin/bitstring>>) ->
    if
        Opcode =:= 0 ->
            DataType = get_continue_data_type(),
            FragmentSign = get_fragment_sign(Fragment#fragment.fin, Opcode),
            decode_mask(Fragment#fragment{data_type = DataType, fragment_sign = FragmentSign}, Bin);
        Opcode =:= 1 orelse Opcode =:= 2 ->
            FragmentSign = get_fragment_sign(Fragment#fragment.fin, Opcode),
            decode_mask(Fragment#fragment{data_type = Opcode, fragment_sign = FragmentSign}, Bin);
        Opcode =:= 8 ->
            {close_socket, Fragment};
        Opcode =:= 9 andalso Fragment#fragment.fin =:= 1 ->
            %% TODO 处理ping-pong之后的包
            ping;
        Opcode =:= 10 andalso Fragment#fragment.fin =:= 1 ->
            pong;
        true ->
            {error, opcode_error, Fragment}
    end;
decode_opcode(Fragment, _) -> {error, opcode_error, Fragment}.

get_fragment_sign(0, 0) ->
    fragmented_continue;
get_fragment_sign(1, 0) ->
    fragmented_end;
get_fragment_sign(0, Opcode) when Opcode > 0 ->
    fragmented_begin;
get_fragment_sign(_Fin, _Opcode) ->
    unfragmented.

decode_mask(Fragment, <<1:1, Bin/bitstring>>) ->
    decode_data_length(Fragment, Bin);
decode_mask(Fragment, _) -> {error, mask_error, Fragment}.

decode_data_length(Fragment, <<Payload:7, Bin/bitstring>>) ->
    case Payload of
        126 ->
            BitLength = 16,
            decode_extended_payload_length(Fragment#fragment{payload_bit = BitLength}, Bin);
        127 ->
            BitLength = 64,
            decode_extended_payload_length(Fragment#fragment{payload_bit = BitLength}, Bin);
        _ ->
            decode_mask_key(Fragment#fragment{payload = Payload}, Bin)
    end;
decode_data_length(Fragment, _) -> {error, data_length_error, Fragment}.

decode_extended_payload_length(Fragment, Bin) ->
    BitLength = Fragment#fragment.payload_bit,
    case Bin of
        <<Length:BitLength, Rest/bitstring>> ->
            decode_mask_key(Fragment#fragment{payload = Length}, Rest);
        _ ->
            {error, extended_payload_length_error, Fragment}
    end.
    
decode_mask_key(Fragment, <<MaskingKey:4/binary, Rest/binary>>) ->
    <<Mask1:8, Mask2:8, Mask3:8, Mask4:8>> = MaskingKey,
    decode_data(Fragment#fragment{mask_key = {Mask1, Mask2, Mask3, Mask4}}, Rest);
decode_mask_key(Fragment, _) -> {error, mask_key_error, Fragment}.

decode_data(Fragment, Bin) ->
    #fragment{payload = Length, mask_key = MaskingKey, data_type = DataType} = Fragment,
    case Bin of
        <<DataBin:Length/binary, Rest/bitstring>> ->
            Data = mask_data(MaskingKey, DataBin),
            case DataType of
                1 ->
                    unicode:characters_to_list(Data);
                2 ->
                    Data
            end,
            OneFragment = Fragment#fragment{data = Data},
            case check_continue(OneFragment) of
                NewFragment when is_record(NewFragment, fragment) ->
                    decode(NewFragment, Rest);
                Other ->
                    Other
            end;
        _ ->
            {error, data_error, Fragment}
    end.

get_continue_data_type() ->
    case get_continue_data() of
        undefined -> 2;
        Data ->
            get_continue_data_type(Data)
    end.
get_continue_data_type(Data) ->
    case is_list(Data) of
        true  -> 1;
        false -> 2
    end.

get_continue_data() ->
    get(continue_data).
save_continue_data(Data) ->
    put(continue_data, Data).
delete_continue_data() ->
    erase(continue_data).

check_continue(Fragment) ->
    #fragment{fragment_sign = FragmentSign, data = Data, fragment_list = FragmentList} = Fragment,
    case FragmentSign of
        unfragmented ->
            #fragment{
                fragment_list = FragmentList ++ [Fragment#fragment.data]
            };
        fragmented_begin ->
            case get_continue_data() =:= undefined of
                true  ->
                    save_continue_data(Data),
                    #fragment{
                        fragment_list = FragmentList
                    };
                false ->
                    {error, continue_error, Fragment}
            end;
        fragmented_continue ->
            case get_continue_data() of
                undefined ->
                    {error, continue_error, Fragment#fragment.fragment_list};
                OldData ->
                    case get_continue_data_type(OldData) of
                        1 ->
                            save_continue_data(OldData ++ Data),
                            #fragment{
                                fragment_list = FragmentList
                            };
                        2 ->
                            save_continue_data(<<OldData/binary, Data/binary>>),
                            #fragment{
                                fragment_list = FragmentList
                            }
                    end
            end;
        fragmented_end ->
            case delete_continue_data() of
                undefined ->
                    {error, continue_error, Fragment};
                OldData ->
                    NewFragment =
                        case get_continue_data_type(OldData) of
                            1 ->
                                Fragment#fragment{data = OldData ++ Data};
                            2 ->
                                Fragment#fragment{data = <<OldData/binary, Data/binary>>}
                        end,
                    #fragment{
                        fragment_list = FragmentList ++ [NewFragment#fragment.data]
                    }
                    
            end
    end.
    
mask_data(MaskingKey, DataBin) ->
    mask_data(MaskingKey, DataBin, 0, <<>>).

mask_data(MaskKeys, <<Bin:8, Rest/binary>>, I, Decode) ->
    J = (I rem 4) + 1,
    Key = erlang:element(J, MaskKeys),
    DecodeByte = Bin bxor Key,
    mask_data(MaskKeys, Rest, I + 1, <<Decode/binary, DecodeByte:8>>);
mask_data(_MaskKeys, _, _, Decode) -> Decode.

%% 打包
encode(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 1, Rsv = 0, OpCode = 1, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.

%% 分包开始
encode_begin(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 0, Rsv = 0, OpCode = 1, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.
%% 分包连续
encode_continue(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 0, Rsv = 0, OpCode = 0, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.
%% 分包结束
encode_end(Msg) ->
    Bin = unicode:characters_to_binary(Msg),
    Fin = 1, Rsv = 0, OpCode = 0, Mask = 0,
    Payload = erlang:byte_size(Bin),
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7, Bin/binary>>.

%% close
encode_close() ->
    Fin = 1, Rsv = 0, OpCode = 8, Mask = 0,
    Payload = 0,
    <<Fin:1, Rsv:3, OpCode:4, Mask:1, Payload:7>>.