%%%-------------------------------------------------------------------
%%% @author liushl
%%% @doc
%%%     获得微信openid
%%% @end
%%%-------------------------------------------------------------------
-module(auth_code2session).
-author("liushl").

-include("socket_conn.hrl").

%% API
-export([handle_event/3]).


handle_event(1, State, Bin) ->
    {APPID, Rest} = util:read_string(Bin),
    {AppSerect, Rest1} = util:read_string(Rest),
    {Code, _} = util:read_string(Rest1),
    case request_weixin_server(APPID, AppSerect, Code) of
        {ok, OpenId} ->
            util:send_to_socket(State#socket_conn.socket, <<1:32>>),
            State#socket_conn{open_id = OpenId};
        {error, Err} ->
            erlang:display({?MODULE, ?LINE, Err}),
            State
    end;
handle_event(_, State, _Info) ->
    erlang:display({?MODULE, ?LINE, no_match, _Info}),
    State.


request_weixin_server(APPID, AppSerect, Code) ->
    erlang:display({?MODULE, ?LINE, APPID, AppSerect, Code}),
    Url = lists:concat([
        "https://api.weixin.qq.com/sns/jscode2session?appid=", APPID,
        "&secret=", AppSerect,
        "&js_code=", Code,
        "&grant_type=authorization_code"
    ]),
    Res = case httpc:request(get, {Url, []}, [{timeout, 3000}], [{sync, true}]) of
        {ok, {_StatusLine, _Headers, Body}} ->
            {ok, Body};
        {ok, {_Status, Body}} ->
            {ok, Body};
        {ok, Body} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end,
    case Res of
        {ok, Data} ->
            case rfc4627:decode(Data) of
                {ok, {obj, Json}, _} ->
                    case lists:keyfind("openid", 1, Json) of
                        {_, OpenId} ->
                            {ok, OpenId};
                        _ ->
                            erlang:display({?MODULE, ?LINE, Json}),
                            {error, no_open_id_return}
                    end;
                {error, Err} ->
                    {error, Err}
            end;
        {error, Err} ->
            {error, Err}
    end.