%%%-------------------------------------------------------------------
%% @doc websocket public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket_app).

-behaviour(application).

-export([start/2, start/0, stop/1]).

start(_StartType, _StartArgs) ->
    websocket_sup:start_link().

start() ->
    websocket_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
