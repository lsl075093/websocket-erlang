%%%-------------------------------------------------------------------
%% @doc websocket top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(websocket_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        {server, {server, start_link, [1234]}, transient, 2000, worker, [server]},
        {wuzi_mgr, {wuzi_mgr, start_link, []}, transient, 2000, worker, [wuzi_mgr]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
