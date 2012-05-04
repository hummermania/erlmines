%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
-module(erlmines_sup).

-behaviour(supervisor).

-export([start/0, start_link/1, init/1]).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).
  
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  
init([]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
    Type = worker,       % worker | supervisor

    AChild = {erlmines_sup, % used to identify the child spec internally by the supervisor
          {erlmines, start_link, []}, % StartFun = {M, F, A}
          Restart, Shutdown, Type, 
          [erlmines]}, % Modules  = [Module] | dynamic

    {ok, {SupFlags, [AChild]}}.
 
  
%  {ok, {{one_for_one, 100, 300},
%    [{erlmines_sup,
%       {erlmines, start_link, []},
%       permanent, 10000, worker, [erlmines]}
%    ]}}.
