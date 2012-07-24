%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
-module(connection_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
  
init(_Args) ->
	io:format("===connection_sup:init===~n",[]),
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
	Type = worker,       % worker | supervisor

	Connection = {connection_dispatcher, % used to identify the child spec internally by the supervisor
          {connection_disp, start_link, []}, % StartFun = {M, F, A}
          Restart, Shutdown, Type, 
          [connection_disp]}, % Modules  = [Module] | dynamic

    {ok, {SupFlags, [Connection]}}.
