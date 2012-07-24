%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
-module(erlmines_sup).

-behaviour(supervisor).

-export([start_link/0, init/1,
		 shutdown/0 
		% start_shell/0
		]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%start_shell() ->
%    {ok, Pid} = supervisor:start_link(erlmines_sup, []),
%    unlink(Pid).
  
init(_Args) ->
	io:format("===erlmines_sup:init===~n",[]),
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = infinity,     % brutal_kill | int() >= 0 | infinity
	Type = supervisor,       % worker | supervisor

%    Erlmines = {erlmines_main, % used to identify the child spec internally by the supervisor
%          {erlmines, start_link, []}, % StartFun = {M, F, A}
%          Restart, Shutdown, Type, 
%          [erlmines]}, 
	
	Connection_sup = {erlmines_connection_sup, 
          {connection_sup, start_link, []},
          Restart, Shutdown, Type, 
          [connection_sup]}, 

    %%{ok, {SupFlags, [Erlmines, Connection_sup]}}.
	%, ?SUPERVISOR_LINK(connection_sup)]}}.
    {ok, {SupFlags, [Connection_sup]}}.


% supervisor can be shutdown by calling exit(SupPid,shutdown)
% or, if it's linked to its parent, by parent calling exit/1.
shutdown() ->
     exit(whereis(?MODULE), shutdown).
     % or
     % exit(normal).