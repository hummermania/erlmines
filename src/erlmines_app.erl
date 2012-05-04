%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 

-module(erlmines_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  erlmines_sup:start_link(StartArgs).
  
stop(_State) ->
  ok.
