%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 

-module(erlmines_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
  erlmines_sup:start_link().
  
stop(_State) ->
  ok.
