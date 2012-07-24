-module(erlmines).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).
        
%% Client API
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
    
%% Server functions
init(_Args) ->
	io:format("===erlmines:init===~n",[]),
	erlang:process_flag(trap_exit, true),
	io:format("erlmines has started (~w)~n", [self()]),
	{ok,[]}.

    
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    %error_logger:info_report("handle_cast"),
    {noreply,State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("===erlmines:terminate===~n",[]),
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






