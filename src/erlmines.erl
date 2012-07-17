-module(erlmines).

-behaviour(gen_server).
-include("connection.hrl").

-export([start_link/0]).
-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).
        
%% @type udp_server_option() =
%%  {option(), port(), max_restarts(), time(), shutdown(),recv_length(), recv_timeout()}.
%%  A data structure holding the options.
%% @type option()       = [term()].
%% @type port()         = integer().
%% @type max_restarts() = integer().
%% @type time()         = integer().
%% @type shutdown()     = integer().
%% @type recv_length()  = integer().
%% @type recv_timeout() = integer() | infinity.
-record(udp_server_option, {
  option = [binary],
  port = 4000,
  max_restarts = 3,
  time = 60,
  shutdown = 2000,
  recv_length = 0,
  recv_timeout = infinity
}).

%% Client API
start_link() ->
    gen_server:start_link(?MODULE, [], []).
    
%% Server functions
init([]) -> 
    case gen_udp:open(30000,[binary]) of
        {ok,Socket} -> {ok, [Socket]};
        {error, Reason} -> {stop, Reason}
    end.

set_socket(Peer, Socket) when is_pid(Peer) andalso is_port(Socket) ->
  gen_tcp:controlling_process(Socket, Peer),
  gen_server:call(Peer, {set_socket, Socket}).
        
handle_call({set_socket, Socket}, _From, State) ->
  inet:setopts(Socket, [{packet,raw},{active,once}]),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
    error_logger:info_report("handle_cast"),
    {noreply,State}.

handle_info({udp, Socket, _IP, _InPortNo, Bin} = UdpMsg, State) ->
  inet:setopts(Socket, [{active,once}]),
  process_packet(UdpMsg),
  %%{ok, Decoder1, Frames} = decode(Bin, Decoder),
  %%[Consumer ! Frame || Frame <- Frames],
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_packet({udp,Socket,Host,Port,Bin}) ->
    io:format("========================================~n",[]),
    % io:format("Host = ~p, Port = ~p~n",[Host,Port]), 
    <<ProtocolId:?U32, SenderPeerId:?U16, Channel:?U8, 
        PacketType:?U8, Data/binary>> = Bin,
    case PacketType of
        ?TYPE_CONTROL -> Type = "TYPE_CONTROL (0)",
            type_control(Data);
        ?TYPE_ORIGINAL -> Type = "TYPE_ORIGINAL(1)",
            type_original(Data);
        ?TYPE_SPLIT -> Type = "TYPE_SPLIT(2)",
            type_split(Data);
        ?TYPE_RELIABLE -> Type = "TYPE_RELIABLE(2)",
            type_reliable(Data)
    end,        
    %io:format("ProtocolId: ~p, SenderPeerId:~p, Channel:~p, PacketType: ~p,~n Data:~p~n",[ProtocolId,SenderPeerId,Channel,Type,Data]).
    io:format("SenderPeerId:~p, Channel:~p, PacketType: ~p,~n Data:~p~n",
        [SenderPeerId,Channel,Type,Data]).

type_control(<<ControlType:?U8,Data/binary>>)-> ok.

type_original(Data)-> ok.
type_split(<<SeqNum:?U16,ChunkCount:?U16,ChunkNum:?U16,Data/binary>>)-> ok.
type_reliable(<<SeqNum:?U16,Data/binary>>)-> ok.







