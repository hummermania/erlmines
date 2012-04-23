-module(erlmines).

%%-behaviour(gen_server).
-include("erlmines.hrl").

-export([server/1]).

server(Port) ->
    {ok,Socket} = gen_udp:open(Port,[binary]),
    listen(Socket).

listen(Socket) ->
    receive
        %%{udp,Socket,Host,Port,Bin} = Message ->
        {udp,Socket,_,_,Bin} ->
          io:format("========================================~n",[]),
          io:format("Data= ~p~n",[Bin]), 
          process_packet(Bin),
          listen(Socket)          
    after 10000 ->
		gen_udp:close(Socket),
		{exit,timeout}     
	end.
    
    
process_packet(<<ProtocolId:?U32, SenderPeerId:?U16, Channel:?U8, PacketType:?U8, Data/binary>>) ->
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
	io:format("ProtocolId: ~p, SenderPeerId:~p, Channel:~p, PacketType: ~p, Data:~p~n",[ProtocolId,SenderPeerId,Channel,Type,Data]).

type_control(<<ControlType:?U8,Data/binary>>)-> ok.

type_original(Data)-> ok.
type_split(<<SeqNum:?U16,ChunkCount:?U16,ChunkNum:?U16,Data/binary>>)-> ok.
type_reliable(<<SeqNum:?U16,Data/binary>>)-> ok.







