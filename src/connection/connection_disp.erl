%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
      
-module(connection_disp).

-behaviour(gen_server).


-export([start_link/0
		, init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export([set_socket/2, readPeerId/1,readChannel/1]).
-export([makePacket/5,
         makeOriginalPacket/1, 
         makeSplitPacket/3,
         makeAutoSplitPacket/3,
         makeReliablePacket/2,
         test/0]).

-include("connection.hrl").

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%% Server functions
init(_Args) ->
	erlang:process_flag(trap_exit, true),
	io:format("===connection:init===~n",[]),
    case gen_udp:open(30000,[binary]) of
        {ok,Socket} -> {ok, [Socket]};
        {error, Reason} -> {stop, Reason}
    end.

set_socket(Peer, Socket) when is_pid(Peer) andalso is_port(Socket) ->
  gen_tcp:controlling_process(Socket, Peer),
  gen_server:call(Peer, {set_socket, Socket}).
        
handle_call({msg,Msg},From,State) ->
    io:format("===connection:handle_call===~p, ~p, ~p~n",[Msg,From,State]),
	{reply, ok, State};

handle_call({set_socket, Socket}, _From, State) ->
    inet:setopts(Socket, [{packet,raw},{active,once}]),
    {reply, ok, State}.

handle_cast(calc, State) ->
    io:format("result 2+2=4~n"),
    {noreply, State};
handle_cast(calcbad, State) ->
    io:format("result 1/0~n"),
    1 / 0,
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("===connection:handle_cast===~n",[]),
    {noreply,State}.

handle_info({udp, Socket, _IP, _InPortNo, _Bin} = UdpMsg, State) ->
    inet:setopts(Socket, [{active,once}]),
    process_packet(UdpMsg),
    %%{ok, Decoder1, Frames} = decode(Bin, Decoder),
    %%[Consumer ! Frame || Frame <- Frames],
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_packet({udp,_Socket,Host,Port,Bin}) ->
    io:format("========================================~n",[]),
    io:format("Host = ~p, Port = ~p~n",[Host,Port]), 
    <<_ProtocolId:?U32, SenderPeerId:?U16, Channel:?U8, 
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

type_control(<<_ControlType:?U8,_Data/binary>>)-> ok.

type_original(_Data)-> ok.
type_split(<<_SeqNum:?U16,_ChunkCount:?U16,_ChunkNum:?U16,_Data/binary>>)-> ok.
type_reliable(<<_SeqNum:?U16,_Data/binary>>)-> ok.


ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

readPeerId(PacketData) ->
    <<_P:?U32,SenderPeerId:?U16,_Other/binary>> = PacketData,
    SenderPeerId.

readChannel(PacketData) ->
    <<_P:?U32,_S:?U16,Channel:?U8,_Other/binary>> = PacketData,
    Channel.

% This adds the base headers to the data and makes a packet out of it
makePacket(Address, Data, ProtocolId,SenderPeerId,Channel)->
    #buffered_packet{
        address=Address,
        data = <<ProtocolId:?U32,SenderPeerId:?U16,Channel:?U8,Data/binary>>
        }.
% Add the TYPE_ORIGINAL header to the data
makeOriginalPacket(Data) ->
    #original_packet{ data = <<?TYPE_ORIGINAL:?U8,Data/binary>> }.

% Split data in chunks and add TYPE_SPLIT headers to them
makeSplitPacket(Data,ChunkSizeMax,SeqNum) ->
    ChunkHeaderSize = 7,
    MaxDataSize = ChunkSizeMax - ChunkHeaderSize,
    ChunkCount=ceiling(byte_size(Data)/MaxDataSize),
    Chunks = makeSplitPacketList(MaxDataSize, SeqNum, ChunkCount, 0, Data),
    Chunks.

makeSplitPacketList(_MaxDataSize, _SeqNum, ChunkCount, ChunkCount, _Data) ->[];
makeSplitPacketList(MaxDataSize, SeqNum, ChunkCount, ChunkNum, Data) ->
    DataSize = byte_size(Data),
    ChunkDataSize = case MaxDataSize - DataSize < 0 of
        true -> MaxDataSize;
        false -> DataSize
    end,
    <<ChunkData:ChunkDataSize/binary,Tile/binary>> = Data,
    %%ChunkNum1 = ChunkNum+1, 
    [<<?TYPE_SPLIT:?U8, SeqNum:?U16, ChunkCount:?U16, ChunkNum:?U16,ChunkData/binary>>|makeSplitPacketList(MaxDataSize,SeqNum,ChunkCount,ChunkNum+1,Tile)].

test() ->
    makeSplitPacket(list_to_binary([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]),9,65500).
    
% Depending on size, make a TYPE_ORIGINAL or TYPE_SPLIT packet
% Increments split_seqnum if a split packet is made
makeAutoSplitPacket(_Data,_ChunkSizeMax,_SplitSeqNum) -> 
    todo.
    
% core::list<SharedBuffer<u8> > makeAutoSplitPacket(
%       SharedBuffer<u8> data,
%       u32 chunksize_max,
%       u16 &split_seqnum)
%{
%   u32 original_header_size = 1;
%   core::list<SharedBuffer<u8> > list;
%   if(data.getSize() + original_header_size > chunksize_max)
%   {
%       list = makeSplitPacket(data, chunksize_max, split_seqnum);
%       split_seqnum++;
%       return list;
%   }
%   else
%   {
%       list.push_back(makeOriginalPacket(data));
%   }
%   return list;
%}
%
    
% Add the TYPE_RELIABLE header to the data
makeReliablePacket(Data, SeqNum) ->
    #reliable_packet{ data = <<?TYPE_RELIABLE:?U8,SeqNum:?U16,Data/binary>> }.

