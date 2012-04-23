%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
      
-module(connection).
-author('Alexander Markevych <rabota.pmr@gmail.com>').
-include("connection.hrl").

-export([makePacket/5, makeSplitPacket/3, test/0]).

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

makeSplitPacketList(MaxDataSize, SeqNum, ChunkCount, ChunkCount, Data) ->[];
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
makeAutoSplitPacket(Data,ChunkSizeMax,SplitSeqNum) -> 
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

