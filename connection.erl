-module(connection).

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

makePacket(Address, Data, ProtocolId,SenderPeerId,Channel)->
	#buffered_packet{
		address=Address,
		data = <<ProtocolId:?U32,SenderPeerId:?U16,Channel:?U8,Data/binary>>
		}.

makeOriginalPacket(Data) ->
	#original_packet{ data = <<?TYPE_ORIGINAL:?U8,Data/binary>>	}.


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

%core::list<SharedBuffer<u8> > makeSplitPacket(
		%SharedBuffer<u8> data,
		%u32 chunksize_max,
		%u16 seqnum)
%{
	%// Chunk packets, containing the TYPE_SPLIT header
	%core::list<SharedBuffer<u8> > chunks;
	
	%u32 chunk_header_size = 7;
	%u32 maximum_data_size = chunksize_max - chunk_header_size;
	%u32 start = 0;
	%u32 end = 0;
	%u32 chunk_num = 0;
	%do{
		%end = start + maximum_data_size - 1;
		%if(end > data.getSize() - 1)
			%end = data.getSize() - 1;
		
		%u32 payload_size = end - start + 1;
		%u32 packet_size = chunk_header_size + payload_size;

		%SharedBuffer<u8> chunk(packet_size);
		
		%writeU8(&chunk[0], TYPE_SPLIT);
		%writeU16(&chunk[1], seqnum);
		%// [3] u16 chunk_count is written at next stage
		%writeU16(&chunk[5], chunk_num);
		%memcpy(&chunk[chunk_header_size], &data[start], payload_size);

		%chunks.push_back(chunk);
		
		%start = end + 1;
		%chunk_num++;
	%}
	%while(end != data.getSize() - 1);

	%u16 chunk_count = chunks.getSize();

	%core::list<SharedBuffer<u8> >::Iterator i = chunks.begin();
	%for(; i != chunks.end(); i++)
	%{
		%// Write chunk_count
		%writeU16(&((*i)[3]), chunk_count);
	%}

	%return chunks;
%}

