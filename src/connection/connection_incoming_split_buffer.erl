%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 

-module(connection_incoming_split_buffer).
-author('Alexander Markevych <rabota.pmr@gmail.com>').     

-export([insert/2, removeUnreliableTimedOuts/2]).

%-define(TABLE_ID, ?MODULE).

init(Table) ->
    ets:new(Table, [public, named_table]),
    ok.

insert(Table, BufferedPacket, Reliable) -> 
    HeaderSize = ?BASE_HEADER_SIZE+7,
    <<_Header:?BASE_HEADER_SIZE, Type:?U8, SeqNum:?U16, ChunkCount:?U16, ChunkNum:?U16, _Other/binary>> = BufferedPacket,
    case ets:lookup(Table, SeqNum) ->
        [{SeqNum, FindPacket}] -> io:format("IncomingSplitPacket with SeqNum=~p is found in ReliablePacketBuffer~n",[SeqNum]);
        [] -> ets:insert(Table, {SeqNum, BufferedPacket}).
%
%   This will throw a GotSplitPacketException when a full
%   split packet is constructed.
%
% SharedBuffer<u8> IncomingSplitBuffer::insert(BufferedPacket &p, bool reliable)
%{
%   u32 headersize = BASE_HEADER_SIZE + 7;
%   assert(p.data.getSize() >= headersize);
%   u8 type = readU8(&p.data[BASE_HEADER_SIZE+0]);
%   assert(type == TYPE_SPLIT);
%   u16 seqnum = readU16(&p.data[BASE_HEADER_SIZE+1]);
%   u16 chunk_count = readU16(&p.data[BASE_HEADER_SIZE+3]);
%   u16 chunk_num = readU16(&p.data[BASE_HEADER_SIZE+5]);
%
%   // Add if doesn't exist
%   if(m_buf.find(seqnum) == NULL)
%   {
%       IncomingSplitPacket *sp = new IncomingSplitPacket();
%       sp->chunk_count = chunk_count;
%       sp->reliable = reliable;
%       m_buf[seqnum] = sp;
%   }
%   
%   IncomingSplitPacket *sp = m_buf[seqnum];
%   
%   // TODO: These errors should be thrown or something? Dunno.
%   if(chunk_count != sp->chunk_count)
%       derr_con<<"Connection: WARNING: chunk_count="<<chunk_count
%               <<" != sp->chunk_count="<<sp->chunk_count
%               <<std::endl;
%   if(reliable != sp->reliable)
%       derr_con<<"Connection: WARNING: reliable="<<reliable
%               <<" != sp->reliable="<<sp->reliable
%               <<std::endl;
%
%   // If chunk already exists, cancel
%   if(sp->chunks.find(chunk_num) != NULL)
%       throw AlreadyExistsException("Chunk already in buffer");
%   
%   // Cut chunk data out of packet
%   u32 chunkdatasize = p.data.getSize() - headersize;
%   SharedBuffer<u8> chunkdata(chunkdatasize);
%   memcpy(*chunkdata, &(p.data[headersize]), chunkdatasize);
%   
%   // Set chunk data in buffer
%   sp->chunks[chunk_num] = chunkdata;
%   
%   // If not all chunks are received, return empty buffer
%   if(sp->allReceived() == false)
%       return SharedBuffer<u8>();
%
%   // Calculate total size
%   u32 totalsize = 0;
%   core::map<u16, SharedBuffer<u8> >::Iterator i;
%   i = sp->chunks.getIterator();
%   for(; i.atEnd() == false; i++)
%   {
%       totalsize += i.getNode()->getValue().getSize();
%   }
%   
%   SharedBuffer<u8> fulldata(totalsize);
%
%   // Copy chunks to data buffer
%   u32 start = 0;
%   for(u32 chunk_i=0; chunk_i<sp->chunk_count;
%           chunk_i++)
%   {
%       SharedBuffer<u8> buf = sp->chunks[chunk_i];
%       u16 chunkdatasize = buf.getSize();
%       memcpy(&fulldata[start], *buf, chunkdatasize);
%       start += chunkdatasize;;
%   }
%
%   // Remove sp from buffer
%   m_buf.remove(seqnum);
%   delete sp;
%
%   return fulldata;
% }

removeUnreliableTimedOuts(Table) -> ok.

% void IncomingSplitBuffer::removeUnreliableTimedOuts(float dtime, float timeout)
% {
%   core::list<u16> remove_queue;
%   core::map<u16, IncomingSplitPacket*>::Iterator i;
%   i = m_buf.getIterator();
%   for(; i.atEnd() == false; i++)
%   {
%       IncomingSplitPacket *p = i.getNode()->getValue();
%       // Reliable ones are not removed by timeout
%       if(p->reliable == true)
%           continue;
%       p->time += dtime;
%       if(p->time >= timeout)
%           remove_queue.push_back(i.getNode()->getKey());
%   }
%   core::list<u16>::Iterator j;
%   j = remove_queue.begin();
%   for(; j != remove_queue.end(); j++)
%   {
%       dout_con<<"NOTE: Removing timed out unreliable split packet"
%               <<std::endl;
%       delete m_buf[*j];
%       m_buf.remove(*j);
%   }
% }
