-module(connection_reliable_packet_buffer).

-include("connection.hrl").

-compile(export_all).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.
    
insert(BufferedPacket) ->
    <<_Header:?BASE_HEADER_SIZE,Type:?U8,SeqNum:?U16,_Other/binary>> = BufferedPacket,
    case ets:lookup(?TABLE_ID, SeqNum) ->
        [{SeqNum, FindPacket}] -> io:format("Reliable packet with SeqNum=~p is found in ReliablePacketBuffer~n",[SeqNum]);
        [] -> ets:insert(?TABLE_ID, {Key, BufferedPacket}).
    
%void ReliablePacketBuffer::insert(BufferedPacket &p)
%{
%	assert(p.data.getSize() >= BASE_HEADER_SIZE+3);
%	u8 type = readU8(&p.data[BASE_HEADER_SIZE+0]);
%	assert(type == TYPE_RELIABLE);
%	u16 seqnum = readU16(&p.data[BASE_HEADER_SIZE+1]);
%
%	// Find the right place for the packet and insert it there
%
%	// If list is empty, just add it
%	if(m_list.empty())
%	{
%		m_list.push_back(p);
%		// Done.
%		return;
%	}
%	// Otherwise find the right place
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	// Find the first packet in the list which has a higher seqnum
%	for(; i != m_list.end(); i++){
%		u16 s = readU16(&(i->data[BASE_HEADER_SIZE+1]));
%		if(s == seqnum){
%			throw AlreadyExistsException("Same seqnum in list");
%		}
%		if(seqnum_higher(s, seqnum)){
%			break;
%		}
%	}
%	// If we're at the end of the list, add the packet to the
%	// end of the list
%	if(i == m_list.end())
%	{
%		m_list.push_back(p);
%		// Done.
%		return;
%	}
%	// Insert before i
%	m_list.insert_before(i, p);
%}


    
findPacket(SeqNum) ->
    case ets:lookup(?TABLE_ID, SeqNum) of
        [{Key, ReliablePacket}] -> {ok, ReliablePacket};
        []-> {error, not_found}
    end.
    
% RPBSearchResult ReliablePacketBuffer::findPacket(u16 seqnum)
%{
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	for(; i != m_list.end(); i++)
%	{
%		u16 s = readU16(&(i->data[BASE_HEADER_SIZE+1]));
%		/*dout_con<<"findPacket(): finding seqnum="<<seqnum
%				<<", comparing to s="<<s<<std::endl;*/
%		if(s == seqnum)
%			break;
%	}
%	return i;
%}

    
%delete(Pid) ->
%    ets:match_delete(?TABLE_ID, {'_', Pid}).


print() ->
    io:format("SeqNum",[]).

%void ReliablePacketBuffer::print()
%{
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	for(; i != m_list.end(); i++)
%	{
%		u16 s = readU16(&(i->data[BASE_HEADER_SIZE+1]));
%		dout_con<<s<<" ";
%	}
%}

empty() ->
    false.
    
%bool ReliablePacketBuffer::empty()
%{
%	return m_list.empty();
%}

size() ->
    0.
    
%u32 ReliablePacketBuffer::size()
%{
%	return m_list.getSize();
%}

notFound() ->
    ets:last(?TABLE_ID).
    
%RPBSearchResult ReliablePacketBuffer::notFound()
%{
%	return m_list.end();
%}

getFirstSeqnum() ->
    FirstPacket = ets:first(?TABLE_ID),
    <<_Header:?BASE_HEADER_SIZE,_Type:?U8,SeqNum:?U16,_Other/binary>> = FirstPacket,
    SeqNum.

%u16 ReliablePacketBuffer::getFirstSeqnum()
%{
%	if(empty())
%		throw NotFoundException("Buffer is empty");
%	BufferedPacket p = *m_list.begin();
%	return readU16(&p.data[BASE_HEADER_SIZE+1]);
%}

popFirst() ->
    FirstPacket = ets:first(?TABLE_ID),
    <<_Header:?BASE_HEADER_SIZE,_Type:?U8,SeqNum:?U16,_Other/binary>> = FirstPacket,
    ets:delete(?TABLE_ID,SeqNum),
    FirstPacket.

%BufferedPacket ReliablePacketBuffer::popFirst()
%{
%	if(empty())
%		throw NotFoundException("Buffer is empty");
%	BufferedPacket p = *m_list.begin();
%	core::list<BufferedPacket>::Iterator i = m_list.begin();
%	m_list.erase(i);
%	return p;
%}

popSeqnum(SeqNum) ->
    FindPacket = ets:lookup(?TABLE_ID,SeqNum),
    %%<<_Header:?BASE_HEADER_SIZE,_Type:?U8,SeqNum:?U16,_Other/binary>> = FirstPacket,
    ets:delete(?TABLE_ID,SeqNum),
    FindPacket.

%BufferedPacket ReliablePacketBuffer::popSeqnum(u16 seqnum)
%{%
%	RPBSearchResult r = findPacket(seqnum);
%	if(r == notFound()){
%		dout_con<<"Not found"<<std::endl;
%		throw NotFoundException("seqnum not found in buffer");
%	}
%	BufferedPacket p = *r;
%	m_list.erase(r);
%	return p;
%}

void ReliablePacketBuffer::incrementTimeouts(float dtime)
{
	core::list<BufferedPacket>::Iterator i;
	i = m_list.begin();
	for(; i != m_list.end(); i++){
		i->time += dtime;
		i->totaltime += dtime;
	}
}

void ReliablePacketBuffer::resetTimedOuts(float timeout)
{
	core::list<BufferedPacket>::Iterator i;
	i = m_list.begin();
	for(; i != m_list.end(); i++){
		if(i->time >= timeout)
			i->time = 0.0;
	}
}

bool ReliablePacketBuffer::anyTotaltimeReached(float timeout)
{
	core::list<BufferedPacket>::Iterator i;
	i = m_list.begin();
	for(; i != m_list.end(); i++){
		if(i->totaltime >= timeout)
			return true;
	}
	return false;
}

core::list<BufferedPacket> ReliablePacketBuffer::getTimedOuts(float timeout)
{
	core::list<BufferedPacket> timed_outs;
	core::list<BufferedPacket>::Iterator i;
	i = m_list.begin();
	for(; i != m_list.end(); i++)
	{
		if(i->time >= timeout)
			timed_outs.push_back(*i);
	}
	return timed_outs;
}
