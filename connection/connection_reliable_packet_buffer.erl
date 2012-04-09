-module(connection_reliable_packet_buffer).

-include("connection.hrl").

-compile(export_all).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.
    
insert(#buffered_packet{data=BufferedPacket}) ->
    <<_Header:?BASE_HEADER_SIZE/binary, Type:?U8,  SeqNum:?U16,_Other/binary>> = BufferedPacket,
    io:format("Type=~p, SeqNum=~p~n",[Type,SeqNum]),
    case ets:lookup(?TABLE_ID, SeqNum) of 
        [{SeqNum, FindPacket}] -> 
            io:format("Reliable packet with SeqNum=~p is found in ReliablePacketBuffer~n",[SeqNum]),
            {exists,FindPacket};
        [] -> ets:insert(?TABLE_ID, {SeqNum, #buffered_packet{data=BufferedPacket}}),
            {ok,#buffered_packet{data=BufferedPacket}}
    end.

test_insert() ->
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,00,34,77,77,77,77])}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,01,11,77,77,77,77])}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,02,62,77,77,77,77])}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,67,02,77,77,77,77])}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,00,10,77,77,77,77])}).
    
  
findPacket(SeqNum) ->
    case ets:lookup(?TABLE_ID, SeqNum) of
        [{SeqNum, FindPacket}] -> {ok,  FindPacket};
        []-> {error, not_found}
    end.

test_findPacket()->
    findPacket(15874). % 8704, 2817, 579
    
print() ->
    ets:foldl(fun({SeqNum, _Packet},AccIn) ->
      io:format("~p ~n", [SeqNum]),
      AccIn
   end, end_list, ?TABLE_ID).

empty() ->
    Size = ets:info(?TABLE_ID,size),
    case Size of
        0 -> true;
        _ -> Size
    end.

size() ->
    ets:info(?TABLE_ID,size).
    
notFound() ->
    ets:last(?TABLE_ID).
    
%RPBSearchResult ReliablePacketBuffer::notFound()
%{
%	return m_list.end();
%}

getFirstSeqnum() ->
    ets:first(?TABLE_ID).

popFirst() ->
    FirstSeqNum = ets:first(?TABLE_ID),
    #buffered_packet{data=FirstPacket} = ets:lookup(?TABLE_ID,FirstSeqNum),
    <<_Header:?BASE_HEADER_SIZE,_Type:?U8,SeqNum:?U16,_Other/binary>> = FirstPacket,
    ets:delete(?TABLE_ID,SeqNum),
    #buffered_packet{data=FirstPacket}.

popSeqnum(SeqNum) ->
    FindPacket = ets:lookup(?TABLE_ID,SeqNum),
    ets:delete(?TABLE_ID,SeqNum),
    FindPacket.

% TODO
incrementTimeouts(_Dtime) -> ok.
    %ets:update_counter(?TABLE_ID,SeqNum,[{}]).
    
%void ReliablePacketBuffer::incrementTimeouts(float dtime)
%{
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	for(; i != m_list.end(); i++){
%		i->time += dtime;
%		i->totaltime += dtime;
%	}
%}


% TODO
resetTimedOuts(Timeout) ->
    ets:update_counter().
    
%void ReliablePacketBuffer::resetTimedOuts(float timeout)
%{
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	for(; i != m_list.end(); i++){
%		if(i->time >= timeout)
%			i->time = 0.0;
%	}
%}


% TODO
anyTotaltimeReached(Timeout) -> 
    true.
    
%bool ReliablePacketBuffer::anyTotaltimeReached(float timeout)
%{
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	for(; i != m_list.end(); i++){
%		if(i->totaltime >= timeout)
%			return true;
%	}
%	return false;
%}


getTimedOuts(timeout) ->
    [].

% core::list<BufferedPacket> ReliablePacketBuffer::getTimedOuts(float timeout)
%{
%	core::list<BufferedPacket> timed_outs;
%	core::list<BufferedPacket>::Iterator i;
%	i = m_list.begin();
%	for(; i != m_list.end(); i++)
%	{
%		if(i->time >= timeout)
%			timed_outs.push_back(*i);
%	}
%	return timed_outs;
%}
