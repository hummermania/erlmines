-module(connection_reliable_packet_buffer).

-include("connection.hrl").

-compile(export_all).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.
    
insert(#buffered_packet{data=BufferedPacket, time=_Time, totaltime=_TotalTime, address = _Address} = BPrec) ->
    <<_Header:?BASE_HEADER_SIZE/binary, Type:?U8,  SeqNum:?U16,_Other/binary>> = BufferedPacket,
    io:format("Type=~p, SeqNum=~p~n",[Type,SeqNum]),
    case ets:lookup(?TABLE_ID, SeqNum) of 
        [{SeqNum, FindPacket}] -> 
            io:format("Reliable packet with SeqNum=~p is found in ReliablePacketBuffer~n",[SeqNum]),
            {exists,FindPacket};
        [] -> ets:insert(?TABLE_ID, {SeqNum, BPrec}),
            {ok,BPrec}
    end.

test_insert() ->
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,00,34,77,77,77,77]), time=0, totaltime=0}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,01,11,77,77,77,77]), time=0, totaltime=0}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,02,62,77,77,77,77]), time=0, totaltime=0}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,67,02,77,77,77,77]), time=0, totaltime=0}),
    insert(#buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,00,10,77,77,77,77]), time=0, totaltime=0}).
    
  
findPacket(SeqNum) ->
    case ets:lookup(?TABLE_ID, SeqNum) of
        [{SeqNum, FindPacket}] -> {ok,  FindPacket};
        []-> {error, not_found}
    end.

test_findPacket()->
    findPacket(15874). % 8704, 2817, 579
    
print() ->
    ets:foldl(fun({SeqNum, Packet},AccIn) ->
      io:format("SeqNum=~p: ~p ~n", [SeqNum,Packet]),
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

incrementTimeouts(Dtime) ->
    ets:foldl(fun({SeqNum, Packet},AccIn) ->
        #buffered_packet{data = Data, time = Time, totaltime = TotalTime, address = Address} = Packet,
        ets:update_element(?TABLE_ID,SeqNum,
            {2,#buffered_packet{data=Data,time = Time + Dtime, totaltime = TotalTime + Dtime, address = Address}}),
        AccIn
    end, end_list, ?TABLE_ID).
    
test_increment()->
    init(),
    test_insert(),
    print(),
    incrementTimeouts(35),
    print().
    
resetTimedOuts(Timeout) ->
    ets:foldl(fun({SeqNum, Packet},AccIn) ->
        #buffered_packet{data = Data, time = Time, totaltime = TotalTime, address = Address} = Packet,
        if Time >= Timeout ->       
            ets:update_element(?TABLE_ID,SeqNum,
                {2,#buffered_packet{data=Data,time = 0, totaltime = TotalTime, address = Address}});
            true -> ok
        end,        
        AccIn
    end, end_list, ?TABLE_ID).
    

anyTotaltimeReached(Timeout) -> 
    %ets:select(?TABLE_ID, ets:fun2ms(fun({SeqNum,#buffered_packet{totaltime = TotalTime}}) when TotalTime >= Timeout -> true end)).
    %ets:select(?TABLE_ID, 
    %    ets:fun2ms(fun({Seq,#buffered_packet{data = _, time = _, totaltime = TotalTime, address = _}}) -> TotalTime end)).
    
    %TODO This construction need to refactoring
    case ets:select(?TABLE_ID, [{{'_',#buffered_packet{data='_',time='_',totaltime='$1',address='_'}},[{'>=','$1',Timeout}],['$1']}]) of
        [] -> false; 
        _ -> true  % or we must check list size
    end.
    
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


getTimedOuts(Timeout) ->
    % TODO - Check the returning value: list of buffered_packet, or SeqNum 
    ets:select(?TABLE_ID, [{{'$1',#buffered_packet{data='_',time='$2',totaltime='_',address='_'}},[{'>=','$2',Timeout}],['$1']}]).

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
