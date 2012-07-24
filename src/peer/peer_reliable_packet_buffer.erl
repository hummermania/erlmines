%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
-module(peer_reliable_packet_buffer).
-include("connection.hrl").

-export([init/1, insert/2, findPacket/2, print/1, empty/1, 
    size/1, notFound/1, getFirstSeqnum/1,
    popFirst/1, popSeqnum/2,
    incrementTimedOuts/2, resetTimedOuts/2,
    anyTotaltimeReached/2, getTimedOuts/2]).
         
-export([test_insert/1, test_findPacket/1, test_increment/0]).

%-define(TABLE_ID, ?MODULE).

init(Table) ->
    ets:new(Table, [public, named_table]).
    
insert(Table, #buffered_packet{data=BufferedPacket, time=_Time, totaltime=_TotalTime, address = _Address} = BPrec) ->
    <<_Header:?BASE_HEADER_SIZE/binary, Type:?U8,  SeqNum:?U16,_Other/binary>> = BufferedPacket,
    io:format("Type=~p, SeqNum=~p~n",[Type,SeqNum]),
    case ets:lookup(Table, SeqNum) of 
        [{SeqNum, FindPacket}] -> 
            io:format("Reliable packet with SeqNum=~p is found in ReliablePacketBuffer~n",[SeqNum]),
            {exists,FindPacket};
        [] -> ets:insert(Table, {SeqNum, BPrec}),
            {ok,BPrec}
    end.

test_insert(Table) ->
    insert(Table, #buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,00,34,77,77,77,77]), time=0, totaltime=0}),
    insert(Table, #buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,01,11,77,77,77,77]), time=0, totaltime=0}),
    insert(Table, #buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,02,62,77,77,77,77]), time=0, totaltime=0}),
    insert(Table, #buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,67,02,77,77,77,77]), time=0, totaltime=0}),
    insert(Table, #buffered_packet{data=list_to_binary([1,2,3,4,5,6,7,33,00,10,77,77,77,77]), time=0, totaltime=0}).
    
  
findPacket(Table, SeqNum) ->
    case ets:lookup(Table, SeqNum) of
        [{SeqNum, FindPacket}] -> {ok,  FindPacket};
        []-> {error, not_found}
    end.

test_findPacket(Table)->
    findPacket(Table, 15874). % 8704, 2817, 579
    
print(Table) ->
    ets:foldl(fun({SeqNum, Packet},AccIn) ->
      io:format("SeqNum=~p: ~p ~n", [SeqNum,Packet]),
      AccIn
   end, end_list, Table).

empty(Table) ->
    Size = ets:info(Table, size),
    case Size of
        0 -> true;
        _ -> Size
    end.

size(Table) ->
    ets:info(Table, size).
 
% TODO Check the returning value    
notFound(Table) ->
    ets:last(Table).
    
%RPBSearchResult ReliablePacketBuffer::notFound()
%{
%   return m_list.end();
%}

getFirstSeqnum(Table) ->
    ets:first(Table).

popFirst(Table) ->
    FirstSeqNum = ets:first(Table),
    #buffered_packet{data=FirstPacket} = ets:lookup(Table, FirstSeqNum),
    <<_Header:?BASE_HEADER_SIZE,_Type:?U8,SeqNum:?U16,_Other/binary>> = FirstPacket,
    ets:delete(Table, SeqNum),
    #buffered_packet{data=FirstPacket}.

popSeqnum(Table, SeqNum) ->
    FindPacket = ets:lookup(Table, SeqNum),
    ets:delete(Table, SeqNum),
    FindPacket.

incrementTimedOuts(Table, Dtime) ->
    ets:foldl(fun({SeqNum, Packet},AccIn) ->
        #buffered_packet{data = Data, time = Time, totaltime = TotalTime, address = Address} = Packet,
        ets:update_element(Table, SeqNum,
            {2,#buffered_packet{data=Data,time = Time + Dtime, totaltime = TotalTime + Dtime, address = Address}}),
        AccIn
    end, end_list, Table).
    
test_increment()->
    init(rpb_test),
    test_insert(rpb_test),
    print(rpb_test),
    incrementTimedOuts(rpb_test, 35),
    print(rpb_test).
    
resetTimedOuts(Table, Timeout) ->
    ets:foldl(fun({SeqNum, Packet},AccIn) ->
        #buffered_packet{data = Data, time = Time, totaltime = TotalTime, address = Address} = Packet,
        if Time >= Timeout ->       
            ets:update_element(Table, SeqNum,
                {2,#buffered_packet{data=Data,time = 0, totaltime = TotalTime, address = Address}});
            true -> ok
        end,        
        AccIn
    end, end_list, Table).
    

anyTotaltimeReached(Table, Timeout) -> 
    %ets:select(Table, ets:fun2ms(fun({SeqNum,#buffered_packet{totaltime = TotalTime}}) when TotalTime >= Timeout -> true end)).
    %ets:select(Table, 
    %    ets:fun2ms(fun({Seq,#buffered_packet{data = _, time = _, totaltime = TotalTime, address = _}}) -> TotalTime end)).
    
    %TODO This construction need to refactoring
    case ets:select(Table, [{{'_',#buffered_packet{data='_',time='_',totaltime='$1',address='_'}},[{'>=','$1',Timeout}],['$1']}]) of
        [] -> false; 
        _ -> true  % or we must check list size
    end.
    
%bool ReliablePacketBuffer::anyTotaltimeReached(float timeout)
%{
%   core::list<BufferedPacket>::Iterator i;
%   i = m_list.begin();
%   for(; i != m_list.end(); i++){
%       if(i->totaltime >= timeout)
%           return true;
%   }
%   return false;
%}


getTimedOuts(Table, Timeout) ->
    % TODO - Check the returning value: list of buffered_packet, or SeqNum 
    ets:select(Table, [{{'$1',#buffered_packet{data='_',time='$2',totaltime='_',address='_'}},[{'>=','$2',Timeout}],['$1']}]).

% core::list<BufferedPacket> ReliablePacketBuffer::getTimedOuts(float timeout)
%{
%   core::list<BufferedPacket> timed_outs;
%   core::list<BufferedPacket>::Iterator i;
%   i = m_list.begin();
%   for(; i != m_list.end(); i++)
%   {
%       if(i->time >= timeout)
%           timed_outs.push_back(*i);
%   }
%   return timed_outs;
%}
