%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
-module(connection_peers).

-compile(export_all).

-define(RPB, connection_reliable_packet_buffer).
-define(ISB, connection_incoming_split_buffer).

-record(peer,{
  address,
  id,
  timeout_counter,
  ping_timer,
  resend_timeout,
  avg_rtt,
  has_sent_with_id,
  m_sendtime_accu,
  m_max_packets_per_second,
  m_num_sent,
  m_max_num_sent
 }).


init() -> 
    ets:new(peers, [ordered_set, public, named_table]),
    ok.

new(Address) ->
    PeerId = case ets:match(peers,{{used,false},'$1'}) of
        [] -> % If we can't find not used peer id's
            case ets:last(peers) of % try get the last PeerId
                LastKey when is_integer(LastKey) -> 
                    LastKey + 1;
                '$end_of_table' -> 2   % If peer list is empty-set Id=2
            end;
        [[#peer{id=UnusedPeerId}]] -> UnusedPeerId
    end,
    ets:insert(peers,
        {PeerId,{
            {used,true},
                #peer{
                    address = Address,
                    id = PeerId,
                    timeout_counter = 0.0,
                    ping_timer = 0.0,
                    resend_timeout=0.5,
                    avg_rtt = -1.0,
                    has_sent_with_id = false,
                    m_sendtime_accu = 0,
                    m_max_packets_per_second = 10,
                    m_num_sent = 0,
                    m_max_num_sent = 0
                    }
                }
        }).
        
delete(PeerId) ->
    ets:delete(peers,PeerId).

lookup(PeerId) ->
    ets:lookup(peers,PeerId).


