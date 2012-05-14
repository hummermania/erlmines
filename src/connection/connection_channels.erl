%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 
-module(connection_channels).

-compile(export_all).

-define(RPB, connection_reliable_packet_buffer).
-define(ISB, connection_incoming_split_buffer).

init(ChannelID) ->
    ets:new(channels,[public, named_table]),
    ?RPB:init(list_to_atom(atom_to_list(incoming_reliables_) ++ integer_to_list(ChannelID))),
    ?RPB:init(list_to_atom(atom_to_list(outgoing_reliables_) ++ integer_to_list(ChannelID))),
    ?ISB:init(list_to_atom(atom_to_list(incoming_splits_) ++ integer_to_list(ChannelID))).



