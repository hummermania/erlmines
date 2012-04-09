
-define(U8,  8/unsigned-little-integer).
-define(U16, 16/unsigned-little-integer).
-define(U32, 32/unsigned-little-integer).

-record(buffered_packet, {
  data,  	 % Data of the packet, including headers
  time, 	 % Seconds from buffering the packet or re-sending
  totaltime, % Seconds from buffering the packet
  address    % Sender or destination
}).

-record(original_packet, {
  data,
  address
}).

-record(reliable_packet, {
  data
}).

-record(incoming_split_packet, {
	% Key is chunk number, value is data without headers
	chunks,
	chunk_count,
	time, % Seconds from adding
	reliable % If true, isn't deleted on timeout
}).

-define(PROTOCOL_VERSION, 7).
-define(PROTOCOL_ID, 16#4f457403).
-define(PASSWORD_SIZE,28). %% Maximum password length. Allows for base64-encoded SHA-1 (27+\0).
-define(TEXTURENAME_ALLOWED_CHARS,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.").


%% === NOTES ===

%% A packet is sent through a channel to a peer with a basic header:
%% TODO: Should we have a receiver_peer_id also?
%%	Header (7 bytes):
%%	[0] u32 protocol_id
%%	[4] u16 sender_peer_id
%%	[6] u8 channel
%% sender_peer_id:
%%	Unique to each peer.
%%	value 0 is reserved for making new connections
%%	value 1 is reserved for server
%% channel:
%%	The lower the number, the higher the priority is.
%% 	Only channels 0, 1 and 2 exist.

-define(BASE_HEADER_SIZE, 7).
-define(PEER_ID_INEXISTENT, 0).
-define(PEER_ID_SERVER, 1).
-define(CHANNEL_COUNT, 3).


%% Packet types:

%% CONTROL: This is a packet used by the protocol.
%% - When this is processed, nothing is handed to the user.
%%	 Header (2 byte):
%%	  [0] u8 type
%%	  [1] u8 controltype
%% controltype and data description:
%%	CONTROLTYPE_ACK
%%		[2] u16 seqnum
%%	CONTROLTYPE_SET_PEER_ID
%%		[2] u16 peer_id_new
%%	CONTROLTYPE_PING
%%	- There is no actual reply, but this can be sent in a reliable
%%	  packet to get a reply
%%	CONTROLTYPE_DISCO

-define(TYPE_CONTROL, 0).
-define(CONTROLTYPE_ACK, 0).
-define(CONTROLTYPE_SET_PEER_ID, 1).
-define(CONTROLTYPE_PING, 2).
-define(CONTROLTYPE_DISCO, 3).

%% ORIGINAL: This is a plain packet with no control and no error checking at all.
%% - When this is processed, it is directly handed to the user.
%%	Header (1 byte):
%%	[0] u8 type

-define(TYPE_ORIGINAL, 1).
-define(ORIGINAL_HEADER_SIZE, 1).


%% SPLIT: These are sequences of packets forming one bigger piece of data.
%% - When processed and all the packet_nums 0...packet_count-1 are
%%  present (this should be buffered), the resulting data shall be
%%  directly handed to the user.
%% - If the data fails to come up in a reasonable time, the buffer shall
%%  be silently discarded.
%% - These can be sent as-is or atop of a RELIABLE packet stream.
%%	Header (7 bytes):
%%	[0] u8 type
%%	[1] u16 seqnum
%%	[3] u16 chunk_count
%%	[5] u16 chunk_num

-define(TYPE_SPLIT, 2).

%% RELIABLE: Delivery of all RELIABLE packets shall be forced by ACKs,
%% and they shall be delivered in the same order as sent. This is done
%% with a buffer in the receiving and transmitting end.
%% - When this is processed, the contents of each packet is recursively
%% processed as packets.
%%	Header (3 bytes):
%%	[0] u8 type
%%	[1] u16 seqnum


-define(TYPE_RELIABLE, 3).
-define(RELIABLE_HEADER_SIZE, 3).
%%#define SEQNUM_INITIAL 0x10
-define(SEQNUM_INITIAL, 65500).
-define(SEQNUM_MAX, 65535).


%% ================================= 
%% TOSERVER command group
%% ================================= 

-define(TOSERVER_INIT,16#10).
    
%%  Sent first after connected.
%%
%%      [0] u16 TOSERVER_INIT
%%      [2] u8 SER_FMT_VER_HIGHEST
%%      [3] u8[20] player_name
%%      [23] u8[28] password (new in some version)
%%      [51] u16 client network protocol version (new in some version)


-define(TOSERVER_INIT2,16#11).

%%       Sent as an ACK for TOCLIENT_INIT.
%%        After this, the server can send data.
%%
%%        [0] u16 TOSERVER_INIT2
  

-define(TOSERVER_GETBLOCK, 16#20). %% Obsolete
-define(TOSERVER_ADDNODE, 16#21). %% Obsolete
-define(TOSERVER_REMOVENODE, 16#22). %% Obsolete

-define(TOSERVER_PLAYERPOS, 16#23).

%%        [0] u16 command
%%        [2] v3s32 position*100
%%        [2+12] v3s32 speed*100
%%        [2+12+12] s32 pitch*100
%%        [2+12+12+4] s32 yaw*100
   

-define(TOSERVER_GOTBLOCKS,16#24).
    
%%        [0] u16 command
%%        [2] u8 count
%%        [3] v3s16 pos_0
%%        [3+6] v3s16 pos_1
%%        ...


-define(TOSERVER_DELETEDBLOCKS, 16#25).

%%        [0] u16 command
%%        [2] u8 count
%%        [3] v3s16 pos_0
%%        [3+6] v3s16 pos_1
%%        ...
  

-define(TOSERVER_ADDNODE_FROM_INVENTORY, 16#26). %% Obsolete
    
%%        [0] u16 command
%%        [2] v3s16 pos
%%        [8] u16 i


-define(TOSERVER_CLICK_OBJECT, 16#27). %% Obsolete
    
%%        length: 13
%%        [0] u16 command
%%        [2] u8 button (0=left, 1=right)
%%        [3] v3s16 blockpos
%%        [9] s16 id
%%        [11] u16 item


-define(TOSERVER_GROUND_ACTION, 16#28). %% Obsolete
    
%%        length: 17
%%        [0] u16 command
%%        [2] u8 action
%%        [3] v3s16 nodepos_undersurface
%        [9] v3s16 nodepos_abovesurface
%        [15] u16 item
%        actions:
%        0: start digging (from undersurface)
%        1: place block (to abovesurface)
%        2: stop digging (all parameters ignored)
%        3: digging completed
  
    
-define(TOSERVER_RELEASE, 16#29). %% Obsolete

% (oops, there is some gap here)


-define(TOSERVER_SIGNTEXT, 16#30). %% Old signs, obsolete
    
%        u16 command
%        v3s16 blockpos
%        s16 id
%        u16 textlen
%        textdata
    
    
-define(TOSERVER_INVENTORY_ACTION, 16#31).
    
%  See InventoryAction in inventory.h
  

-define(TOSERVER_CHAT_MESSAGE, 16#32).
    
%%        u16 command
%%        u16 length
%%        wstring message


-define(TOSERVER_SIGNNODETEXT, 16#33).
    
%%        u16 command
%%        v3s16 p
%%        u16 textlen
%%        textdata


-define(TOSERVER_CLICK_ACTIVEOBJECT, 16#34). % Obsolete
    
%%        length: 7
%%        [0] u16 command
%%        [2] u8 button (0=left, 1=right)
%%        [3] u16 id
%%        [5] u16 item

    
-define(TOSERVER_DAMAGE, 16#35).
    
%%        u16 command
%%        u8 amount


-define(TOSERVER_PASSWORD, 16#36).
    
%%        Sent to change password.

%%        [0] u16 TOSERVER_PASSWORD
%%        [2] u8[28] old password
%%        [30] u8[28] new password
  

-define(TOSERVER_PLAYERITEM, 16#37).
    
%%        Sent to change selected item.
%%        [0] u16 TOSERVER_PLAYERITEM
%%        [2] u16 item

    
-define(TOSERVER_RESPAWN,16#38).
    
%%        u16 TOSERVER_RESPAWN
  
-define(TOSERVER_INTERACT, 16#39).
    
%%        [0] u16 command
%%        [2] u8 action
%%        [3] u16 item
%%        [5] u32 length of the next item
%%        [9] serialized PointedThing
%%        actions:
%%        0: start digging (from undersurface) or use
%%        1: stop digging (all parameters ignored)
%%        2: digging completed
%%        3: place block or item (to abovesurface)
%%        4: use item

%%        (Obsoletes TOSERVER_GROUND_ACTION and TOSERVER_CLICK_ACTIVEOBJECT.)
  
    
-define(TOSERVER_REQUEST_TEXTURES, 16#40).

%%            u16 command
%%            u16 number of textures requested
%%            for each texture {
%%                u16 length of name
%%                string name
%%            }
  
  
  
  
%% =================================  
%%  TOCLIENT command group  
%% =================================    
  
-define(TOCLIENT_INIT, 16#10).
	
%%		Server's reply to TOSERVER_INIT.
%%		Sent second after connected.
%%
%%		[0] u16 TOSERVER_INIT
%%		[2] u8 deployed version
%%		[3] v3s16 player's position + v3f(0,BS/2,0) floatToInt'd 
%%		[12] u64 map seed (new as of 2011-02-27)
%%
%%		NOTE: The position in here is deprecated; position is
%%		      explicitly sent afterwards
	

-define(TOCLIENT_BLOCKDATA, 16#20). %%TODO: Multiple blocks
-define(TOCLIENT_ADDNODE, 16#21).
-define(TOCLIENT_REMOVENODE, 16#22).
	

-define(TOCLIENT_PLAYERPOS, 16#23). %% Obsolete
	
%%		[0] u16 command
%%		// Followed by an arbitary number of these:
%%		// Number is determined from packet length.
%%		[N] u16 peer_id
%%		[N+2] v3s32 position*100
%%		[N+2+12] v3s32 speed*100
%%		[N+2+12+12] s32 pitch*100
%%		[N+2+12+12+4] s32 yaw*100
	

-define(TOCLIENT_PLAYERINFO, 16#24). %% Obsolete
	
%%		[0] u16 command
%%		// Followed by an arbitary number of these:
%%		// Number is determined from packet length.
%%		[N] u16 peer_id
%%		[N] char[20] name
	
	
-define(TOCLIENT_OPT_BLOCK_NOT_FOUND, 16#25). %% Obsolete

-define(TOCLIENT_SECTORMETA, 16#26). %% Obsolete
	
%%		[0] u16 command
%%		[2] u8 sector count
%%		[3...] v2s16 pos + sector metadata
	

-define(TOCLIENT_INVENTORY, 16#27).
	
%%		[0] u16 command
%%		[2] serialized inventory
	
	
-define(TOCLIENT_OBJECTDATA, 16#28) %% Obsolete
	
%%		Sent as unreliable.
%%
%%		u16 command
%%		u16 number of player positions
%%		for each player:
%%			u16 peer_id
%%			v3s32 position*100
%%			v3s32 speed*100
%%			s32 pitch*100
%%			s32 yaw*100
%%		u16 count of blocks
%%		for each block:
%%			v3s16 blockpos
%%			block objects
	

-define(TOCLIENT_TIME_OF_DAY, 16#29).
	
%%		u16 command
%%		u16 time (0-23999)
	

%% (oops, there is some gap here)

-define(TOCLIENT_CHAT_MESSAGE, 16#30).
	
%%		u16 command
%%		u16 length
%%		wstring message
	

-define(TOCLIENT_ACTIVE_OBJECT_REMOVE_ADD, 16#31).
	
%%		u16 command
%%		u16 count of removed objects
%%		for all removed objects {
%%			u16 id
%%		}
%%		u16 count of added objects
%%		for all added objects {
%%			u16 id
%%			u8 type
%%			u32 initialization data length
%%			string initialization data
%%		}
	
	
-define(TOCLIENT_ACTIVE_OBJECT_MESSAGES, 16#32).
	
%%		u16 command
%%		for all objects
%%		{
%%			u16 id
%%			u16 message length
%%			string message
%%		}
	

-define(TOCLIENT_HP, 16#33).
	
%%		u16 command
%%		u8 hp
	

-define(TOCLIENT_MOVE_PLAYER, 16#34).
	
%%		u16 command
%%		v3f1000 player position
%%		f1000 player pitch
%%		f1000 player yaw
	

-define(TOCLIENT_ACCESS_DENIED, 16#35).
	
%%		u16 command
%%		u16 reason_length
%%		wstring reason
	

-define(TOCLIENT_PLAYERITEM, 16#36)
	
%%		u16 command
%%		u16 count of player items
%%		for all player items {
%%			u16 peer id
%%			u16 length of serialized item
%%			string serialized item
%%		}
	

-define(TOCLIENT_DEATHSCREEN, 16#37).
	
%%		u16 command
%%		u8 bool set camera point target
%%		v3f1000 camera point target (to point the death cause or whatever)
	

-define(TOCLIENT_TEXTURES, 16#38).
	
%%		u16 command
%%		u16 total number of texture bunches
%%		u16 index of this bunch
%%		u32 number of textures in this bunch
%%		for each texture {
%%			u16 length of name
%%			string name
%%			u32 length of data
%%			data
%%		}
	
	
-define(TOCLIENT_TOOLDEF, 16#39).
	
%%		u16 command
%%		u32 length of the next item
%%		serialized ToolDefManager
	
	
-define(TOCLIENT_NODEDEF, 16#3a).
	
%%		u16 command
%%		u32 length of the next item
%%		serialized NodeDefManager
	
	
-define(TOCLIENT_CRAFTITEMDEF, 16#3b).
	
%%		u16 command
%%		u32 length of the next item
%%		serialized CraftiItemDefManager
	

-define(TOCLIENT_ANNOUNCE_TEXTURES, 16#3c).

%%		u16 command
%%		u32 number of textures
%%		for each texture {
%%			u16 length of name
%%			string name
%%			u16 length of sha1_digest
%%			string sha1_digest
%%		}
	

-define(TOCLIENT_ITEMDEF, 16#3d).
	
%%		u16 command
%%		u32 length of next item
%%		serialized ItemDefManager
