%%
%% erlmines - The minetest server written in Erlang
%% Copyright (C) 2012 hummermania, Markevych Alexander <rabota.pmr@gmail.com>
%% 

-module(clientserver).
-export([init/0]).

init() ->
    ToClientCommand = gb_sets:from_list([
    
    {toclient_init, 16#10},
    %
    %   Server's reply to TOSERVER_INIT.
    %   Sent second after connected.
    %
    %   [0] u16 TOSERVER_INIT
    %   [2] u8 deployed version
    %   [3] v3s16 player's position + v3f(0,BS/2,0) floatToInt'd 
    %   [12] u64 map seed (new as of 2011-02-27)
    %
    %   NOTE: The position in here is deprecated; position is
    %         explicitly sent afterwards

    { toclient_blockdata,      16#20}, % TODO: Multiple blocks
    { toclient_addnode,        16#21},
    { toclient_removenode,     16#22},
    { toclient_playerpos,      16#23}, % Obsolete
    %
    %    [0] u16 command
    %    //  Followed by an arbitary number of these:
    %    //  Number is determined from packet length.
    %    [N] u16 peer_id
    %    [N+2] v3s32 position*100
    %    [N+2+12] v3s32 speed*100
    %    [N+2+12+12] s32 pitch*100
    %    [N+2+12+12+4] s32 yaw*100
    
    { toclient_playerinfo,     16#24}, % Obsolete
    %
    %    [0] u16 command
    %    // Followed by an arbitary number of these:
    %    // Number is determined from packet length.
    %    [N] u16 peer_id
    %    [N] char[20] name
    %
    
    { toclient_opt_block_not_found, 16#25}, % Obsolete

    { toclient_sectormeta,     16#26}, % Obsolete
    %
    %    [0] u16 command
    %    [2] u8 sector count
    %    [3...] v2s16 pos + sector metadata
    
    { toclient_inventory,      16#27},
    %
    %    [0] u16 command
    %    [2] serialized inventory
    %
    
    { toclient_objectdata,     16#28}, % Obsolete
    %
    %    Sent as unreliable.
    %
    %    u16 command
    %    u16 number of player positions
    %    for each player:
    %        u16 peer_id
    %        v3s32 position*100
    %        v3s32 speed*100
    %        s32 pitch*100
    %        s32 yaw*100
    %    u16 count of blocks
    %    for each block:
    %        v3s16 blockpos
    %        block objects
    %

    { toclient_time_of_day,    16#29},
    %
    %    u16 command
    %    u16 time (0-23999)
    %    Added in a later version:
    %    f1000 time_speed
    %

    % (oops, there is some gap here)

    { toclient_chat_message,   16#30},
    %
    %    u16 command
    %    u16 length
    %    wstring message
    %

    { toclient_active_object_remove_add, 16#31},
    %
    %    u16 command
    %    u16 count of removed objects
    %    for all removed objects {
    %        u16 id
    %    }
    %    u16 count of added objects
    %    for all added objects {
    %        u16 id
    %        u8 type
    %        u32 initialization data length
    %        string initialization data
    %    }
    
    
    { toclient_active_object_messages, 16#32},
    %
    %    u16 command
    %    for all objects
    %    {
    %        u16 id
    %        u16 message length
    %        string message
    %    }
    %

    { toclient_hp,             16#33},
    %
    %    u16 command
    %    u8 hp
    %

    { toclient_move_player,    16#34},
    %
    %    u16 command
    %    v3f1000 player position
    %    f1000 player pitch
    %    f1000 player yaw
    %

    { toclient_access_denied,  16#35},
    %
    %    u16 command
    %    u16 reason_length
    %    wstring reason
    %

    { toclient_playeritem,     16#36},
    %
    %    u16 command
    %    u16 count of player items
    %    for all player items {
    %        u16 peer id
    %        u16 length of serialized item
    %        string serialized item
    %    }
    %
    
    { toclient_deathscreen,    16#37},
    %
    %    u16 command
    %    u8 bool set camera point target
    %    v3f1000 camera point target (to point the death cause or whatever)
    %

    { toclient_media,         16#38},
    %
    %    u16 command
    %    u16 total number of texture bunches
    %    u16 index of this bunch
    %    u32 number of files in this bunch
    %    for each file {
    %        u16 length of name
    %        string name
    %        u32 length of data
    %        data
    %    }
    %
    
    { toclient_tooldef,        16#39},
    %
    %    u16 command
    %    u32 length of the next item
    %    serialized ToolDefManager
    %
    
    { toclient_nodedef,        16#3a},
    %
    %    u16 command
    %    u32 length of the next item
    %    serialized NodeDefManager
    %
    
    { toclient_craftitemdef,   16#3b},
    %
    %    u16 command
    %    u32 length of the next item
    %    serialized CraftiItemDefManager
    %

    { toclient_announce_media, 16#3c},
    %
    %    u16 command
    %    u32 number of files
    %    for each texture {
    %        u16 length of name
    %        string name
    %        u16 length of sha1_digest
    %        string sha1_digest
    %    }
    %

    { toclient_itemdef,        16#3d},
    %
    %    u16 command
    %    u32 length of next item
    %    serialized ItemDefManager
    %
    
    { toclient_play_sound,     16#3f},
    %
    %    u16 command
    %    s32 sound_id
    %    u16 len
    %    u8[len] sound name
    %    s32 gain*1000
    %    u8 type (0=local, 1=positional, 2=object)
    %    s32[3] pos_nodes*10000
    %    u16 object_id
    %    u8 loop (bool)
    %

    { toclient_stop_sound,     16#40}
    %
    %    u16 command
    %    s32 sound_id
    %
    ]),
                       
    ToServerCommand = gb_sets:from_list([
 
    { toserver_init,           16#10},
    %
    %    Sent first after connected.
    %
    %    [0] u16 TOSERVER_INIT
    %    [2] u8 SER_FMT_VER_HIGHEST
    %    [3] u8[20] player_name
    %    [23] u8[28] password (new in some version)
    %    [51] u16 client network protocol version (new in some version)
    %

    { toserver_init2,          16#11},
    %
    %    Sent as an ACK for TOCLIENT_INIT.
    %    After this, the server can send data.
    %
    %    [0] u16 TOSERVER_INIT2
    %

    { toserver_getblock,       16#20}, % Obsolete
    { toserver_addnode,        16#21}, % Obsolete
    { toserver_removenode,     16#22}, % Obsolete

    { toserver_playerpos,      16#23},
    %
    %    [0] u16 command
    %    [2] v3s32 position*100
    %    [2+12] v3s32 speed*100
    %    [2+12+12] s32 pitch*100
    %    [2+12+12+4] s32 yaw*100
    %

    { toserver_gotblocks,      16#24},
    %
    %    [0] u16 command
    %    [2] u8 count
    %    [3] v3s16 pos_0
    %    [3+6] v3s16 pos_1
    %    ...
    %

    { toserver_deletedblocks,  16#25},
    %
    %    [0] u16 command
    %    [2] u8 count
    %    [3] v3s16 pos_0
    %    [3+6] v3s16 pos_1
    %    ...
    %

    { toserver_addnode_from_inventory, 16#26}, % Obsolete
    %
    %    [0] u16 command
    %    [2] v3s16 pos
    %    [8] u16 i
    %

    { toserver_click_object,   16#27}, % Obsolete
    %
    %    length: 13
    %    [0] u16 command
    %    [2] u8 button (0=left, 1=right)
    %    [3] v3s16 blockpos
    %    [9] s16 id
    %    [11] u16 item
    %

    { toserver_ground_action,  16#28}, % Obsolete
    %
    %    length: 17
    %    [0] u16 command
    %    [2] u8 action
    %    [3] v3s16 nodepos_undersurface
    %    [9] v3s16 nodepos_abovesurface
    %    [15] u16 item
    %    actions:
    %    0: start digging (from undersurface)
    %    1: place block (to abovesurface)
    %    2: stop digging (all parameters ignored)
    %    3: digging completed
    %
    
    { toserver_release,        16#29}, % Obsolete

    % (oops, there is some gap here)

    { toserver_signtext,       16#30}, % Old signs, obsolete
    %
    %    u16 command
    %    v3s16 blockpos
    %    s16 id
    %    u16 textlen
    %    textdata
    %

    { toserver_inventory_action, 16#31},
    %
    %    See InventoryAction in inventory.h
    %

    { toserver_chat_message,   16#32},
    %
    %    u16 command
    %    u16 length
    %    wstring message
    %

    { toserver_signnodetext,   16#33},
    %
    %    u16 command
    %    v3s16 p
    %    u16 textlen
    %    textdata
    %

    { toserver_click_activeobject, 16#34}, % Obsolete
    %
    %    length: 7
    %    [0] u16 command
    %    [2] u8 button (0=left, 1=right)
    %    [3] u16 id
    %    [5] u16 item
    %
    
    { toserver_damage,         16#35},
    %
    %    u16 command
    %    u8 amount
    %

    { toserver_password,       16#36},
    %
    %    Sent to change password.
    %
    %    [0] u16 TOSERVER_PASSWORD
    %    [2] u8[28] old password
    %    [30] u8[28] new password
    %

    { toserver_playeritem,     16#37},
    %
    %    Sent to change selected item.
    %
    %    [0] u16 TOSERVER_PLAYERITEM
    %    [2] u16 item
    %
    
    { toserver_respawn,        16#38},
    %
    %    u16 TOSERVER_RESPAWN
    %

    { toserver_interact,       16#39},
    %
    %    [0] u16 command
    %    [2] u8 action
    %    [3] u16 item
    %    [5] u32 length of the next item
    %    [9] serialized PointedThing
    %    actions:
    %    0: start digging (from undersurface) or use
    %    1: stop digging (all parameters ignored)
    %    2: digging completed
    %    3: place block or item (to abovesurface)
    %    4: use item
    %
    %    (Obsoletes TOSERVER_GROUND_ACTION and TOSERVER_CLICK_ACTIVEOBJECT.)
    %
    
    { toserver_removed_sounds, 16#3a},
    %
    %    u16 command
    %    u16 len
    %    s32[len] sound_id
    %

    { toserver_request_media,  16#40}
    %
    %    u16 command
    %    u16 number of files requested
    %    for each file {
    %        u16 length of name
    %        string name
    %    }
    %
    ]),
    [ToClientCommand, ToServerCommand].

