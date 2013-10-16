
-module(common_config).
-export([get_server_dir/0,
         get_map_config_dir/0]).

get_server_dir() ->
    {ok, [[ServerDir]]} = init:get_argument(server_dir),
    ServerDir.

get_map_config_dir() ->
    lists:concat([get_server_dir(), "/resource/map/mcm/"]). 