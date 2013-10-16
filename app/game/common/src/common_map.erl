%%-------------------------------------------------------------
%% @Module : mod_map_loader
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.10.16
%% @Description : 地图信息模块
%%-------------------------------------------------------------

-module(common_map).

-export([get_common_map_name/1]).

get_common_map_name(MapId) when is_integer(MapId)->
    lists:concat([map_, MapId]).