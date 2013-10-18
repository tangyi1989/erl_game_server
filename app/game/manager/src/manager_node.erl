%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%		负责启动其他节点
%%% @end
%%% Created : 2013-09-25
%%%-------------------------------------------------------------------

-module(manager_node).
-include("manager.hrl").
-include("logger.hrl").

-export([
	start/0
	]).
-compile(export_all).
-define(CONFIG_GATEWAY_PATH, "/data/erl_game_server/script/start_gateway.sh").
-define(CONFIG_LOGGER_PATH,"/data/erl_game_server/script/start_logger.sh").
-define(ITEM_LIST, [gateway, map, master_host]).

%%%-------------------------------------------------------------------
%%% @doc
%%%		在manager节点处启动其他节点
%%% @end
%%%-------------------------------------------------------------------
start() ->
	yes = global:register_name(manager_node, erlang:self()),
%%	start_logger_node(),
	start_gateway_node(),
	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%             启动日志节点,并且等待管理节点的响应
%%% @end
%%%-------------------------------------------------------------------
%%start_logger_node() ->
%%	Command = execute_logger_command(),
%%	?SYSTEM_LOG("~ts~n ~s~n",["准备启动日志节点", Command]),
%%	erlang:open_port({spawn, Command}, [stream]),
%%	receive
%%	{logger_node_up, NodeName} ->
%%		?SYSTEM_LOG("~ts~n ~s~n",["日志节点启动成功", NodeName]),
%%		net_kernel:connect_node(NodeName)
%%	end,
%%	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%		启动网关节点,并且等待管理节点的响应
%%% @end
%%%-------------------------------------------------------------------
start_gateway_node() ->
	Command = execute_gateway_command(),
	?INFO("==========READY TO START GATEWAY NODE========="),
	erlang:open_port({spawn, Command}, [stream]),
	receive 
       	{gateway_node_up, NodeName} ->
            ?INFO("======~p GATEWAY NODE START SUCCESSFUL=====", [NodeName]),
            net_kernel:connect_node(NodeName)                                                
    	end,
	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%		生成启动网关节点脚本命令行
%%% @end
%%%-------------------------------------------------------------------
execute_gateway_command() ->
	Command = lists:flatten(lists:concat(["bash ", ?CONFIG_GATEWAY_PATH])),
	Command.

%%%-------------------------------------------------------------------
%%% @doc
%%%             生成启动日志节点脚本命令行
%%% @end
%%%-------------------------------------------------------------------
%%execute_logger_command() ->
%%	Command = lists:flatten(lists:concat(["bash ", ?CONFIG_LOGGER_PATH])),
%%	Command.
