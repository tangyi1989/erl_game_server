%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%		负责启动其他节点
%%% @end
%%% Created : 2013-09-25
%%%-------------------------------------------------------------------

-module(manager_node).
-export([
	start/0
	]).
-compile(export_all).
-define(CONFIG_FILE_PATH, "/data/erl_game_server/script/start_gateway.sh").
-define(ITEM_LIST, [gateway, map, master_host]).

%%%-------------------------------------------------------------------
%%% @doc
%%%		在manager节点处启动其他节点
%%% @end
%%%-------------------------------------------------------------------
start() ->
	yes = global:register_name(manager_node, erlang:self()),
	start_gateway_node(),
	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%		启动网关节点,并且等待网关节点的响应
%%% @end
%%%-------------------------------------------------------------------
start_gateway_node() ->
	Command = execute_gateway_command(),
	erlang:open_port({spawn, Command}, [stream]),
	io:format("--------------------------~n"),
	receive 
       	{gateway_node_up, NodeName} ->
            net_kernel:connect_node(NodeName)                                                
    end,
	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%		生成启动网关节点脚本命令行
%%% @end
%%%-------------------------------------------------------------------
execute_gateway_command() ->
	Command = lists:flatten(lists:concat(["bash ", ?CONFIG_FILE_PATH])),
	io:format("Command=~p~n",[Command]),
	Command.
