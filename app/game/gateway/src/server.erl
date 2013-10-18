%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%		负责启动管理节点对应的进程,该节点由manager节点负责启动
%%% @end
%%% Created : 2013-09-25
%%%-------------------------------------------------------------------

-module(server).
-behaviour(application).
-include("common.hrl").
-include("logger.hrl").
-export([
	 start/2,
	 stop/1,
	 start/0,
	 stop/0
        ]).

-compile(export_all).

-define(APPS, [sasl, lager, server]).

%%-server_boot_step({server, 
%%			[{description, "gateway node handle connect"},  %%description
%%                   	{mfa, { server_mods, 	%%module
%%                   	        start,		%%method
%%                                [8888]		%%parameter
%%                        }
%%                    }]}).

%%-server_boot_step({server,
%%                        [{description, "notify the manager node"},  %%description
%%                        {mfa, { global,    			    %%module
%%                                send,          			    %%method
%%                                []          %%parameter
%%                        }
%%                    }]}).

-define(Attributes,[
		{	"server_mods process",
			fun()->
				server_mods:start(8888)
			end
		},
		%%这里注意要先拼通管理节点啊，要不下面的global:send会报错的，调了两个多钟头
		{	"join manager node",
			fun()->
				{ok, [[MasterNodeTmp]]} = init:get_argument(master_node),
				net_kernel:connect_node(erlang:list_to_atom(MasterNodeTmp)),
				timer:sleep(2000), 
				ok
			end
		},
		{
			"notify the manager node",
			fun() ->
				global:send(manager_node, {gateway_node_up,erlang:node()})
			end
		}
	]).

%%%-------------------------------------------------------------------
%%% @doc
%%%		负责启动sasl和manager应用程序
%%% @end
%%%-------------------------------------------------------------------
start() ->
	application_behaviour(
				fun lists:foldl/3,
				fun application:start/1,
				fun application:stop/1,
				already_start,
				cannot_start_application,
				?APPS
	).

%%%-------------------------------------------------------------------
%%% @doc
%%%		负责关闭sasl和manager应用程序
%%% @end
%%%-------------------------------------------------------------------
stop() ->
	application_behaviour(
				fun lists:foldl/3,
				fun application:start/1,
				fun application:stop/1,
				not_started,
				cannot_stop_application,
				?APPS
	).

%%%-------------------------------------------------------------------
%%% @doc
%%%		负责manager应用程序中的子进程,改方法在manager.app配置中
%%% @end
%%%-------------------------------------------------------------------
start(normal, []) ->
	{ok, SuperPid} = server_sup:start_link(),
%%	Attributes = common_node:all_module_attributes(server, server_boot_step),
	worker_behaviour(fun lists:foreach/2, ?Attributes),
	{ok, SuperPid}.

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%% @doc
%%%		负责应用程序的启动行为,如果应用程序中有一个启动出现问题,那么将
%%%     会导致其他应用程序的关闭,如果应用程序中有一个关闭出现问题,那么
%%%     会导致其他应用程序重新启动
%%% @end
%%%-------------------------------------------------------------------
application_behaviour(Iterate, ApplicationStart, ApplicationStop, SkipError, ErrorTag, Apps) ->
	Iterate(fun(App, Acc) ->
				case ApplicationStart(App) of
					ok -> [App | Acc];
					{error, {SkipError, _}} -> Acc;
					{error, Reason} ->
						lists:foreach(ApplicationStop, Acc),
						throw({error, {ErrorTag, App, Reason}})
				end
	end, [], Apps).

worker_behaviour(Iterate, Attributes) ->
	Iterate(fun({Msg, Thunk}) ->
				?INFO("===========~p ready start===========",[Msg]),
				Thunk(),
				?INFO("===========~p start end=============",[Msg])
	end, Attributes).
