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

-export([
	 start/2,
	 stop/1,
	 start/0,
	 stop/0
        ]).

-compile(export_all).

-define(APPS, [sasl, server]).

-server_boot_step({server, 
				 	[{description, "manager node to start other nodes"},  %%description
                   	{mfa, {	server_mods, 	%%module
                   			start,			%%method
                            [8888]				%%parameter
                          }
                    }]}).
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
	[Attributes] = common_node:all_module_attributes(server, server_boot_step),
	worker_behaviour(fun lists:foreach/2, Attributes),
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
				io:format("App=~p~n",[App]),
				case ApplicationStart(App) of
					ok -> io:format("----------------~n"),[App | Acc];
					{error, {SkipError, _}} -> Acc;
					{error, Reason} ->
						lists:foreach(ApplicationStop, Acc),
						throw({error, {ErrorTag, App, Reason}})
				end
	end, [], Apps).

worker_behaviour(Iterate, Attributes) ->
	io:format("~p~n",[Attributes]),
	Iterate(fun({Module, [{description, Description}, {mfa, MFA}]}) ->
				io:format("~p module's ~p going ----~n",[Module, Description]),
				{M, F, A} = MFA,
				apply(M, F, A),
				io:format("~p module's ~p done------~n",[Module, Description])
	end, Attributes).
