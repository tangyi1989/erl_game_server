%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%		负责启动日志节点
%%% @end
%%% Created : 2013-10-14
%%%-------------------------------------------------------------------

-module(logger).
-behaviour(application).

-export([
	 		start/2,
	 		stop/1,
	 		start/0,
	 		stop/0
        ]).
-compile(export_all).
-define(APPS, [sasl, logger]).


-define(Attributes,[
		{       "join manager node!!",
                        fun()->
                                {ok, [[MasterNodeTmp]]} = init:get_argument(master_node),
                                net_kernel:connect_node(erlang:list_to_atom(MasterNodeTmp)),
                                timer:sleep(2000),
                                ok
                        end
                },
		{	"notify the manager node!!",
			fun()->
				global:send(manager_node, {logger_node_up, erlang:node()})
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
%%%		负责关闭sasl和logger应用程序
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
%%%		负责logger应用程序中的子进程,该方法在logger.app配置中
%%% @end
%%%-------------------------------------------------------------------
start(normal, []) ->
	{ok, SuperPid} = logger_sup:start_link(),
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
				io:format("~p going ----~n",[Msg]),
				Thunk(),
				io:format("~p done------~n",[Msg])
	end, Attributes).
