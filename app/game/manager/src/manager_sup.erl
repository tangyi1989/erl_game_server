%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%		管理进程监控树,负责监控管理进程
%%% @end
%%% Created : 2013-09-25
%%%-------------------------------------------------------------------

-module(manager_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([
	 init/1
        ]).

%%%-------------------------------------------------------------------
%%% @doc
%%%		由manager模块调用
%%% @end
%%%-------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-------------------------------------------------------------------
%%% @doc
%%%		{one_for_one,10,10}表示一旦有子进程退出,监督进程将针对该进程,且
%%%     针对该进程进行重启,第一个值指定的是最大重启次数,第二个值指定的是
%%%     时间片
%%% @end
%%%-------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_one,10,10}, []}}.