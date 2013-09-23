%%-------------------------------------
%% @Module : server_sup
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : GameServer的root监控树
%%-------------------------------------


-module(server_sup).
-behaviour(supervisor).

%% export API
-export([start_link/0, start_child/1, start_child/2]).
%% export supervisor callbacks
-export([init/1]).

%% ------------------------- API --------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% 下面这两个API貌似没怎么用到。
start_child(Mod) ->
    start_child(Mod, []).

start_child(Mod, Args) ->
    {ok, _} = supervisor:start_child(?MODULE,
                                    {Mod, {Mod, start_link, Args}},
                                    {transient, 100, worker}, [Mod]).

%% ------------------ supervisor callbacks ----------------

init([]) ->
    {
        ok, 
        {
            {one_for_one, 3, 10},
            []
        }
    }.