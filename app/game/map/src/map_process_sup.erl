%%%--------------------------------------------------------------------------
%%% @Module : map_process_sup
%%% @Author : TangYi
%%% @Email : tang_yi_1989@qq.com
%%% @Created : 2013.10.16
%%% @Description : map_process监督树
%%%--------------------------------------------------------------------------

-module(map_process_sup).
-export([start_link/0, start/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start() ->
    {ok, _} = supervisor:start_child(map_sup, {?MODULE,
                                                 {?MODULE, start_link, []},
                                                 permanent, infinity, supervisor, 
                                                 [?MODULE]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = {map_process,{map_process,start_link,[]},
              temporary, 50000, worker, [map_process]},%% transient
    {ok,{{simple_one_for_one,100,1}, [AChild]}}.

