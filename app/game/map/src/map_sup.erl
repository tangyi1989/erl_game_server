%%%--------------------------------------
%%% @Module : map_sup
%%% @Author : TangYi
%%% @Email : tang_yi_1989@qq.com
%%% @Created : 2013.10.12
%%% @Description : 地图监督树
%%%--------------------------------------

-module(map_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    Children = [],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.