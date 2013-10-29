%%-------------------------------------
%% @Module : gateway_client
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.20
%% @Description : 客户端监控树
%%-------------------------------------

-module(gateway_client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%------------------------------- API -----------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------- supervisor callbacks ------------------------
init([]) ->
    {
        ok,
        {
            {simple_one_for_one, 10, 10},
            [
                {
                    gateway_client, 
                    {
                        gateway_client, start_link, []
                    },
                    temporary,
                    brutal_kill,
                    worker,
                    [gateway_client]
                }
            ]
        }
    }.