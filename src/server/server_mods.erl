%%--------------------------------------
%% @Module : server_mods
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : 启动Server模块的程序
%%--------------------------------------

-module(server_mods).
-export([start/1]).

start(Port) ->
    ok = start_kernel(),
    ok = start_client_sup(),
    ok = start_tcp_listener(Port).

start_kernel() ->
    {ok, _} = supervisor:start_child(server_sup, 
                                    {
                                        mod_kernel,
                                        {mod_kernel, start_link, []},
                                        permanent, 10000, worker, [mod_kernel]
                                    }),
    ok. 

start_client_sup() ->
    {ok, _} = supervisor:start_child(
                                    server_sup,
                                    {
                                        client_sup,
                                        {client_sup, start_link, []},
                                        transient, infinity, supervisor, [client_sup]
                                    }
                                ),
    ok.

start_tcp_listener(Port) ->
    {ok, _} = supervisor:start_child(
                                    server_sup,
                                    {
                                        server_listener_sup, 
                                        {server_listener_sup,start_link, [Port]},
                                        transient, infinity, supervisor, [server_listener_sup]
                                    }
                                ),
    ok.

