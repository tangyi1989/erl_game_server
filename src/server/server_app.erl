%%--------------------------------------
%% @Module : server_app
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : 打包程序
%%--------------------------------------

-module(server_app).
-behaviour(application).

-export([start/0, stop/0]).

start() ->
    Port = 8888,
    {ok, SupPid} = server_sup:start_link(),
    server_mods:start(Port),
    {ok, SupPid}.

stop() ->
    void.