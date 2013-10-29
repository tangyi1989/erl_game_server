%%--------------------------------------
%% @Module : gateway_mods
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : 启动Server模块的程序
%%--------------------------------------

-module(gateway_mods).
-export([start/1]).

-include("logger.hrl").

start(ListenPort) ->
    lists:foreach(
        fun({Msg, Thunk}) ->
            ?INFO("Starting ~-32s~n", [Msg]),
            Thunk(),
            ?INFO("Done~n") 
        end, [
        {
            "Gateway client supervisor",
            fun() ->
                {ok, _} = supervisor:start_child(
                                                    gateway_sup,
                                                    {
                                                        gateway_client_sup,
                                                        {gateway_client_sup, start_link, []},
                                                        transient, infinity, supervisor, [gateway_client_sup]
                                                    }
                                                )
            end
        },
        {
            "Gateway TCP listener",
            fun() ->
                {ok, _} = supervisor:start_child(
                                                    gateway_sup,
                                                    {
                                                        gateway_listener_sup, 
                                                        {gateway_listener_sup,start_link, [ListenPort]},
                                                        transient, infinity, supervisor, [gateway_listener_sup]
                                                    }
                                                )
            end
        }]).
    