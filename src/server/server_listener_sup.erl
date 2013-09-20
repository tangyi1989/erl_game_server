%%--------------------------------------
%% @Module : server_listener_sup
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : server listener 监控树
%%--------------------------------------

-module(server_listener_sup).
-behaviour(supervisor).

%% export API
-export([start_link/1]).
%% export supervisor callbacks
-export([init/1]).

%%------------------------- API ------------------------------
start_link(Port) ->
    supervisor:start_link(?MODULE, {10, Port}).


%%--------------------- supervisor callbacks -----------------
init({AcceptorCount, Port}) ->
    {
        ok,
        {{one_for_all, 10, 10},
            [
                {
                    server_acceptor_sup,
                    {server_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [server_acceptor_sup]
                },
                {
                    server_listener,
                    {server_listener, start_link, [AcceptorCount, Port]},
                    transient,
                    100,
                    worker,
                    [server_listener]
                }
            ]
        }
    }.
