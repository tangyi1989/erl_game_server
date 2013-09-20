%%-------------------------------------
%% @Module : client_packet_handler
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.20
%% @Description : 接收和发送客户端封包
%%-------------------------------------

-module(client_packet_handler).

-export([start_link/0]).

-export([init/0]).

%%------------------------ API -------------------------

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

%%------------------ moudle callbacks ------------------

init() ->
    process_flag(trap_exit, true),
    receive
        {go, Sock} ->
            handle_socket(Sock)
    end.

%%------------------ internals -------------------------

%% 接受信息，现在对于prim_inet里面的内容还很不了解。
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

handle_socket(Sock) ->
    Ref = async_recv(Sock, 0, 1000000),
    receive_loop(Sock, Ref).

receive_loop(Sock, Ref) ->
    receive
        {inet_async, Sock, Ref, {ok, Data}} ->
            io:format("Recevie Data : ~p~n", [Data]),
            gen_tcp:send(Sock, Data)
    end,
    NewRef = async_recv(Sock, 0, 1000000),
    receive_loop(Sock, NewRef).