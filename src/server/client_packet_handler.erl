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

-record(client, {state = init}).
-include("common.hrl").

%%网络协议定义
-define(MAGIC_CODE, 16#ffffffff). % magic code，在每个包的头部，用于检验出错
-define(HEART_TIMEOUT, 60000). % 心跳包超时时间
-define(TCP_TIMEOUT, 10000). % 解析协议超时时间


%%------------------------ API -------------------------

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

%%------------------ moudle callbacks ------------------

init() ->
    process_flag(trap_exit, true),
    receive
        {go, Sock} ->
            Client = #client{state=init},
            parse_packet_loop(Sock, none, Client)
    end.

%%------------------ internals -------------------------

%% 接受信息，现在对于prim_inet里面的内容还很不了解。
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

%% 下面为读包和解包的代码.
parse_packet_loop(Sock, none, Client) ->
    Ref = async_recv(Sock, 12, ?HEART_TIMEOUT),
    parse_packet_loop(Sock, Ref, Client);

parse_packet_loop(Sock, Ref, Client) ->
    receive
        {inet_async, Sock, Ref, {ok, <<?MAGIC_CODE:32, Cmd:32, BodyLen:32>>}} ->
            case BodyLen > 0 of
                true ->
                    io:format("Cmd:~p Bodylen:~p~n", [Cmd, BodyLen]),
                    PacketRef = async_recv(Sock, BodyLen, ?TCP_TIMEOUT),
                    receive
                        {inet_async, Sock, PacketRef, {ok, ProtoBuf}} ->
                            handle_packet(Cmd, ProtoBuf, Client),
                            NewRef = async_recv(Sock, 12, ?HEART_TIMEOUT),
                            parse_packet_loop(Sock, NewRef, Client);
                        _Other ->
                            lost_client(Sock, Client, "Lost when receive body.")
                    end;
                false ->
                    handle_packet(Cmd, none, Client),
                    NewRef = async_recv(Sock, 12, ?HEART_TIMEOUT),
                    parse_packet_loop(Sock, NewRef, Client)
            end;
        Other ->
            io:format("~p~n", [Other]), 
            lost_client(Sock, Client, "Lost when receive header.")
    end.

%% 因为一些不可预知的原因丢失玩家
lost_client(Sock, _Client, Reason) ->
    io:format("lost client : ~p~n", [Reason]),
    gen_tcp:close(Sock).

%% 处理封包
handle_packet(Cmd, ProtoBuf, _Client) ->
    handle(Cmd, ProtoBuf).

handle(123, ProtoBuf) ->
    Login = game_pb:decode_login(ProtoBuf),
    io:format("Login : ~p~n", [Login]);

handle(Cmd, ProtoBuf) ->
    io:format("Unprocessed Cmd:~p ProtoBuf:~p ~n", [Cmd, ProtoBuf]). 
    