%%-------------------------------------
%% @Module : gateway_listener
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : TCP 监听
%%-------------------------------------

-module(gateway_listener).
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

%%------------------------------ API --------------------------------------

start_link(AcceptorCount, Port) ->
    gen_server:start_link(?MODULE, {AcceptorCount, Port}, []).

%%--------------------------- gen_server callbacks ------------------------

%% 监听Port，并且创建AcceptorCount个进程用来accept连接。
init({AcceptorCount, Port}) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSock} ->
            lists:foreach(
                fun(_) -> 
                        {ok, _APid} = supervisor:start_child(
                                gateway_acceptor_sup, [LSock]
                            ) 
                end,
                lists:duplicate(AcceptorCount, dumy)),
            {ok, LSock};
        {error, Reason} ->
            {stop, {cannot_listen, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.