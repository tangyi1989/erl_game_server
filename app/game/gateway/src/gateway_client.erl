%%-------------------------------------
%% @Module : gateway_client
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.20
%% @Description : 接收和发送客户端封包
%%-------------------------------------

-module(gateway_client).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").


%%网络协议定义
-define(MAGIC_CODE, 16#ffffffff). % magic code，在每个包的头部，用于检验出错
-define(HEART_TIMEOUT, 60000). % 心跳包超时时间
-define(TCP_TIMEOUT, 10000). % 解析协议超时时间
-define(WAIT_HEADER, wait_header). % Socket 正在等待 header
-define(WAIT_BODY, wait_body). % Socket 正在等待 body

-record(state, {receive_status, sock, sock_ref=none, receive_cmd=undefined}).

start_link() ->
    gen_server:start_link(?MODULE, {}, []).

%% @private
init({}) ->
    {ok, init}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% 进行初始化，准备接收消息
handle_info({start, ClientSocket}, init) -> 
    io:format("Start Client...~n"), 
    Ref = async_recv(ClientSocket, 12, ?HEART_TIMEOUT),
    State = #state{receive_status=?WAIT_HEADER, sock=ClientSocket, sock_ref=Ref},
    {noreply, State};

%% 处理网络信息
handle_info({inet_async, _Socket, _Ref, {ok, Data}}, State) ->
    case handle_socket_data(Data, State) of
        {ok, undefined, NewState} -> 
            {noreply, NewState};
        {ok, {Module, Method, Record}, NewState} ->
            handle_packet({Module, Method, Record}, NewState),
            {noreply, NewState};
        Other ->
            io:format("handle_socket_data return:~p ~n", [Other]),
            {stop, normal, State}
    end;

handle_info({inet_async, _Socket, _Ref, {error, Reason}}, State) ->
    {stop, Reason, State}.

%% @private
terminate(Reason, State) ->
    io:format("terminate:~p~n", [Reason]),
    lost_client(State#state.sock, Reason, State),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------ internals -------------------------

%% 接受信息.
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

%% 下面为读包和解包的代码.
handle_socket_data(Data, State) ->
    case State#state.receive_status of
        ?WAIT_HEADER ->
            <<?MAGIC_CODE:32, Cmd:32, BodyLen:32>> = Data,
            Ref = async_recv(State#state.sock, BodyLen, ?TCP_TIMEOUT),
            NewState = #state{receive_status=?WAIT_BODY, sock=State#state.sock, 
                                sock_ref=Ref, receive_cmd=Cmd},
            {ok, undefined, NewState};
        ?WAIT_BODY ->
            {Module, Method, Record} = decode:decode_packet(State#state.receive_cmd, Data),
            Ref = async_recv(State#state.sock, 12, ?HEART_TIMEOUT),
            NewState = #state{receive_status=?WAIT_HEADER, sock=State#state.sock, 
                                sock_ref=Ref, receive_cmd=undefined},
            {ok, {Module, Method, Record}, NewState}
    end.

%% 因为一些不可预知的原因丢失玩家
lost_client(Sock, _Reason, _State) ->
    io:format("Close socket. ~n"),
    gen_tcp:close(Sock).

%% 处理封包
handle_packet({Module, Method, Record}, _State) ->
    io:format("Module:~p Method:~p Record:~p ~n", [Module, Method, Record]).