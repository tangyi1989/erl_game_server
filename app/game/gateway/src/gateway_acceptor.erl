%%-------------------------------------
%% @Module : gateway_listener
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.09.19
%% @Description : TCP 监听
%%-------------------------------------


-module(gateway_acceptor).
-behaviour(gen_server).

-record(state, {sock, ref}).

-export([start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, 
    terminate/2, code_change/3]).


%% -------------------------------- API ------------------------------------

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% ---------------------------- gen_server callbacks ------------------------

init([LSock]) ->
    gen_server:cast(self(), accept),
    {ok, #state{sock=LSock}}.

%% 这个是我们最要注意的
handle_info({inet_async, LSock, Ref, {ok, Sock}}, State=#state{sock=LSock, ref=Ref}) ->
    case set_sockopt(LSock, Sock) of
        ok -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
    end,
    start_client(Sock),
    accept(State);

handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{sock=LSock, ref=Ref}) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(accept, State) ->
    accept(State).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------- internal functions ---------------------------

%% 目前这些设置也没有搞清楚。
set_sockopt(LSock, Sock) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Sock, Opts) of
                ok -> ok;
                Error -> gen_tcp:close(Opts),
                        Error
            end;
        Error ->
            gen_tcp:close(Sock),
            Error
    end.

accept(State=#state{sock=LSock}) ->
    %% 这个函数现在还没有搞清楚
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error -> {stop, {cannot_accept, Error}, State}
    end.

%% 开启客户端进程，突然觉得这段代码才是erlang的真谛。
start_client(Sock) ->
    {ok, ClientPid} = supervisor:start_child(client_sup, []),
    ok = gen_tcp:controlling_process(Sock, ClientPid),
    ClientPid ! {start, Sock}.





