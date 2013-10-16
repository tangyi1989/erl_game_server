%%%---------------------------------------------------------------------
%%% @Module : map_unit
%%% @Author : TangYi
%%% @Email : tang_yi_1989@qq.com
%%% @Created : 2013.10.12
%%% @Description : 每一个地图单元代表一个控制逻辑的地图
%%%----------------------------------------------------------------------

-module(map_process).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% -------------------------------------------------------------
%% API
%% -------------------------------------------------------------
start_link({MapProcessName, MapId}) ->
    gen_server:start_link(?MODULE, [MapProcessName, MapId], 
        [{spawn_opt, [{min_heap_size, 10*1024}, {min_bin_vheap_size, 10*1024}]}]).

%% -------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------
init([MapProcessName, MapId]) ->
    io:format("MapProcessName:~p MapId:~p ~n", [MapProcessName, MapId]),
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------
%% Internals
%% -------------------------------------------------------------
