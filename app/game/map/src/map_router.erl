%%-------------------------------------------------------------
%% @Module : mod_map_loader
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.10.16
%% @Description : 控制地图创建的模块，目前这是一个单机版本的。
%%-------------------------------------------------------------

-module(map_router).

-export([start/0,
         start_link/0,
         create_map_if_not_exist/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------
%% API
%% ------------------------------------------------------------

start() ->
    supervisor:start_child(map_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            transient, 30000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

create_map_if_not_exist(MapId) ->
    MapProcessName = common_map:get_common_map_name(MapId),
    case global:whereis_name(MapProcessName) of
        undefined ->
            gen_server:call(?SERVER, {do_start_map, {MapProcessName, MapId}});
        _ -> ok
    end.

%% -------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------
init({}) ->
    {ok, undefined}.

handle_call({do_start_map, {MapProcessName, MapId}}, _From, State) ->
    case supervisor:start_child(map_process_sup, [{MapProcessName, MapId}]) of
        {ok, NewMapProcessPid} ->
            io:format("Lanunch map ~p success ~n", [MapProcessName]),
            {reply, {ok, NewMapProcessPid}, State};
        {already_started, Pid} ->
            {reply, {ok, Pid}, State};
        Reason ->
            io:format("Lauanch map ~p error~n ", [MapId]),
            {reply, {errror, Reason}, State}
    end;
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