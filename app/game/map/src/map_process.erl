%%%---------------------------------------------------------------------
%%% @Module : map_process
%%% @Author : TangYi
%%% @Email : tang_yi_1989@qq.com
%%% @Created : 2013.10.16
%%% @Description : 每一个地图单元代表一个控制逻辑的地图
%%%----------------------------------------------------------------------

-module(map_process).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(map_process_record, {map_id=undefined}).
-include("map.hrl").

%% -------------------------------------------------------------
%% API
%% -------------------------------------------------------------
start_link({MapProcessName, MapIdIn}) ->
    gen_server:start_link(?MODULE, [MapProcessName, MapIdIn], 
        [{spawn_opt, [{min_heap_size, 10*1024}, {min_bin_vheap_size, 10*1024}]}]).

%% -------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------
init([MapProcessName, MapIdIn]) ->
    MapId = common_tool:to_integer(MapIdIn), 
    case global:register_name(MapProcessName, self()) of 
        yes ->
            erlang:put(is_map_process, true),
            process_flag(trap_exit, true),
            ok = init_map_process(MapProcessName, MapId),
            {ok, #map_process_record{map_id=MapId}};
        _ -> {stop, already_registered}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', PID, Reason}, State) ->
    MapId = State#map_process_record.map_id,
    io:format("WARNING: map exit: MapID=~w,Reason=~w,PID=~w,State=~w", 
        [MapId, Reason, PID, State]),
    {stop, normal};
handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Exception ->
            MapId = State#map_process_record.map_id,
            io:format("Error when hanle info - MapId:~p  Info:~p Exception:~p ~n",
                [MapId, Info, Exception])
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------
%% Internals
%% -------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @doc
%%%     处理消息
%%% @end
%%%-------------------------------------------------------------------
do_handle_info(loop_ms, _State) ->
    erlang:send_after(200, self(), loop_ms),
    io:format("loop_ms~n");

do_handle_info(loop_second, _State) ->
    erlang:send_after(1000, self(), loop_second),
    io:format("loop_second~n");

do_handle_info(loop_minute, _State) ->
    erlang:send_after(60000, self(), loop_minute),
    io:format("loop_minute~n").

%%%-------------------------------------------------------------------
%%% @doc
%%%     初始化地图进程，读取地图，初始化九宫格，初始化Role数据。
%%% @end
%%%-------------------------------------------------------------------
init_map_process(MapProcessName, MapId) ->
    io:format("Init MapProcessName:~p MapId:~p ~n", [MapProcessName, MapId]),
    case ets:lookup(?ETS_IN_MAP_DATA, MapId) of
        [{MapId, {MapId, _MapType, GridWidth, GridHeight, GridList}}] -> 
            ok = init_map_slices(GridWidth, GridHeight),
            ok = init_map_grid(GridList),
            erlang:send(self(), loop_ms),
            erlang:send(self(), loop_second),
            erlang:send(self(), loop_minute),
            ok;
        What ->
            {error, "Map not found or data not match"}
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     将地图构造成一个一个的格子，然后组成一个个九宫格的结构。
%%% @end
%%%-------------------------------------------------------------------
init_map_slices(GridWidth, GridHeight) ->
    io:format("Init Slices"),
    %% 格子的最大长度和宽度
    SMaxY = common_tool:ceil(GridWidth / ?MAP_SLICE_WIDTH) - 1,
    SMaxX = common_tool:ceil(GridHeight / ?MAP_SLICE_HEIGHT) - 1,

    %% 构造格子
    lists:foreach(fun(SY) ->
            lists:foreach(fun(SX) -> 
                SliceName = mod_map_slice:get_slice_name(SX, SY),
                erlang:put({slice_role, SliceName}, [])
            end, 
            lists:seq(0, SMaxX))  
        end, 
    lists:seq(0, SMaxY)),

    %% 构造九宫格
    lists:foreach(fun(SY) ->
            lists:foreach(fun(SX) -> 
                Slices9 = mod_map_slice:get_9slices(SMaxX, SMaxY, SX, SY),
                io:format("Slices 9:~p~n", [Slices9]),
                erlang:put({slices, SX, SY}, Slices9)
            end, 
            lists:seq(0, SMaxX))  
        end, 
    lists:seq(0, SMaxY)),
    ok.

%%%-------------------------------------------------------------------
%%% @doc
%%%     构造地图的格子，现在只是行走数据
%%% @end
%%%-------------------------------------------------------------------
init_map_grid(GridList) ->
    lists:foreach(
        fun({{Tx, Ty}, Type}) -> 
            case Type of
                _ -> erlang:put({can_walk, Tx, Ty}, true)
            end
        end, 
    GridList),
    ok. 
