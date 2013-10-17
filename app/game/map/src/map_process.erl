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
            put(is_map_process, true),
            process_flag(trap_exit, true),
            ok = init_map_process(MapProcessName, MapId),
            {ok, #map_process_record{map_id=MapId}};
        _ -> {stop, already_registered}
    end.

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
                put({slice_role, SliceName}, [])
            end, 
            lists:seq(0, SMaxX))  
        end, 
    lists:seq(0, SMaxY)),

    %% 构造九宫格
    lists:foreach(fun(SY) ->
            lists:foreach(fun(SX) -> 
                Slices9 = mod_map_slice:get_9slices(SMaxX, SMaxY, SX, SY),
                io:format("Slices 9:~p~n", [Slices9]),
                put({slices, SX, SY}, Slices9)
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
                _ -> put({can_walk, Tx, Ty}, true)
            end
        end, 
    GridList),
    ok. 

