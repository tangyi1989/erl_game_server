%%-------------------------------------------------------------
%% @Module : mod_map_loader
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.10.12
%% @Description : 地图加载模块
%%-------------------------------------------------------------

-module(mod_map_loader).

-export([start/0,
         start_link/0,
         get_all_maps/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("map.hrl").

%% -------------------------------------------------------------
%% API
%% -------------------------------------------------------------
start() ->
    supervisor:start_child(map_sup, {?MODULE,
                                     {?MODULE, start_link, []},
                                     permanent, 10000, worker,
                                     [?MODULE]}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

get_all_maps() ->
    gen_server:call(?SERVER, {get_all_maps}).

%% -------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------
init({}) ->
    init_ets(),
    load_map_data(),
    {ok, undefined}.

handle_call({get_all_maps}, _From, State) ->
    {reply, ets:tab2list(?ETS_MAPS), State};
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
%% internals
%% -------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @doc
%%%     初始化一些要用到的ets表
%%% @end
%%%-------------------------------------------------------------------
init_ets() ->
    %%保存各个地图的详细信息
    ets:new(?ETS_IN_MAP_DATA, [protected, bag, named_table]),
    %%地图ID列表
    ets:new(?ETS_MAPS, [protected, set, named_table]),
    ok.

%%%-------------------------------------------------------------------
%%% @doc
%%%     载入所有地图数据
%%% @end
%%%-------------------------------------------------------------------
load_map_data() ->
    MapConfDir = common_config:get_map_config_dir(),
    ExtName = ".mcm",
    try file:list_dir(MapConfDir) of
        {ok, FileList} ->
            lists:foreach(
                fun(FileName) ->
                    case filename:extension(FileName) of
                        ExtName ->
                            load_mcm_map_file(MapConfDir, FileName);
                        _ ->
                            ok
                    end
                end,FileList);
        {error, Reason} ->
            io:format("Cannot open map dir, reason:~p~n", [Reason])
    catch
        _ ->
            io:format("load map data error")
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     读取mcm地图文件
%%% @end
%%%-------------------------------------------------------------------
load_mcm_map_file(ConfDir, FileName) ->
    %% 读取mcm文件并进行解压
    io:format("loading Mcm File:~p~n", [FileName]),

    McmFilePath = lists:concat([ConfDir, FileName]),
    io:format("~p~n", [McmFilePath]),
    {ok, CompressedBin} = file:read_file(McmFilePath),
    RawBin = zlib:uncompress(CompressedBin),

    %% 创建临时用表
    ets:new(?ETS_MAP_DATA_TMP,[set, private, named_table]),

    %% 读取文件的header部分
    <<MapId:32, MapType:32, _MapName:256, _:256, TileRow:32, 
      TileCol:32, ElementNum:32, JumpPointNum:32, OffsetX:32, 
      OffsetY:32, TW:32, TH:32, DataSection/binary>> = RawBin,

    %% 读取Tile部分
    TileLength = TileRow * TileCol * 8,
    <<TileData:TileLength/bitstring, DataRemain/binary>> = DataSection,
    ok = load_mcm_tile(TileData, 0, 0, TileCol),

    %% 读取怪物和NPC数据
    {ok, JumpTileSection} = load_mcm_element_tile(ElementNum, DataRemain, MapId),
    
    %% 读取地图上的传送点
    ok = load_mcm_jump_tile(JumpPointNum, JumpTileSection, MapId),

    Data = ets:tab2list(?ETS_MAP_DATA_TMP),

    %% 删除掉临时用表
    ets:delete(?ETS_MAP_DATA_TMP),

    %% 检查是否有地图重复,如果有的话直接抛出异常。
    case get(map_id_list) of
        undefined ->
            put(map_id_list, [MapId]);
        MapIdList ->
            case lists:member(MapId, MapIdList) of
                true -> throw({error, map_id_error, MapId, FileName});
                _ -> put(map_id_list, [MapIdList])
            end  
    end,

    ets:insert(?ETS_IN_MAP_DATA, {MapId, {MapId, Data}}),
    ets:insert(?ETS_MAPS, {MapId, MapType}),
    ok.

%%%-------------------------------------------------------------------
%%% @doc
%%%     读取地图坐标数据(格子数据)
%%% @end
%%%-------------------------------------------------------------------
load_mcm_tile(<<>>, _Tx, _Ty, _TileCol) ->
    ok;
load_mcm_tile(TileData, Tx, Ty, TileCol) ->
    <<Tile:8/bitstring, DataRemain/binary>> = TileData,
    
    <<_YuLiu:1, Arena:1, Sell:1, AllSafe:1, Safe:1, _Run:1, _Alpha:1, Exist:1>> = Tile,
    case Exist =:= 1 of 
        true ->
            %%io:format("Tx:~p Ty:~p Tile:~p~n", [Tx, Ty, Tile]),
            ets:insert(?ETS_MAP_DATA_TMP, {{Tx, Ty}, reversed});
        _ ->
            ok
    end,
    case Tx + 1 >= TileCol of
        true ->
            load_mcm_tile(DataRemain, 0, Ty + 1, TileCol);
        _ ->
            load_mcm_tile(DataRemain, Tx + 1, Ty, TileCol)
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     读取地图npc，怪物数据
%%% @end
%%%-------------------------------------------------------------------
load_mcm_element_tile(0, DataBin, _MapId) ->
    {ok, DataBin};
load_mcm_element_tile(ElementNum, DataBin, MapId) ->
    <<ID:32, IndexTx:32, IndexTy:32, Type:32, Link:32, DataRemain/binary>> = DataBin,
    %%io:format("Tx:~p, Ty:~p: ID:~p~n", [IndexTx, IndexTy, ID]),
    DataLength = Link * 8,
    <<_:DataLength/bitstring, DataRemain2/binary>> = DataRemain,
    load_mcm_element_tile(ElementNum-1, DataRemain2, MapId).

%%%-------------------------------------------------------------------
%%% @doc
%%%     读取地图上的传送点
%%% @end
%%%-------------------------------------------------------------------
load_mcm_jump_tile(0, _DataBin, _MapId) ->
    ok;
load_mcm_jump_tile(JumpPointNum, DataBin, MapId) ->
    <<_ID:32, IndexTX:32, IndexTY:32,TargetMapID:32, 
      TIndexTX:32, TIndexTY:32, _HW:32, _YL:32, _WL:32, 
      _MinLevel:32, _MaxLevel:32, Link:32, DataRemain/bitstring>> = DataBin,
    io:format("Map Jump Point To Map:~p~n", [TargetMapID]),
    DataLength = Link * 8,
    <<_:DataLength/bitstring, DataRemain2/binary>> = DataRemain,
    load_mcm_jump_tile(JumpPointNum-1, DataRemain2, MapId).

