%%--------------------------------------
%% @Module : map_app
%% @Author : TangYi
%% @Email : tang_yi_1989@qq.com
%% @Created : 2013.10.12
%% @Description : 地图启动程序
%%--------------------------------------


-module(map_app).

-behaviour(application).
-export([start/2, prep_stop/1, stop/1, config_change/3]).

%% @private
start(_StartType, _StartArgs) ->
    {ok, SupPid} = map_sup:start_link(),
    lists:foreach(
        fun({Msg, Thunk}) ->
            io:format("Starting ~-32s ...~n", [Msg]),
            Thunk(),
            io:format("Done~n")
        end,
        [
            {
                "Map Loader Mod",
                fun() ->
                    {ok, _MapLoaderPid} = mod_map_loader:start()
                end
            }
            ,{
                "Map Router",
                fun() ->
                    {ok, MapProcessSupId} = map_process_sup:start(),
                    map_router:start()
                end
            }
            ,{
                "Launch Map Prorcesses",
                fun() ->
                    MapList = mod_map_loader:get_all_maps(),
                    lists:foreach(fun({MapId, _MapType}) ->
                        io:format("Start MapId:~p~n", [MapId]),
                        map_router:create_map_if_not_exist(MapId) 
                    end, MapList)
                end 
            }
        ]), 
    {ok, SupPid}.

%% @private
prep_stop(State) ->
    State.

%% @private
stop(_State) ->
    ok.

%% @private
config_change(_Changed, _New, _Removed) ->
    ok.