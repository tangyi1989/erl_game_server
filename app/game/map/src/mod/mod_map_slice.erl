%%%---------------------------------------------------------------------
%%% @Module : common_tool
%%% @Author : TangYi
%%% @Email : tang_yi_1989@qq.com
%%% @Created : 2013.10.17
%%% @Description : 九宫格相关的函数
%%%---------------------------------------------------------------------

-module(mod_map_slice).

-export([get_slice_name/2,
         get_9slices/4]).

%%%-------------------------------------------------------------------
%%% @doc
%%%     构造一个格子分片的名字。
%%% @end
%%%-------------------------------------------------------------------
get_slice_name(X, Y) ->
    lists:concat(['slice', '_', X , '_', Y]).

%%%-------------------------------------------------------------------
%%% @doc
%%%     获取一个格子所在的九宫格。
%%% @end
%%%-------------------------------------------------------------------
get_9slices(SMaxX, SMaxY, X, Y) ->
    if 
        X > 0 ->
            BeginX = X - 1;
        true ->
            BeginX = 0
    end,
    if
        Y > 0 ->
            BeginY = Y -1;
        true ->
            BeginY = 0
    end,

    if
        X < SMaxX ->
            EndX = X + 1;
        true ->
            EndX = SMaxX
    end,
    if
        Y < SMaxY ->
            EndY = Y + 1;
        true ->
            EndY = SMaxY
    end,
    get_slices(BeginX, BeginY, EndX, EndY).

%%%-------------------------------------------------------------------
%%% @doc
%%%     根据起始坐标和结束坐标得到这个范围内的所有格子分片
%%% @end
%%%-------------------------------------------------------------------
get_slices(BeginX, BeginY, EndX, EndY) ->
    lists:foldl(
        fun (SX, SliceList) ->
            lists:foldl(
                fun(SY, SliceListSub) -> 
                    SliceName = get_slice_name(SX, SY),
                    [SliceName | SliceListSub]
                end, 
                SliceList, lists:seq(BeginY, EndY))
    end, 
    [], lists:seq(BeginX, EndX)).