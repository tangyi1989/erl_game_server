%%%---------------------------------------------------------------------
%%% @Module : common_tool
%%% @Author : TangYi
%%% @Email : tang_yi_1989@qq.com
%%% @Created : 2013.10.17
%%% @Description : 公共工具函数模块
%%%---------------------------------------------------------------------

-module(common_tool).

-export([to_integer/1,
         ceil/1,
         floor/1]).

%%%-------------------------------------------------------------------
%%% @doc
%%%     将其他类型(integer, binary, list, float)转换成整数
%%% @end
%%%-------------------------------------------------------------------
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
    Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> 
    round(Msg);
to_integer(_Msg) ->
    throw(other_value).

%%%-------------------------------------------------------------------
%%% @doc
%%%     向上取整
%%% @end
%%%-------------------------------------------------------------------
ceil(X) ->
    T = trunc(X),
    if
        X - T == 0 ->
            X;
        true ->
            if 
                X > 0 ->
                    T + 1;
                true ->
                    T
            end
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     向下取整
%%% @end
%%%-------------------------------------------------------------------
floor(X) ->
    T = trunc(X),
    if 
        X - T == 0 ->
            T;
        true ->
            if
                X > 0 ->
                    T;
                true ->
                    T-1
            end
    end.