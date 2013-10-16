%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%             启动节点供外部调用的接口
%%% @end
%%% Created : 2013-09-28
%%%-------------------------------------------------------------------

-module(common_node).
-compile(export_all).


module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
            io:format("WARNING: module ~p not found, so not scanned for boot steps.~n",
                      [Module]),
            [];
        {'EXIT', Reason} ->
            exit(Reason);
        V ->
            V
    end.

all_module_attributes(Module, Name) ->
    %%lists:foldl(
    %%  fun (Module, Acc) ->
	io:format("~p~n",[module_attributes(Module)]),
    [Atts || {N,Atts} <- module_attributes(Module), N=:= Name].
    %%case lists:append([Atts || {N, Atts} <- module_attributes(Module),
     %%                                    N =:= Name]) of
     %%   []   -> [];
     %%   Atts -> [Atts | Acc]
   %%end.
    %%  end, [], Modules).
