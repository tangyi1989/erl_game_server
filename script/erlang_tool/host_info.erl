%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%     host_info读取配置文件，解析出host出来，拼接启动字符串后返回给
%%%     shell控制台
%%% @end
%%% Created : 2013-09-25
%%%-------------------------------------------------------------------

-module(host_info).
-compile(export_all).

-define(ITEM_LIST, [gateway, map, master_host]).
-define(CONFIG_FILE_PATH, "/data/erl_game_server/setting/server.setting").

main(CommandList) ->
	if 
		length(CommandList) < 3 ->
			show_help(),
			halt(4);
		true ->
			{ok,RecList} = file:consult(?CONFIG_FILE_PATH),
			[GatewayConfig, MapConfig, MasterHostConfig] = assert_config(RecList),

			if
				GatewayConfig =:= false ->
					io:format("网关配置有误，请检查~n"),
					halt(1);
				MapConfig =:= false ->
					io:format("地图配置有误，请检查~n"),
					halt(2);
				MasterHostConfig =:= false ->
					io:format("主节点配置有误，请检查~n"),
					halt(3);
				true ->
					[Command, TargeNode, SalveNum] = CommandList,
					MasterHost = get_master_host(RecList),
					CodePath = get_code_path(),
					NewCommand = make_start_command(list_to_atom(Command), list_to_atom(TargeNode), MasterHost, list_to_integer(SalveNum), CodePath),
					io:format("~p~n",[NewCommand]),
					halt(0)
			end
	end.

make_start_command(get_start_command, manager, MasterHost, _SalveNum, CodePath) ->
	Cookie = get_cookie(),
	Command = lists:flatten(lists:concat(
					["/usr/local/bin/erl -name manager@", MasterHost, " ", CodePath, " -detached -setcookie ",
					 Cookie, " -noinput  -env ERL_MAX_ETS_TABLES 500000  +P 250000 +K true +h ",
					 "10240 -smp disable -s manager -master_node manager@", MasterHost])),
	Command.

get_cookie() ->
	"123456".

get_ebin_dir(RootDIR) ->
    List = filelib:wildcard(RootDIR ++ "/*"),
    FoldList2 = lists:foldl(
                  fun(F, Acc) ->
                          case filelib:is_dir(F) of
                              true ->
                                  [F | Acc];
                              false ->
                                  Acc
                          end
                  end, [], List),
    lists:foldl(
      fun(F, Acc) ->
              List2 = filelib:wildcard(F ++ "/*"),
              lists:foldl(
                fun(F2, Acc2) ->
                        case filelib:is_dir(F2) of
                            true ->
                                [F2 | Acc2];
                            false ->
                                Acc2
                        end
                end, Acc, List2)
      end, [RootDIR | FoldList2], FoldList2).

get_code_path() ->
	PathList = get_ebin_dir("/data/erl_game_server/ebin"),
    lists:foldl(
      fun(P, Acc) ->
              [" -pa " ++ P | Acc]
      end, [], PathList).

get_master_host(ConfigList) ->
	{_, MasterHost} = lists:keyfind(master_host, 1, ConfigList),
	MasterHost.

assert_config(ConfigList) ->
	Return = 
		lists:foldl(fun(ConfigItem, Acc) ->
					case lists:keyfind(ConfigItem, 1, ConfigList) of
						false ->
							[false | Acc];
						_ ->
							[true | Acc]
					end
		end, [], ?ITEM_LIST),
	Return.

show_help() ->
	io:format("语法：php host_info.php 命令 节点名称~n"),
	io:format("例如 php host_info.php get_debug_command login~n~n").
