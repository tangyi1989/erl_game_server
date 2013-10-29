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
-define(POSSIBLE_CONFIG_PATH, ["../setting", "./setting","/etc/erl_game_server",
    "/data/erl_game_server/setting"]).
-define(POSSIBLE_EBIN_PATH, ["\~/ebin/common/", "/data/erl_game_server/ebin"]).

main(CommandList) ->
	if 
		length(CommandList) < 3 ->
			show_help(),
			halt(4);
		true ->
			{ok,RecList} = get_server_setting(?POSSIBLE_CONFIG_PATH),
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

make_start_command(start_manager_command, TargeNode, MasterHost, _SalveNum, CodePath) ->
	Cookie = get_cookie(),
	Command = lists:flatten(lists:concat(
					["/usr/local/bin/erl -name manager@", MasterHost, " ", CodePath, " -detached -setcookie ",
					 Cookie, " -noinput -env ERL_MAX_ETS_TABLES 500000 +P 250000 +K true +h ",
					 "10240 -smp disable -s ", TargeNode, " -master_node manager@", MasterHost])),
	Command;

make_start_command(start_gateway_command, TargeNode, MasterHost, _SalveNum, CodePath) ->
    Cookie = get_cookie(),
	Command = lists:flatten(lists:concat(
					["/usr/local/bin/erl -name server@", MasterHost, " ", CodePath, " -detached -setcookie ",
					 Cookie, " -noinput -env ERL_MAX_ETS_TABLES 500000 +P 250000 +K true +h ",
					 "10240 -smp disable -s ", TargeNode, " -master_node manager@", MasterHost])),
	Command;

make_start_command(start_logger_command, TargeNode, MasterHost, _SalveNum, CodePath) ->
	Cookie = get_cookie(),
	Command = lists:flatten(lists:concat(
					["/usr/local/bin/erl -name logger@", MasterHost, " ", CodePath, " -detached -setcookie ",
					Cookie, " -noinput -env ERL_MAX_ETS_TABLES 500000 +P 2500000 +K true +h ",
					"10240 -smp disable -s ", TargeNode, " -master_node manager@", MasterHost])),
	Command.

get_cookie() ->
	"123456".

replace_home_path(Path) ->
    case string:substr(Path, 1, 1) of
        "\~" ->
            HomePath = os:getenv("HOME"),
            HomePath ++ string:substr(Path, 2);
        _ ->
            Path
    end.

get_exist_path([]) ->
    {error, cannot_find_path};
get_exist_path([Path | RemainPaths]) ->
    ReplacedPath = replace_home_path(Path),
    case filelib:is_dir(ReplacedPath) of
        true -> {ok, ReplacedPath};
        _ -> get_exist_path(RemainPaths)
    end.

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
    {ok, EbinPath} = get_exist_path(?POSSIBLE_EBIN_PATH),
	PathList = get_ebin_dir(EbinPath),
    lists:foldl(
      fun(P, Acc) ->
              [" -pa " ++ P | Acc]
      end, [], PathList).

get_server_setting([]) ->
    {error, cannot_find_path};
get_server_setting([PossiblePath | RemainPaths]) ->
    ConfigFile = PossiblePath ++ "/server.setting",
    case filelib:is_file(ConfigFile) of
        true -> file:consult(ConfigFile);
        _ -> get_server_setting(RemainPaths)
    end.

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
	io:format("语法：escript host_info.erl 命令 节点名称~n"),
	io:format("例如 escript host_info.erl get_debug_command login~n~n").
