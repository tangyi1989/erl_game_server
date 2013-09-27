%%%-------------------------------------------------------------------
%%% @author lianweijie <jjchen.lian@gmail.com>
%%% @copyright (C) 2013, gmail.com
%%% @doc
%%%		负责启动其他节点
%%% @end
%%% Created : 2013-09-25
%%%-------------------------------------------------------------------

-module(manager_node).

-define(CONFIG_FILE_PATH, "/data/erl_game_server/setting/server.setting").
-define(ITEM_LIST, [gateway, map, master_host]).

%%%-------------------------------------------------------------------
%%% @doc
%%%		在manager节点处启动其他节点
%%% @end
%%%-------------------------------------------------------------------
start() ->
	yes = global:register_name(manager_node, erlang:self()),
	{ok,RecList} = file:consult(?CONFIG_FILE_PATH),
	[GatewayConfig, _, _] = assert_config(RecList),
	start_gateway_node(RecList, GatewayConfig),
	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%		启动网关节点,并且等待网关节点的响应
%%% @end
%%%-------------------------------------------------------------------
start_gateway_node(RecList, GatewayConfig) ->
	Command = get_start_gateway_command(RecList, GatewayConfig),
	erlang:open_port({spawn, Command}, [stream]),
	receive 
       	{gateway_node_up, NodeName} ->
            net_kernel:connect_node(NodeName2)                                                
    end,
	do.

%%%-------------------------------------------------------------------
%%% @doc
%%%		生成启动网关节点命令行
%%% @end
%%%-------------------------------------------------------------------
get_start_gateway_command(RecList, GatewayConfig) ->
	MasterHost = get_master_host(RecList),
	CodePath = get_code_path(),
	Cookie = get_cookie(),
	Command = lists:flatten(lists:concat(
					["/usr/local/bin/erl -name gateway@", MasterHost, " ", CodePath, " -detached -setcookie ",
					 Cookie, " -noinput  -env ERL_MAX_ETS_TABLES 500000  +P 250000 +K true +h ",
					 "10240 -smp disable -s gateway -master_node gateway@", MasterHost])),
	Command.
	do.

get_master_host(ConfigList) ->
	{_, MasterHost} = lists:keyfind(master_host, 1, ConfigList),
	MasterHost.

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