%%----------------------------------
%% File : common.hrl
%% Author : Tang
%% Created : 2013-09-19
%% Description : 公共定义
%%----------------------------------

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, 
    {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).

%%数据库连接
-define(DB, game_server_mysql_conn). % mysql driver 的链接池ID
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "root").
-define(DB_PASS, "tang").
-define(DB_NAME, "game_server").
-define(DB_ENCODE, utf8).

%%ETS
-define(ETS_ONLINE, ets_online).


-define(SYSTEM_LOG(Format, Args), global:send(manager_log, {system_log, erlang:localtime(), Format, Args})).
