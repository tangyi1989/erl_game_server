%%----------------------------------
%% File : common.hrl
%% Author : Tang
%% Created : 2013-09-19
%% Description : 公共定义
%%----------------------------------

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, 
    {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).

