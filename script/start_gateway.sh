#!/bin/bash

ulimit -SHn 51200

##获取脚本执行目录
here=`which "$0" 2>/dev/null || echo .`
base="`dirname $here`"
SHELL_DIR=`(cd "$base"; echo $PWD)`

MANAGER_LOG_FILE=/data/erl_game_server/log/manager_node.log
#ARGS=$@
echo $MANAGER_LOG_FILE
COMMAND=`escript $SHELL_DIR/script/host_info.erl start_gateway_distribution server 1 | sed 's/\"//g'; exit $?`

if [ $? -eq 0 ] ; then
	echo "$COMMAND" >> $MANAGER_LOG_FILE
	bash -c "$COMMAND"
else
	echo $COMMAND
	exit
fi
