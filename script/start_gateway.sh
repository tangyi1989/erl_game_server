#!/bin/bash

ulimit -SHn 51200

##获取脚本执行目录
here=`which "$0" 2>/dev/null || echo .`
base="`dirname $here`"
echo base
SHELL_DIR=`(cd "$base"; echo $PWD)`

#ARGS=$@
COMMAND=`escript $SHELL_DIR/script/host_info.erl start_gateway_distribution server 1 | sed 's/\"//g'; exit $?`
echo $COMMAND
if [ $? -eq 0 ] ; then
	bash -c "$COMMAND"
else
	echo $COMMAND
	exit
fi
