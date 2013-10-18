-define(DEBUG(Format, Args), lager:debug(Format, Args)).
-define(DEBUG(Args), lager:debug(Args)).
-define(INFO(Format, Args), lager:info(Format, Args)).
-define(INFO(Args), lager:info(Args)).
-define(NOTICE(Format, Args), lager:notice(Format, Args)).
-define(NOTICE(Args), lager:notice(Args)).
-define(WARNING(Format, Args), lager:warning(Format, Args)).
-define(WARNING(Args), lager:warning(Args)).
-define(ERROR(Format, Args), lager:error(Format, Args)).
-define(ERROR(Args), lager:error(Args)).
-define(CRITICAL(Format, Args), lager:critical(Format, Args)).
-define(CRITICAL(Args), lager:critical(Args)).
-define(ALERT(Format, Args), lager:alert(Format, Args)).
-define(ALERT(Args), lager:alert(Args)).
-define(EMERGENCY(Format, Args), lager:emergency(Format, Args)).
-define(EMERGENCY(Args), lager:emergency(Args)).

%%运行时改变日志等级状态,不建议使用
-define(CHANGE_LEVEL(File, Level), case File =:= console of
					true ->
						lager:set_loglevel(lager_console_backend, Level);
					false ->
						lager:set_loglevel({lager_file_backend,"log/console.log"}, Level)
				  end).

%%日志追踪功能
%%可以设置日志只打印你模块中达到等级要求的日志
-define(TRACE_ERROR(Module, Level), lager:trace_file("log/error.log", [{module, Module}], Level)).
-define(TRACE_CONSOLE(Module, Level), lager:trace_file("log/console.log", [{module, Module}], Level)).
