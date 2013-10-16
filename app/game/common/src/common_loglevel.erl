%%%----------------------------------------------------------------------
%%% File    : common_loglevel.erl
%%% Author  : Liang
%%% Purpose : Game Server logging.
%%% Created : 2013-10-14
%%% Description: 日志公用接口
%%%----------------------------------------------------------------------

-module(common_loglevel).

-export([set/1, get/0]).

-define(LOGMODULE, "error_logger").

%% Error levels:
-define(LOG_LEVELS,[ {0, no_log, "No log"}
                     ,{1, critical, "Critical"}
                     ,{2, error, "Error"}
                     ,{3, warning, "Warning"}
                     ,{4, info, "Info"}
                     ,{5, debug, "Debug"}
                     ,{6, dev, "Dev"}
                    ]).

get() ->
    Level = common_logger:get(),
    case lists:keysearch(Level, 1, ?LOG_LEVELS) of
        {value, Result} -> Result;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));

set(Loglevel) when is_integer(Loglevel) ->
    try
        {Mod,Code} = dynamic_compile:from_string(common_logger_src(Loglevel)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> io:format("Error compiling logger (~p): ~p~n", [Type, Error])
    end;


set(_) ->
    exit("Loglevel must be an integer").

level_to_integer(Level) ->
    case lists:keysearch(Level, 2, ?LOG_LEVELS) of
        {value, {Int, Level, _Desc}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the common logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
common_logger_src(Loglevel) ->
    L = integer_to_list(Loglevel),
    "-module(common_logger).

    -export([
			debug_msg/5,
             info_msg/5,
             warning_msg/5,
             error_msg/5,
             critical_msg/5,
             dev/5,
			 m_debug_msg/5,
             m_info_msg/5,
             m_warning_msg/5,
             m_error_msg/5,
             m_critical_msg/5,
             m_dev/5,
             get/0]).

    get() -> "++ L ++".

    %% Helper functions
    debug_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    debug_msg(_,_,_,_,_) -> ok.

    info_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    info_msg(_,_,_,_,_) -> ok.

    warning_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    warning_msg(_,_,_,_,_) -> ok.

    error_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            notify(error,
                   \"E(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    error_msg(_,_,_,_,_) -> ok.

    %% 极度严重错误
    critical_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    critical_msg(_,_,_,_,_) -> ok.

    dev(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 6 ->
            notify(info_msg,
                   \"DEV(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    dev(_,_,_,_,_) -> ok.
	
	
	
	%% Helper functions
    m_debug_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            m_notify(info_msg,
                   \"D(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    m_debug_msg(_,_,_,_,_) -> ok.

    m_info_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            m_notify(info_msg,
                   \"I(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    m_info_msg(_,_,_,_,_) -> ok.

    m_warning_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            m_notify(error,
                   \"W(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    m_warning_msg(_,_,_,_,_) -> ok.

    m_error_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            m_notify(error,
                   \"E(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    m_error_msg(_,_,_,_,_) -> ok.

    %% 极度严重错误
    m_critical_msg(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            m_notify(error,
                   \"C(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    m_critical_msg(_,_,_,_,_) -> ok.

    m_dev(ServerName, Module, Line, Format, Args) when " ++ L ++ " >= 6 ->
            m_notify(info_msg,
                   \"DEV(~p:~p:~p:~p) : \"++Format++\"~n\",
                   [ServerName, self(), Module, Line]++Args);
    m_dev(_,_,_,_,_) -> ok.
	
	%% Distribute the message to the Erlang error logger
    m_notify(Type, Format, Args) ->
            LoggerMsg = {module, Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
