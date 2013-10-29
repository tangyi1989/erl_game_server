-module(test).

main(_CommandList) ->
    lists:foreach(
        fun ({Format, Args})->
            io:format(Format, Args) 
        end, [
            {
                "Env : ~p~n",
                [os:getenv()]
            },
            {
                "CWD : ~p~n",
                [file:get_cwd()]
            },
            {
                "Which application : ~p~n",
                [application:which_applications()]
            },
            {
                "get_application : ~p~n",
                [application:get_application()]
            },
            {
                "get_all_env : ~p~n",
                [application:get_all_env()]
            },
            {
                "lib_dir : ~p~n",
                [code:lib_dir()]
            }
        ]) .