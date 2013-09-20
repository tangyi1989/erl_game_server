
-module(test_client).

-export([test/0]).

test() ->
    {ok, _} = client_sup:start_link(),
    {ok, _} = supervisor:start_child(client_sup, []),
    ok.
