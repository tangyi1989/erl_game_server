-ifndef(LOGIN_PB_H).
-define(LOGIN_PB_H, true).
-record(login, {
    name = erlang:error({required, name}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(RESPONSE_PB_H).
-define(RESPONSE_PB_H, true).
-record(response, {
    result = erlang:error({required, result}),
    description = erlang:error({required, description})
}).
-endif.

