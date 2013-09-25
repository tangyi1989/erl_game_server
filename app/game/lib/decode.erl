%% This is a auto generated file, please do not modify it.

-export([decode_record/3]).

decode_record(?MODULE_LOGIN, ?METHOD_LOGIN, ProtoBuf) -> login_pb:decode_login(ProtoBuf);
decode_record(?MODULE_ROLE, ?METHOD_ROLE_ADD, ProtoBuf) -> login_pb:decode_role_add(ProtoBuf);
decode_record(?MODULE_ROLE, ?METHOD_ROLE_CHOSE, ProtoBuf) -> login_pb:decode_role_chose(ProtoBuf);
decode_record(?MODULE_ROLE, ?METHOD_ROLE_DEL, ProtoBuf) -> login_pb:decode_role_del(ProtoBuf);
decode_record(?MODULE_ROLE, ?METHOD_ROLE_LIST, ProtoBuf) -> login_pb:decode_role_list(ProtoBuf);
decode_record(?MODULE_ROLE, ?METHOD_ROLE_ENTER, ProtoBuf) -> login_pb:decode_role_enter(ProtoBuf).