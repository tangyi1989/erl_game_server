%% This is a auto generated file, please do not modify it.

-module(decode).
-include("proto.hrl").
-export([decode_packet/2]).

decode_packet(?METHOD_LOGIN, ProtoBuf) -> {?MODULE_LOGIN, ?METHOD_LOGIN, login_pb:decode_login(ProtoBuf)}.