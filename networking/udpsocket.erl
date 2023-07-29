-module(udpsocket).
-export([open_socket/0]).

open_socket()->
    gen_udp:open(8789,[binary,{active,true}]).