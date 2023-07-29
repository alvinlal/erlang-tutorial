-module(tcpsocket).
-export([open_socket/0]).

open_socket()->
    gen_tcp:open(8789,[binary,{active,true}]).