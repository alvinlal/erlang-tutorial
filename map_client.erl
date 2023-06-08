-module(map_client).
-export([get_value/2,set_value/3,get_size/1,get_keys/1,delete_key/2,key_exists/2]).

get_value(Server,Key) ->
    Server ! {self(),{get,Key}},
    receive
        {Server,{ok,Value}} ->
            Value;
        {Server,error} ->
            error
    end.

set_value(Server,Key,Value) ->
    Server ! {self(),{set,Key,Value}},
    receive
        {Server,{ok,Key,Value}}->
            ok
    end.

get_size(Server) ->
    Server ! {self(),{size}},
    receive
        {Server,{ok,Size}} ->
            Size
    end.

get_keys(Server) ->
    Server ! {self(),{keys}},
    receive
        {Server,{Keys}} ->
            Keys
    end.

delete_key(Server,Key) ->
    Server ! {self(),{delete,Key}},
    receive
        {Server,{ok,Key}}->
            ok
    end.

key_exists(Server,Key) ->
    Server ! {self(),{exists,Key}},
    receive
        {Server,Exists} ->
            Exists
    end.