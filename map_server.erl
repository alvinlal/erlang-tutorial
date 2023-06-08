-module(map_server).
-export([start/0,loop/1]).

start()->
    spawn(map_server,loop,[maps:new()]).

loop(Map)->
    receive
        {Client,{get,Key}} ->
            Client ! {self(),{ok,maps:find(Key,Map)}},
            loop(Map);
        {Client,{set,Key,Value}} ->
            UpdatedMap = maps:put(Key,Value,Map),
            Client ! {self(),{ok,Key,Value}},
            loop(UpdatedMap);
        {Client,{size}} ->
            Client ! {self(),{ok,maps:size(Map)}},
            loop(Map);
        {Client,{keys}} ->
            Client ! {self(),{maps:keys(Map)}},
            loop(Map);
        {Client,{delete,Key}} ->
            UpdatedMap = maps:remove(Key,Map),
            Client ! {self(),{ok,Key}},
            loop(UpdatedMap);
        {Client,{exists,Key}} ->
            Client ! {self(),maps:is_key(Key,Map)}
    end.