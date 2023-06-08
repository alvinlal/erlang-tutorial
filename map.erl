-module(map).
-export([start/0]).

start() ->
    Bio = #{ name=>"alvin", age=>22 },
    io:format("~w",[Bio]).