-module(recursion).
-export([count_up/1]).

count_up(Limit) ->
    count_up(1,Limit).

count_up(Accumulator,Limit) when Accumulator=<Limit ->
    io:format("~w!~n",[Accumulator]),
    count_up(Accumulator+1,Limit);

count_up(Accumulator,Limit) ->
    io:format("Finished!~n").