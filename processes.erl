-module(processes).
-export([report/0]).

report() ->
    receive
        X -> io:format("Received ~p~n",[X/2]),
        report()
    end.