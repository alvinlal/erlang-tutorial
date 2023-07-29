-module(iolists).
-export([foo/0]).

foo() ->
    IoList = [$H, $e, [$l, <<"lo">>, " "], [[["W","o"], <<"rl">>]] | [<<"d">>]],
    io:format("~s~n",[IoList]).