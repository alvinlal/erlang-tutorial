-module(linkmon).
-export([myproc/0,chain/1,testProc/0]).


myproc() ->
    timer:sleep(2000),
    exit(reason).

testProc() ->
    io:format("i am a testproc~n").

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;

chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.