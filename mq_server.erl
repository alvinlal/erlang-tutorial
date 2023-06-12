-module(mq_server).
-export([start/0,loop/1]).

start() ->
    Queue = #{clients=>[],queue=>[],robinIdx=>0},
    spawn(mq_server,loop,[Queue]).

loop(Queue) ->
    receive
        {Client,{register}} -> 
        NewQueue = maps:update(clients,[Client | maps:get(clients,Queue,[])],Queue) , 
        Client ! {self(),{ok}},
        loop(NewQueue);
    
        {Client,{enqueue,Message}} -> 
        NewMessage = #{messageid:erlang:uuid(),message:Message}
        NewQueue =  maps:update(queue,maps:get(queue,Queue,[]) ++ [NewMessage],Queue), 
        RobinIdx = maps:get(robinIdx,Queue),
        ClientPid = lists:nth(RobinIdx,maps:get(clients,Queue)),
        maps:update(robinIdx,RobinIdx+1,Queue),
        ClientPid ! {self(),{newmessage,{NewMessage}}}
        Client ! {self(),{ok}},
        io:format("The queue is now ~w",[NewQueue]),
        loop(NewQueue)
    end.