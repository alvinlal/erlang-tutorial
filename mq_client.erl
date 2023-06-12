-module(mq_client).
-export([register/1,enqueue/2,onMessages/1]).

register(Server) ->
    Server ! {self(),{register}},
    receive
        {Server,{ok}} ->
            io:format("successfully registered ~w~n",[self()])
    end.

enqueue(Server,Message) ->
    Server ! {self(),{enqueue,Message}},
    receive
        {Server,{ok}} ->
            io:format("successfully enqueued item ~w~n",[Message])
    end.

onMessages(Server) ->
    receive
        {Server,{newmessage,{NewMessage}}} ->
            io:format("received message ~w~n",[NewMessage]),
            Server ! {messagehandled,{maps:get(messageid,NewMessage)}},
        onMessages(Server)
    end.