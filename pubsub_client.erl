-module(pubsub_client).
-export([subscribe/1,onMessage/1,publishMessage/2]).

subscribe(Server) ->
    Server ! {self(),{subscribe}},
    receive
        {Server,{ok}} ->
            io:format("successfully subscribed to server ~w~n",[Server]),
            
    end.

publishMessage(Server,Message) ->
    Server ! {self(),{publish,Message}},
    receive
        {Server,{ok}} ->
            io:format("successfully published message \"~w\" to ~w~n",[Message,Server])
    end.

onMessage(Server) ->
    receive
        {newmessage,{Message}} ->
            io:format("[X] received new message:- ~w~n",[Message]),
            onMessage(Server)
    end.