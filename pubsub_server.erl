-module(pubsub_server).
-export([start/0,loop/1]).

start() ->
    PubSubEngine = #{clients=>[],topic=>[]},
    spawn(pubsub_server,loop,[PubSubEngine]).

publishMessage(Client,Publisher,Message) ->
    if Client =/= Publisher ->
        Client ! {newmessage,{Message}};
    true -> ok
    end.

loop(PubSubEngine) ->
    receive
        {Client,{subscribe}} ->
            CurrentClients = maps:get(clients,PubSubEngine),
            NewPubSubEngine = maps:update(clients,CurrentClients ++ [Client],PubSubEngine),
            Client ! {self(),{ok}},
            loop(NewPubSubEngine);

        {Client,{publish,Message}} ->
            CurrentMessages = maps:get(topic,PubSubEngine),
            NewPubSubEngine = maps:update(topic,CurrentMessages ++ [Message],PubSubEngine),
            CurrentClients = maps:get(clients,PubSubEngine),
            lists:foreach(fun(Subscriber) -> publishMessage(Subscriber,Client,Message) end,CurrentClients),
            loop(NewPubSubEngine)
    end.