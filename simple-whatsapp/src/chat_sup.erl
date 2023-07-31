% A supervisor which listens on a tcp port
-module(chat_sup).
-behaviour(supervisor).
-export([start_link/0,init/1,spawn_chat_process/0]).

start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init([]) ->
    % Starts a tcp listener on a port
    {ok,Port} = application:get_env(port),
    {ok,ListenSocket} = gen_tcp:listen(Port,[binary,{packet,0},{active,once}]),
    % Spawns 20 standby chat processes, this is for minimizing accept latency and make the application
    % to be able to serve queries as soon as possible
    spawn_link(fun create_standby_processes/0),
    % Child specification for the supervisor
    {ok,{{simple_one_for_one,60,3600},
    [{chat_process,
        {chat_process,start_link,[ListenSocket]}, % passing the listen socket as args to all childs
        transient,1000,worker,[chat_process]}
    ]}}.

% Dynamically spawns a chat_process to the supervisor
spawn_chat_process() ->
    supervisor:start_child(?MODULE,[]).


% Start with 20 listeners so that many multiple connections can
% be started at once, without serialization.
create_standby_processes() ->
    [spawn_chat_process() || _ <- lists:seq(1,20)],
    ok.
   