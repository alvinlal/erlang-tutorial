-module(ppool_sup).
-export([start_link/3, init/1]).
-behaviour(supervisor).

%% starts the pool supervisor
start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

%% initializes the pool supervisor
init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok,
        {{one_for_all, MaxRestart, MaxTime}, [
            {serv, {ppool_serv, start_link, [Name, Limit, self(), MFA]}, permanent, 5000, worker, [
                ppool_serv
            ]}
        ]}}.
