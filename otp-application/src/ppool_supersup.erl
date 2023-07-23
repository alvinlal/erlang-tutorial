%% Requirements
%% Starting and stopping the application
%% Starting and stopping a particular process pool
%% Running a task in the pool and telling you it can't be started if the pool is full
%% Running a task in the pool if there's room, otherwise keep the calling process waiting while the task is in the queue, Free the caller once the task can be run.
%% Running a task asynchronously in the pool, as soon as possible. If no place is available, queue it up and run it whenever.

-module(ppool_supersup).
-behaviour(supervisor).
-export([start_link/0,start_pool/3,stop_pool/1,init/1]).

%% starts the topmost supervisor
start_link()->
    supervisor:start_link({local,ppool},?MODULE,[]).

%% initializes the topmost supervisor
init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok,{{one_for_one,MaxRestart,MaxTime},[]}}.

%% starts a proccess pool
start_pool(Name,Limit,MFA) ->
    ChildSpec = {Name,{ppool_sup,start_link,[Name,Limit,MFA]},permanent,10500,supervisor,[ppool_sup]},
    supervisor:start_child(ppool,ChildSpec).

%% stops a process pool
stop_pool(Name) ->
    supervisor:terminate_child(ppool,Name),
    supervisor:delete_child(ppool,Name).


%% Brutally kills the topmost supervisor
% stop() -> %% now handled by application behaviour
%     case whereis(ppool) of 
%         P when is_pid(P) ->
%             exit(P,kill);
%     _ -> ok
%     end.