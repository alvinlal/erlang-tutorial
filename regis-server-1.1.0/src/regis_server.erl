% Requirements
% 1. Should register a process by name
% 2. Should unregister a process by name
% 3. Should return pid of a process given a name
% 4. Should return all the registered names

-module(regis_server).
-behaviour(gen_server).
-export([start_link/0,stop/0,register/2,unregister/1,whereis/1,get_names/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
-include_lib("stdlib/include/ms_transform.hrl").


%% interface
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
    gen_server:call(?MODULE,stop).

% Registers a process
register(Name,Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE,{register,Name,Pid}).
    
% Unregisters a process
unregister(Name) ->
    gen_server:call(?MODULE,{unregister,Name}).

% Finds the pid associated with a process
whereis(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Pid, _Ref}] -> Pid;
        [] -> not_found
    end.

% Finds all the names currently registered
get_names() ->
    MatchSpec = ets:fun2ms(fun({Name, _, _}) -> Name end),
    ets:select(?MODULE, MatchSpec).

%% callbacks
init([]) ->
    ?MODULE = ets:new(?MODULE,[set,named_table,protected]),
    {ok,?MODULE}.

handle_call({register,Name,Pid},_From,Tid) ->
    MatchSpec = ets:fun2ms(fun({N,P,_Ref}) when N==Name; P==Pid -> {N,P} end),
    case ets:select(Tid,MatchSpec) of
        [] ->
            Ref = erlang:monitor(process,Pid),
            ets:insert(Tid,{Name,Pid,Ref}),
            {reply,ok,Tid};
        [{Name,_}|_] ->
            {reply,{error,name_exists},Tid};
        [{_,Pid}|_] ->
            {reply,{error,pid_exists},Tid}
    end;

handle_call({unregister,Name},_From,Tid) ->
    case ets:lookup(Tid,Name) of
        [{Name,_Pid,Ref}] ->
            erlang:demonitor(Ref,[flush]),
            ets:delete(Tid,Name),
            {reply,ok,Tid};
        [] ->
            {reply,ok,Tid}
    end;

handle_call(stop, _From, Tid) ->
    ets:delete(Tid),
    {stop, normal, ok, Tid};

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, Tid) ->
    ets:match_delete(Tid, {'_', '_', Ref}),
    {noreply, Tid};

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.