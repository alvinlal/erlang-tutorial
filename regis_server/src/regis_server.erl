% Requirements
% 1. Should register a process by name
% 2. Should unregister a process by name
% 3. Should return pid of a process given a name
% 4. Should return all the registered names

-module(regis_server).
-behaviour(gen_server).
-export([start_link/0,stop/0,register/2,unregister/1,whereis/1,get_names/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

-record(state, {pid, name}).

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
    gen_server:call(?MODULE,{whereis,Name}).

% Finds all the names currently registered
get_names() ->
    gen_server:call(?MODULE,get_names).

%% callbacks
init([]) ->
    {ok,#state{pid = gb_trees:empty(),name=gb_trees:empty()}}.

handle_call({register,Name,Pid},_From,S = #state{pid = P, name = N}) ->
    case {gb_trees:is_defined(Pid,P),gb_trees:is_defined(Name,N),is_list(Name)} of
        {true,_,_} ->
            {reply,{error,pid_exists},S};
        {_,true,_} ->
            {reply,{error,name_exists},S};
        {_,_,false} ->
            {reply,{error,bad_type},S};
        {false,false,true} ->
            Ref = erlang:monitor(process,Pid),
            {reply,ok,S#state{pid = gb_trees:insert(Pid,{Name,Ref},P),
                              name = gb_trees:insert(Name,{Pid,Ref},N)}}
    end;

handle_call({unregister,Name},_From,S = #state{pid = P, name = N}) ->
    case gb_trees:lookup(Name,N) of
        {value,{Pid,Ref}} ->
            erlang:demonitor(Ref,[flush]),
            {reply,ok,S#state{pid = gb_trees:delete(Pid,P),
                              name = gb_trees:delete(Name,N)}};
        none ->
            {reply,ok,S}
    end;

handle_call({whereis,Name},_From,S = #state{name=N}) ->
    case gb_trees:lookup(Name,N) of
        {value,{Pid,_}} ->
            {reply,Pid,S};
        none ->
            {reply,not_found,S}
    end;

handle_call(get_names, _From, S = #state{name=N}) ->
    {reply, gb_trees:keys(N), S};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, S = #state{pid=P,name=N}) ->
    {value, {Name, Ref}} = gb_trees:lookup(Pid, P),
    {noreply, S#state{pid = gb_trees:delete(Pid, P),
                 name = gb_trees:delete(Name, N)}};

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.