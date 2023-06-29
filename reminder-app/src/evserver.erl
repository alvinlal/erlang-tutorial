-module(evserver).
-compile(export_all).
-record(state,{events,clients}).
-record(event,{name="",description="",pid,timeout={{1970,1,1},{0,0,0}}}).

init()->
    loop(#state{events=maps:new(),clients=maps:new()}).

valid_datetime({Date,Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
        false
    end;

valid_datetime(_) -> false.
 
valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                      M >= 0, M < 60,
                     S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

send_to_clients(Msg, ClientDict) ->
     maps:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

loop(S = #state{}) -> 
    receive 
        % subscribe a client to the server
        {Pid,MsgRef,{subscribe,Client}} ->
            Ref = erlang:monitor(process,Client),
            UpdatedClients = maps:put(Ref,Client,S#state.clients),
            Pid ! {MsgRef,ok},
            loop(S#state{clients=UpdatedClients});

        % add a new event 
        {Pid,MsgRef,{add,Name,Description,TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name,TimeOut),
                    UpdatedEvents = maps:put(Name,#event{name=Name,description=Description,pid=EventPid,timeout=TimeOut},S#state.events),
                    Pid ! {MsgRef,ok},
                    loop(S#state{events=UpdatedEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;

        % cancel an event
        {Pid,MsgRef,{cancel,Name}} ->
            Events = case maps:find(Name,S#state.events) of
                    {ok,E} ->
                        event:cancel(E#event.pid),
                        maps:remove(Name,S#state.events);
                    error ->
                        S#state.events
                    end,
            Pid ! {MsgRef,ok},
            loop(S#state{events=Events});

        % event timeout
        {done, Name} ->
            case maps:find(Name,S#state.events) of
                {ok,E} ->
                    send_to_clients({done,E#event.name,E#event.description},S#state.clients),
                    UpdatedEvents = maps:remove(Name,S#state.events),
                    loop(S#state{events=UpdatedEvents});
                error ->
                    loop(S)
            end;

        % shutdown server
        shutdown ->
            exit(shutdown);
        
        % client dead
        {'DOWN', Ref, process, _Pid, _Reason} ->
           loop(S#state{clients=maps:remove(Ref, S#state.clients)});

        % code change
        code_change ->
        ?MODULE:loop(S);

        % Unknown message
        Unknown ->
          io:format("Unknown message: ~p~n",[Unknown]),
          loop(S)
    end.

start() ->
    register(?MODULE, Pid=spawn(?MODULE,init,[])),
    Pid.

start_link() ->
    register(?MODULE,Pid=spawn_link(?MODULE,init,[])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after Delay*1000 ->
        []
    end.