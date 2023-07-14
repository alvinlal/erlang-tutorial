-module(stop_test).
-behaviour(gen_server).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,stop_call/1,stop_cast/1]).


%% Client API
stop_call(Pid) -> gen_server:call(Pid,{stop}).

stop_cast(Pid) -> gen_server:cast(Pid,{stop}). 


%% Gen server callbacks
start_link() -> gen_server:start_link(?MODULE,[],[]).

init([]) -> {ok,[]}.

handle_call({stop},_From,State) -> {stop,just_testing_error,State}.

handle_cast({stop},State) -> {stop,just_testing_error,State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(Reason,State) ->
    io:format("Gen server terminated due to ~p~n",[Reason]),
    {ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



