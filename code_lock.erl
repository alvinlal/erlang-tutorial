-module(code_lock).
-behaviour(gen_statem).
-define(NAME,?MODULE).
-export([start_link/1,button/1,callback_mode/0,init/1,locked/3,open/3,terminate/3]).

%% Client API
start_link(Code) ->
    gen_statem:start_link({local,?NAME},?MODULE,Code,[]).

button(Button) ->
    gen_statem:cast(?NAME, {button,Button}).



%% Callbacks
callback_mode() ->
    state_functions.

init(Code) ->
    do_lock(),
    Data = #{code=>Code,length=>length(Code),buttons=>[]},
    {ok,locked,Data}.

locked(cast,{button,Button}, #{code := Code, length := Length, buttons := Buttons} = Data) ->
    %% 1. if length(Buttons) is less than length , it means that code is still being entered, so append the Button to the list of Buttons
    %% 2. if length(Buttons) is greater than length, remove oldest button from the list of buttons using tl(Buttons)
    NewButtons = if length(Buttons) < Length ->  Buttons;
                 true -> tl(Buttons)
                end ++ [Button],
    %% 3. if the Code and NewButtons lists are equal, call do_unlock do a state transition to open with a time_out to lock state after 10 seconds
    if Code =:= NewButtons ->
            do_unlock(),
            {next_state,open,Data#{buttons:=[]},[{state_timeout,5000,lock}]};
        true ->
            {next_state, locked, Data#{buttons := NewButtons}}
    end.

open(state_timeout, lock,  Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button,_}, Data) ->
    {next_state, open, Data}.

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.

    

%% lock and open
do_lock()->
    io:format("door is now locked~n").

do_unlock()->
    io:format("door is now open~n").