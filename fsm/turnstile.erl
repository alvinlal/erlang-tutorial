-module(turnstile).
-author("Alvin lal").
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

% Tutorial from https://kasunedward.medium.com/gen-fsm-the-way-to-implement-finite-state-machines-in-erlang-7544c6b9655e

%% Public API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,coin/0,push/0,locked/2,unlocked/2]).

%% Public API
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> {ok,locked,[]}.

coin() ->
    gen_fsm:send_event(?MODULE,coin).

push() ->
    gen_fsm:send_event(?MODULE,push).

%% gen_fsm callbacks

locked(Event,State) ->
    case Event of 
        coin -> 
            io:format("turnstile opening...~n"),
            {next_state,unlocked,State};
        push ->
            io:format("please put coin and push! ~n"),
            {next_state,locked,State}
    end.

unlocked(Event,State) ->
    case Event of
        coin ->
            io:format("turnstile is already unlocked ~n"),
            {next_state,unlocked,State};
        push ->
            io:format("closing turnstile...~n"),
            {next_state,locked,State}
    end.

