% process responsible for handling the tcp connection
-module(chat_process).
-behaviour(gen_server).

-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

% user_id - unique id of the user connected
% socket - socket descriptor of the current tcp connection
-record(state,{user_id,socket}).


start_link(Socket) ->
    gen_server:start_link(?MODULE,Socket,[]).

init(Socket) ->
    % Because accepting a connection is a blocking function call,
    % we can not do it in here. Forward to the server loop!
    gen_server:cast(self(),accept),
    {ok,#state{socket = Socket}}.


% Callbacks

% won't be used
handle_call(_E,_From,State) ->
    {noreply,State}.

handle_cast(accept,S = #state{socket = ListenSocket}) ->
    {ok,AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Accepted new connection~n",[]),
    chat_sup:spawn_chat_process(), % a new acceptor is created, so that we always have a standby connection handler
    inet:setopts(AcceptSocket, [{active, once}]), % for receiving messages in active mode (as process messages) 
    % one by one in the mailbox
    {noreply,S#state{socket=AcceptSocket}}.

% all messages send by the client will handled by handle_info
handle_info({tcp,_Socket,Str},S = #state{socket = Socket}) ->
    io:format("Got new message from ~p, :- ~s~n",[_Socket,Str]),
    % convert str to json and switch on event type
    inet:setopts(Socket, [{active, once}]),
    {noreply,S};

% called when tcp connection is closed
handle_info({tcp_closed,_Socket},S) ->
    io:format("tcp connection ~p have been closed~n",[_Socket]),
    {stop,normal,S};

% called on tcp connection error
handle_info({tcp_error,_Socket,_},S) ->
    io:format("tcp connection ~p errored out!~n",[_Socket]),
    {stop,normal,S};

% called on unrecognized messages
handle_info(E,S) ->
    io:format("unexpected: ~p~n",[E]),
    {noreply,S}.

% code change
code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

% terminations
terminate(normal, #state{socket = Socket} = S) ->
    io:format("tcp connection ~p terminated normally~n",[Socket]),
    ok;
terminate(_Reason, #state{socket = Socket} = S) ->
    io:format("tcp connection ~p terminated for reason ~p~n", [Socket,_Reason]).