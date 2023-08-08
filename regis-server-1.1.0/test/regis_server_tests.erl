-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

% Requirements
% 1. The server can be started, stopped and has a registered name
% 2. A process can be registered by a name and be contacted
% 3. A process shouldn't register if the name already exists
% 4. A process shouldn't be registerd if the pid already exists
% 5. A list of processes currently registered can be obtained
% 6. An unregistered name should return 'not_found' on process lookup
% 7. A process can be unregistered and should no longer appear on lookup
% 8. Unregistering a process with a name that does not exists should not cause an error
% 9. A process that was registered can be registered again if it was unregistered between both calls
% 10. A crashed process should be unregistered


% Test Descriptions
start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
    ?setup(fun is_registered/1)}.

register_test_() ->
    [{"A process can be registered by a name and be contacted using the name",
    ?setup(fun register_and_contact/1)},
    {"A process shouldn't register if the name already exists",
    ?setup(fun register_name_exists/1)},
     {"A process shouldn't be registerd if the pid already exists",
    ?setup(fun register_pid_exists/1)},
    {"A list of processes currently registered can be obtained",
    ?setup(fun register_list/1)},
    {"An unregistered name should return 'not_found' on process lookup",
    ?setup(fun registered_notfound/1)}
    ].

unregister_test_() ->
    [{"A process can be unregistered and should no longer appear on lookup",
    ?setup(fun unregister_lookup/1)},
    {"Unregistering a process with a name that does not exists should not cause an error",
    ?setup(fun unregister_name_exists/1)},
    {"A process that was registered can be registered again if it was unregistered between both calls",
    ?setup(fun unregister_between_call/1)},
    {"A crashed process should be unregistered",
    ?setup(fun unregister_crash/1)}  
    ].

% Setup Functions
start() ->
    {ok,Pid} = regis_server:start_link(),
    Pid.

% Cleanup Functions
stop(_) ->
    regis_server:stop().

% Actual tests
% register tests
is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid,whereis(regis_server))
    ].

register_and_contact(_) ->
    Name = "test_process",
    TestPid = spawn_link(fun() -> testProcess(Name) end),
    timer:sleep(15),
    Ref = make_ref(),
    WherePid = regis_server:whereis(Name),
    regis_server:whereis(Name) ! {self(),Ref,hi},
    Rec = receive
          {Ref,hi} -> true
          after 2000 -> false
    end,
    [?_assertEqual(TestPid,WherePid),
     ?_assert(Rec)].

register_name_exists(_) ->
    Name = "test_process",
    spawn(fun() -> testProcess(Name) end),
    timer:sleep(25),
    Res = regis_server:register(Name,self()),
    [?_assertEqual({error,name_exists},Res)].

register_pid_exists(_) ->
   Name = "test_process",
   ok = regis_server:register(Name,self()), 
   Name2 = "test_process_2",
   Res = regis_server:register(Name2,self()),
   [?_assertEqual(Res,{error,pid_exists})].

register_list(_) ->
    L1 = regis_server:get_names(),
    Pids = [spawn(fun() -> testProcess(N) end) || N <- ["a","b","c","d","e"]],
    timer:sleep(200),
    L2 = regis_server:get_names(),
    [exit(Pid,kill) || Pid <- Pids],
    [?_assertEqual([],L1),
    ?_assertEqual(["a","b","c","d","e"],lists:sort(L2)) 
    ].


registered_notfound(_) ->
    Name = "unregistered_process",
    Res = regis_server:whereis(Name),
    [?_assertEqual(not_found,Res)].

% unregister tests
unregister_lookup(_) -> 
   Name = "test_process",
   ok = regis_server:register(Name,self()),
   ok = regis_server:unregister(Name),
   Res = regis_server:whereis(Name),
   [?_assertEqual(not_found,Res)].

unregister_name_exists(_) ->
   Name = "test_process",
   Res = regis_server:unregister(Name),
   [?_assertEqual(Res,ok)].

unregister_between_call(_) ->
   Name = "test_process",
   ok = regis_server:register(Name,self()),
   ok = regis_server:unregister(Name),
   Res = regis_server:register(Name,self()),
   [?_assertEqual(Res,ok)].

unregister_crash(_) ->
    Name = "test_process",
    spawn(fun() -> testProcess(Name) end) ,
    timer:sleep(150),
    Pid = regis_server:whereis(Name),
    exit(Pid,kill),
    [?_assertEqual(regis_server:whereis(Name),not_found)].

% Helper functions
testProcess(Name) ->
    ok = regis_server:register(Name,self()),
    receive
        {From,Ref,Msg} -> From ! {Ref,Msg}
    end.