% Application wrapper module for regis,
% A process registration application.

-module(regis).
-behaviour(application).
-export([start/2, stop/1]).
-export([register/2, unregister/1, whereis/1, get_names/0]).


start(normal, []) ->
    regis_sup:start_link().

stop(_) ->
    ok.

register(Name, Pid) -> regis_server:register(Name, Pid).

unregister(Name) -> regis_server:unregister(Name).

whereis(Name) -> regis_server:whereis(Name).

get_names() -> regis_server:get_names().
