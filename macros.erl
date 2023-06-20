-module(macros).
-export([greet/0]).
-define(NAME,"alvin").

greet() ->
    io:format(?NAME).
