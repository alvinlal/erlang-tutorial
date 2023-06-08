-module(strings).
-export([main/0,concatenate/2]).

main() ->
    X="this is a string",
    X.

concatenate(A,B)->
    A++B.