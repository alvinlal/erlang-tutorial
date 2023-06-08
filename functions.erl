-module(functions).
-export([add/2,tripler/2]).

add(A,B) -> % this is a function in erlang
    A+B.


tripler(Value, Function) -> 3 * Function(Value).

