-module(factorial).
-export([fact/1]).

fact(Num) when Num > 0 -> Num * fact(Num-1);
fact(Num) when Num == 0 -> 1.