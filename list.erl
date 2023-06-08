-module(list).
-export([getAsList/3,combineList/2]).

getAsList(A,B,C) -> [A,B,C].

combineList(A,B) -> A++B.