-module(listLength).
-export([listLength/1,listLengthTail/1]).

listLength([]) -> 0;
listLength([_|T]) -> 1 + listLength(T).

listLengthTail(L) -> listLengthTail(L,0).

listLengthTail([],Acc)->Acc;
listLengthTail([_|T],Acc)->listLengthTail(T,Acc+1).