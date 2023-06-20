-module(calc).
-export([start/0]).


start() ->
    {ok,Expresssion} = io:read("Enter reverse polish notation (enclosed in double quotes):- "),
    rpn(Expresssion).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2,[],string:tokens(L," ")),
    Res.

rpn("+",[M,N|S]) -> [M+N|S];
rpn("-",[M,N|S]) -> [M-N|S];
rpn("*",[M,N|S]) -> [M*N|S];
rpn("/",[M,N|S]) -> [M/N|S];
rpn("^",[M,N|S]) -> [math:pow(M,N)|S];
rpn("ln",[N|S]) -> [math:log(N)|S];
rpn("log10",[N|S]) -> [math:log10(N)|S];
rpn(X,Stack) -> [read(X)|Stack].

read(N)->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_} -> F
    end.

