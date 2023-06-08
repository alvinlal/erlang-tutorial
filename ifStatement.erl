-module(ifStatement).
-export([dayNumber/1]).

dayNumber(Day) ->
    if 
        Day==sunday -> 1;
        Day==monday -> 2;
        Day==tuesday -> 3;
        Day==wednesday -> 4;
        Day==thursday -> 5;
        Day==friday -> 6;
        Day==saturday -> 7
    end.
