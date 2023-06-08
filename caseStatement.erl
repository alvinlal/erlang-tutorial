-module(caseStatement).
-export([dayNumber/1,fall_velocity/2,is_correct_order/1]).

dayNumber(Day) ->
    case Day of
        'sunday' -> 1;
        'monday' -> 2;
        'tuesday' -> 3;
        'wednesday' -> 4;
        'thursday' -> 5;
        'friday' -> 6;
        'saturday' -> 7;
		 _ -> -1
    end.

fall_velocity(Planemo,Distance) ->
    Gravity = case Planemo of
        earth when Distance >= 0 -> 9.8;
        moon when Distance >= 0 -> 1.6;
        mars when Distance >= 0 -> 3.71
    end,
    math:sqrt(2*Gravity*Distance).

is_correct_order(GameTuple) ->
    case GameTuple of
        {stone,paper,scissor} -> true;
        _ -> false
    end.

