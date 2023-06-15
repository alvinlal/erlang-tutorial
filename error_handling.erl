-module(error_handling).
-export([fall_velocity/2]).


fall_velocity(Planemo,Distance) ->
    try
        Gravity = case Planemo of
            earth -> 9.8;
            moon -> 1.6;
            mars -> 3.71;
            _ -> throw({error,"invalid planemo!"})
        end,
        math:sqrt(Gravity * Distance * 2)
    catch
        error:Error -> {error,Error};
        throw:Error -> {error,Error}
    after
        io:format("running after block~n")
    end.