-module(debugging).
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) ->
Gravity = case Planemo of
   earth -> 9.8;
   moon -> 1.6;
   mars -> 3.71
 end,

try math:sqrt(2 * Gravity * Distance) of
   Result -> Result
catch
   error:Error -> {error, Error}
end.