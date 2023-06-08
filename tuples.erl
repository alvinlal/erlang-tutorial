-module(tuples).
-export([main/0,fall_velocity/1]).

main()->
    Tuple1={hello,i,am,a,tuple,2,3.2},
    tuple_size(Tuple1).

fall_velocity({earth,Distance})->math:sqrt(2*9.8*Distance);
fall_velocity({moon,Distance})->math:sqrt(2*1.6*Distance);
fall_velocity({mars,Distance})->math:sqrt(2*3.71*Distance).
