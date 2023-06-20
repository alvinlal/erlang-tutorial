-module(records).
-include("records.hrl").
-export([main/0]).

main() ->
    Tower1 = #tower{location="Grand Canyon"},
    Tower1.