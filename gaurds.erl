-module(gaurds).
-export([greet/1]).

greet(TimeOfDay) when TimeOfDay=='morning' -> io:format("good morning");
greet(TimeOfDay) when TimeOfDay=='evening' -> io:format("good evening");
greet(TimeOfDay) when TimeOfDay=='night' -> io:format("good night").

