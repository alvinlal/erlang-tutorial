-module(input).
-export([getInfo/0,chooseMenu/0,enterSentence/0]).


getInfo() ->
    Info = io:read("What is your name and age , {name,age} ?"),
    io:format("Your name is ~w, and your age is ~w.~n",[element(1,element(2,Info)),element(2,element(2,Info))]).

chooseMenu()->
    io:format("Choose an option~n"),
    io:format("1. Next~n"),
    io:format("2. Back~n"),
    io:format("3. exit~n"),
    io:get_chars("which? > ",1).

enterSentence()->
    Sentence = io:get_line("enter a sentence:-"),
    io:format("you entered ~s",[Sentence]).