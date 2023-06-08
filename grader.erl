-module(grader).
-export([grader/1]).

grader(Mark) when Mark>=90,Mark=<100 -> aplus;
grader(Mark) when Mark>=80,Mark<90 -> a;
grader(Mark) when Mark>=70,Mark<80 -> bplus;
grader(Mark) when Mark>=60,Mark<70 -> b;
grader(Mark) when Mark>=50,Mark<60 -> cplus;
grader(Mark) when Mark>=40,Mark<50 -> c;
grader(Mark) when Mark>=30,Mark<40 -> dplus;
grader(Mark) when Mark>=0,Mark<24 -> youfailed;
grader(_) when true -> markshouldbebetween0and100.

