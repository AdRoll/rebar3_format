-module(whitespaces).

-export([func/2]).

func(E) -> io:format("Hi there, 2*4 is: ~s~n", [2 * 4]).

