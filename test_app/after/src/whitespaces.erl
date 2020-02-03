-module(whitespaces).

-export([func/1]).

func(_E) ->
    io:format("Hi there, 2*4 is: ~s~n", [2 * 4]).

