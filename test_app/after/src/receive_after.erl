-module(receive_after).

-export([sleep/1]).

%% This function should not generate a line with 4 spaces between receive and after
sleep(T) ->
    receive
      after T ->
                io:format("Waking up after ~p milliseconds to keep on working as usual!!~n", [T])
    end.
