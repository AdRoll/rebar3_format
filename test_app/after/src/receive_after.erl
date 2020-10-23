-module(receive_after).

-export([sleep/1, other_options/0]).

%% This function should not generate a line with 4 spaces between receive and after
sleep(T) ->
    receive after T ->
        io:format("Waking up after ~p milliseconds to keep on working as usual!!~n", [T])
    end.

other_options() ->
    receive after 1000 ->
        ho
    end,
    receive after a:function(with,
                             a,
                             "very",
                             <<"very">>,
                             long,
                             list,
                             "of",
                             params,
                             that,
                             should,
                             exceed,
                             "paper and ribbon") ->
        this
    end,
    receive after begin
                      a:long(list),
                      of_function:calls()
                  end ->
        another:long(list),
        of_function:calls()
    end.
