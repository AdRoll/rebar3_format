-module(break).

-compile(export_all).

%% receive...after...end is never a one-liner
break_on_receive() ->
    receive
        x ->
            y
    after 1000 ->
        z
    end.

%% try...of...after...end is never a one-liner
break_on_try() ->
    try w of
        x ->
            y
    after
        z
    end.
