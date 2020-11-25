#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

main([]) ->
    main(["none"]);
main([String]) ->
    io:format("Args: ~s~nNode: ~p~n", [String, node()]).
