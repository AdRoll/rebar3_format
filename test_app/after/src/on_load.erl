-module(on_load).

-on_load msg/0.

msg() ->
    io:format("Loaded\n").
