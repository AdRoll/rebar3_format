-module(break).

-compile(export_all).

break_on_receive() ->
    receive
      {this, will, be, indented} -> but_this:is(short)
      after 1000 -> this:too(should, go, below)
    end.

break_on_try() ->
    try this:short(statement) of
      {things, cant, be} -> {a, <<"one-liner">>}
    after
      this:neither()
    end.

