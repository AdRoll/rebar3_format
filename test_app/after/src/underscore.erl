-module(underscore).

-export([x/0, x/1]).

x(Be) ->
    _ = this:code(should,
                  not Be,
                  indented,
                  below,
                  the,
                  underscore,
                  even_when,
                  it,
                  goes,
                  over,
                  paper).

x() ->
    SinceThisVariableIsLong =
        this:code(should, be, indented, below, the, underscore, since, it, goes, over, paper).
