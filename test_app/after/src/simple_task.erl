-module(simple_task).

-vsn(1.00000000000000000000e+00).

-author("test author").

-export([stop/1, start/1]).

-behaviour(task).

stop(_Name) -> ok.

start(Spec) -> task:name(Spec).

