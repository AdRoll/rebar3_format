-module(newline_after_attributes).

-export([a_fun/0]).
-export([another_fun/0]).

-format(#{newline_after_attributes => false}).

-dialyzer([{nowarn_function, {a_fun, 0}}]).

-attribute(x).
-attribute(y).

-type t() :: t(term()).
-type t(X) :: {X}.

-attribute(z).

-spec a_fun() -> t(boolean()).
a_fun() -> {true}.

-ifdef(MORE).

-attribute(w).
-attribute(w2).

-endif.

-spec another_fun() -> t(number()).
another_fun() -> {1}.

-ifdef(LAST).

-attribute(last).

-endif.

