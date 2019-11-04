-module(comments).

-export([fact/1]).

-some(thing).

-export([dummy_fn/1, heredoc/0]).

fact(N) when N > 0 -> N * fact(N - 1);
fact(0) -> 1.

dummy_fn(A) -> Fn = fun (B) -> B end, Fn(A).

heredoc() -> X = "\nThis is\na multiline\nheredoc\n", {ok, X}.

