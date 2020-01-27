-module(weird).   % This is a comment!

-export([ugly/2, nospaces/1, newlines/1, cases/1]).
-export([another/0, one/1, bites/0, the/0, dust/0]).

-spec ugly(any(), Val :: any()) -> Val :: any().
ugly(_, Val) -> Val.

-spec nospaces(any()) -> ok.
nospaces(_) -> it, is, ok.

-spec newlines(Num :: integer()) -> list().
newlines(Num) ->
    List = [{X, Y}
            || X <- lists:seq(1, Num),
               % Weird comment here!
               Y <- [a, b]],
    io:format("~p~n", [List]),
    List.

%% @doc No specs!
cases(V) when is_list(V) ->
    case V of
      [] -> ok;
      _ -> {error, "Uops!"}
    end;
cases(_) -> ok.

another() -> one(fun bites/0), the(), dust().

one(F) -> F().

bites() -> the() orelse dust().

the() -> false.

dust() -> true.

