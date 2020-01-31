-module(others).

-compile(export_all).

strange_infix_operators(A, B) -> bnot (-(A rem B)).

-define(OUT_OF_CONTEXT_CLAUSE, true -> false ).

-record(rec, {key, value}).

named_fun_expr() ->
    fun This(is) -> a;
        This(recursive) -> function;
        This(called) -> This(is)
    end.

catch_expr() ->
    catch this:train(with, all, its, arguments, {they, might, be, too, many, to, "handle"}).

comprehensions(Bin, List) ->
    BinToBin = << <<X:1>>
                   || <<X:8/integer>> <= Bin, X > 0, with:a_very(complex, boolean_filter, on, X) >>,
    BinToList = [X
                 || <<X:8/integer>> <= Bin, X > 0, with:a_very(complex, boolean_filter, on, X)],
    ListToBin = << <<X:1>>
                    || X <- List, X > 0, with:a_very(complex, boolean_filter, on, X) >>,
    ListToList = [X || X <- List, X > 0, with:a_very(complex, boolean_filter, on, X)],
    {BinToBin, BinToList, ListToBin, ListToList}.

parentheses() ->
    {"does                            ",
     this,
     [[code, <<"        look         ">>, like],
      "lisp",
      to,
      [{[<<"(      you or me or them?)">>]}]]}.

receive_expr() -> receive with -> {no, timeout} end.

record_index_expr(List) -> lists:keyfind(a, #rec.key, List).

try_expr_after() ->
    try to:open({the, door}) of
      my -> room or your;
      room -> {my, friend}
    catch
      {you_cant, Open} when Open -> it:was(Open)
    after
      close:the(door, anyway)
    end,
    try with:no(catching) after
      do:something({to,
                    "clean up",
                    <<"the">>,
                    [filthy, filthy, mess],
                    you,
                    created,
                    "if you can"})
    end.

bit_types(X) ->
    <<X:4/little-signed-integer-unit:8, (something:on(X)):32/big-unsigned-integer-unit:32>>.

multi_try_expr() ->
    try
        there:are(2),
        expressions:in(this_block)
    catch
        A:Catch:Expression -> formatter:should(indent,A,Catch,Expression)
    end.
