-module(inline_clause_bodies).

-format #{paper => 60}.

-compile(export_all).

-callback f(inline, this, clause, _) -> [just | like];
           (inline, this, other, much_much_larger_clause) ->
               [please | mister | rebar3 | formatter].

-spec f_cs(inline, this, clause, _) -> [just | like];
          (inline, this, other, much_much_larger_clause) ->
              [please | mister | rebar3 | formatter].
f_cs(inline, this, clause, _) ->
    [just, like];
f_cs(inline, this, other, much_much_larger_clause) ->
    [please, mister, rebar3, formatter].

case_expr_clauses(Inline) ->
    case Inline of
        {this, clause} ->
            [just, like];
        {this, other} ->
            [much,
             much,
             larger,
             clause,
             please,
             mister,
             rebar3,
             formatter]
    end.

if_expr_clauses(Inline) ->
    if Inline == {this, clause} ->
           [just, like];
       true ->
           [this,
            other,
            much,
            much,
            larger,
            clause,
            please,
            mister,
            rebar3,
            formatter]
    end.

fun_expr_clauses() ->
    fun (inline, this, clause) ->
            [just, like];
        (inline, this, other) ->
            [much,
             much,
             larger,
             clause,
             please,
             mister,
             rebar3,
             formatter]
    end.

named_fun_expr_clauses() ->
    fun F(inline, this, clause) ->
            [just, like];
        F(inline, this, other) ->
            F([much,
               much,
               larger,
               clause,
               please,
               mister,
               rebar3,
               formatter])
    end.

receive_expr_clauses() ->
    receive
        {inline, this, clause} ->
            [just, like];
        {this, other} ->
            [much,
             much,
             larger,
             clause,
             please,
             mister,
             rebar3,
             formatter]
    end.

try_expr(Inline) ->
    try Inline() of
        {this, clause} ->
            [just, like];
        {this, other} ->
            [much,
             much,
             larger,
             clause,
             please,
             mister,
             rebar3,
             formatter]
    catch
        _This:clause ->
            [just, like];
        _This:_Other ->
            [much,
             much,
             larger,
             clause,
             please,
             mister,
             rebar3,
             formatter]
    end.
