-module(indent_8).

-format #{break_indent => 8, paper => 50}.
-format #{inline_clause_bodies => true}.

-record(record,
        {fields =
                 should:be(
                         indented_using:break_indent(8)),
         including ::
                 those:that_use_types(with_very_long_names),
         what_about =
                 fields_with:very_long_values(
                         and_very:long_type_names())
                 ::
                 they:also(should:be(indented_using:break_indent(8)))}).

infix_expr() ->
        this:infix(expression) ++
                should:be(indented) ++
                        using:break_indent(8).

prefix_expr() ->
        ThisPrefixExpressionShould =
                not use:break_indent(8).

match_expr() ->
        ThisVeryVeryVeryLongMatchExpression =
                should:be(
                        indented_using:break_indent(8)).

case_expr() ->
        case
                expressions:that(are_too_long_for_a_line,
                                 should:be(
                                         indented_using:break_indent(8)),
                                 but,
                                 the,
                                 "of",
                                 should:be(
                                         indented_using:break_indent(8)))
                of
                clauses ->
                        should:be(
                                indented_using:break_indent(8))
        end.

if_expr() ->
        if
                {expressions_that_are_too_long_for_a_line,
                 [should,
                  be,
                  indented_using,
                  {break_indent, 8}]} ->
                        ok
        end.

-type
         a_type_with_a_very_long_name() :: this_type:definition_doesnt_fit_in(a:line()).

-spec a_function_with_a_very_long_name() ->
                                              a_type_with_a_very_long_name().
a_function_with_a_very_long_name() ->
        {specs,
         " and ",
         types,
         should:be(
                 indented_using:break_indent(8))}.

block_expr() ->
        begin
                block:expressions(),
                should:be(
                        indented_using:break_indent(8))
        end.

catch_expr() ->
        catch exp:ressions(
                      should:be(
                              indented_using:break_indent(8))).

list_generator() ->
        [generators
         || _In
                    <- list:comprehensions(
                               should:be(
                                       indented_using:break_indent(8))),
            _Which
                    <- is:something(
                               that:looks(
                                       quite:awful({in,
                                                    my,
                                                    opinion})))].

binary_generator() ->
        << <<"generators">>
           || <<_In>>
                      <= list:comprehensions(
                                 should:be(
                                         indented_using:break_indent(8))),
              <<_Which>>
                      <= is:something(
                                 that:looks(
                                         quite:awful({in,
                                                      my,
                                                      opinion}))) >>.

receive_after(ExpressionsThatAreReallyTooLongForALine) ->
        receive
                clauses ->
                        should:be(indented_using:break_indent(8))
                after ExpressionsThatAreReallyTooLongForALine ->
                        should:be(indented_using:break_indent(8))
        end.

try_expr() ->
        try
                expressions:that(are_too_long_for_a_line,
                                 should:be(
                                         indented_using:break_indent(8)),
                                 but,
                                 the)
        of
                should -> not be:indented(at_all)
        catch
                Clauses:Should:Also ->
                        be:indented(
                                using:break_indent(8))
        after
                should:be(
                        indented_using:break_indent(8))
        end.

when_clause(A, B, C)
        when is_integer(A),
             B >= A andalso C rem 2 =:= 0 ->
        the:guard(
                should:be(
                        indented_using:break_indent(8)));
when_clause(Along, Blonb, Clong)
        when is_integer(Along),
             Along >= Blong andalso
                     Clong rem Along =:= Blong ->
        here:should(happen, the, same, thing).
