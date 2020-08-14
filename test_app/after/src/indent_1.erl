-module(indent_1).

-format #{break_indent => 1,
          inline_clause_bodies => true,
          paper => 50}.

-record(record,
        {fields =
          should:be(indented_using:break_indent(1)),
         including ::
          those:that_use_types(with_very_long_names),
         what_about =
          fields_with:very_long_values(and_very:long_type_names())
          ::
          they:also(should:be(indented_using:break_indent(1)))}).

infix_expr() ->
 this:infix(expression) ++
  should:be(indented) ++ using:break_indent(1).

prefix_expr() ->
 ThisPrefixExpressionShould =
  not use:break_indent(1).

match_expr() ->
 ThisVeryVeryVeryLongMatchExpression =
  should:be(indented_using:break_indent(1)).

case_expr() ->
 case expressions:that(are_too_long_for_a_line,
                       should:be(indented_using:break_indent(1)),
                       but,
                       the,
                       "of",
                       should:be(indented_using:break_indent(1)))
  of
  clauses ->
   should:be(indented_using:break_indent(1))
 end.

if_expr() ->
 if {expressions_that_are_too_long_for_a_line,
     [should,
      be,
      indented_using,
      {break_indent, 1}]} ->
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
  should:be(indented_using:break_indent(1))}.

block_expr() ->
 begin
  block:expressions(),
  should:be(indented_using:break_indent(1))
 end.

catch_expr() ->
 catch
  exp:ressions(should:be(indented_using:break_indent(1))).

list_generator() ->
 [generators
  || _In
      <- list:comprehensions(should:be(indented_using:break_indent(1))),
     _Which
      <- is:something(that:looks(quite:awful({in,
                                              my,
                                              opinion})))].

binary_generator() ->
 << <<"generators">>
    || <<_In>>
        <= list:comprehensions(should:be(indented_using:break_indent(1))),
       <<_Which>>
        <= is:something(that:looks(quite:awful({in,
                                                my,
                                                opinion}))) >>.

receive_after(ExpressionsThatAreReallyTooLongForALine) ->
 receive
  clauses ->
   should:be(indented_using:break_indent(1))
  after ExpressionsThatAreReallyTooLongForALine ->
         should:be(indented_using:break_indent(1))
 end.

try_expr() ->
 try expressions:that(are_too_long_for_a_line,
                      should:be(indented_using:break_indent(1)),
                      but,
                      the)
 of
  should -> not be:indented(at_all)
 catch
  Clauses:Should:Also ->
   be:indented(using:break_indent(1))
 after
  should:be(indented_using:break_indent(1))
 end.
