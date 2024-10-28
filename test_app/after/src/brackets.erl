-module(brackets).

-format #{inline_items => all}.
-format #{paper => 50}.

-compile(export_all).

many_arguments(One, Two, Three, Four, Five, Six,
               Seven) ->
    {in, a, tuple, One, Two, Three, Four, Five,
     Six, Seven}.

one_big_argument(A_Really_Really_Very_Long_Argument_Name) ->
    [A_Really_Really_Very_Long_Argument_Name, in,
     a, list].

short_list() ->
    [this, is, a, short, list].

long_list() ->
    [this, list, is, quite, long, therefore, it,
     needs, indentation].

short_tuple() ->
    {this, is, a, short, list}.

long_tuple() ->
    {this, tuple, is, quite, long, therefore, it,
     needs, indentation}.

nested() ->
    First =
        {a, long, long, tuple, with,
         [a, really, very, long, long, list, in,
          it],
         plus, other, things},
    Then =
        [a, long, long, list, with,
         {a, really, very, long, long, tuple, in,
          it},
         plus, other, things],
    finally:a_long_function_call(with, many,
                                 arguments, like,
                                 First, Then,
                                 and_more).

with_comments() ->
    [lists, %% with
     comments, %% should not loose them
     even, %% if they are really very long and go beyond the paper limit
     {also, %% it should not
      matter, %% if the comments are
      nested, %% in tuples within lists within tuples or
      [what, 'not']}].

lc() ->
    [{list, comprehensions, should, work, Nicely,
      too}
     || Nicely
            <- even:for(those, who, come, from,
                        functions, with, many,
                        arguments),
        or_have:filters(Nicely)].

map() ->
    #{maps => should,
      also => behave,
      nicely => in,
      regards => [to, indentation]}.
