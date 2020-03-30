-module(empty_lines).

-compile(export_all).

in_this() -> function, there:are(no, empty, lines).

this_function() ->

    has:two(),

    empty:lines(), first:one(wont, be, preserved).

here() ->
    we:have(many),




    empty:lines(),


    but:only(two, should),
    be:preserved().

empty(Lines) ->
    should:nt(
        be,

        preserved, if_they:appear(
            within,

            a:single(expression))),

    but:we_preserve(Lines),

    between:them().

a_fun(With, Some, Arguments) ->
    an:expression(that,
                  occupies,
                  Some,
                  #{lines => since, it => uses, a => [long, list], 'of' => Arguments}),


    %% ^ a couple of empty lines below the aforementioned long expression
    another:expression(With, smaller, size),


    %% Comment lines should count as lines for this sort of thing
    %% So the previous empty lines should be shrinked to 1

    %% But comments themselves are stacked, so if you want to keep an empty line
    %% You should still comment it out, like this:
    %%
    %% ^ an empty line that is preserved
    an:expression(that,

                  has,
                  Some,

                  {empty, lines, inside},
                  #{and_it => uses, a => [long, list], 'of' => Arguments}),
    no_empty_line_above_me.