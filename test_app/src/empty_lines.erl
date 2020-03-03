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

    between:them(),

    and_remove(at_the_end_of_the_file).












