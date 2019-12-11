-module(inline_expressions).

-compile(export_all).

-format([{inline_expressions, false}]).

these() ->
    Expressions = should:occupy(),
    a,
    line,
    each;
these() ->
    other,
    expressions,
    too.

also() ->
    these,
    two.

even() ->
    when_they:are(small),
    enough:to(fit).

white() ->
    lines:should(not be:preserved()),
    Since = preserve_empty_lines:is(false).

