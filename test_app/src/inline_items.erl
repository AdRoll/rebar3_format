-module(inline_items).

-format(#{inline_items => {when_over, 25},
          paper => 50}).

-compile(export_all).

short() ->
    [these,
     items,
     should,
     'not',
     be,
     inlined,
     since,
     they,
     are,
     less,
     than,
     25].

exact() ->
    [these,
     items,
     should,
     'not',
     be,
     inlined,
     $(,
     i,
     ".",
     e,
     ". ",
     each,
     item,
     should,
     occupy,
     a,
     signle,
     line,
     $),
     since,
     they,
     are,
     exactly,
     25,
     $.].

long() ->
    [these, items, should, be, inlined, they, are,
     more, than, 25, $., 'We', can, be, sure,
     about, that, because, we, added, a, very,
     long, number, 'of', items, to, this, list].

