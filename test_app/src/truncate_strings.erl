-module(truncate_strings).

-format #{truncate_strings => true}.

-compile([export_all]).

simple() ->
    "simple".

multi() ->
    "mul" "ti".

rows() ->
    "row1"
    "row2".

newline() ->
    "new
     line".

newlines() ->
    "new
     line1 with\ninside"
    "new
     line2".

char() ->
    "\x61a".

chars() ->
    "\x61a" "b".

long() ->
    "a very very very long line that goes over the paper limit the quick brown fox jumps over the lazy dog, you see".

too_long() ->
    "a very very very long line that goes over the paper limit the quick brown fox jumps over the lazy dog, you see"
    "a very very very long line that goes over the paper limit the quick brown fox jumps over the lazy dog, you see".

with_tabs() ->
    "a\tvery\tvery\tvery\tlong\tline\tthat\tgoes\tover\tthe\tpaper\tlimit\tthe\tquick\tbrown\tfox\tjumps\tover\tthe\tlazy\tdog,\tyou\tsee".

with_special_chars() ->
    "a\^Rvery\^Rvery\^Rvery\^Rlong\^Rline\^Rthat\^Rgoes\^Rover\^Rthe\^Rpaper\^R\x{1}over\x{59}the\x{59}\x60quick\x6brown\x60fox\x60jumps\x60over\x60the\x60lazy\x60dog,\x60you\x60see"
    "something_something_something\123something_something_something\12something_something_something\123something_something_something\32something_something_something".

with_newlines() ->
    "a\nvery\nvery\nvery\nlong\nline\nthat\ngoes\nover























    the\npaper\nlimit\nthe\nquick\nbrown\nfox\njumps
    over\nthe\nlazy\ndog,\nyou\nsee".

with_escaped_chars() ->
    "a \\tvery \\tvery \\tvery \\tlong \\tline \\tthat \\tgoes \\tover \\tthe \\tpaper \\tlimit \\tthe \\tquick \\tbrown \\tfox \\tjumps \\tover \\tthe \\tlazy \\tdog, \\tyou \\tsee".

bad_indent() ->
"misguided"
                            "indentation".

bad_indent_2() ->
"misguided indentation 2".
