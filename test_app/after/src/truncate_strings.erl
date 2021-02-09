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
    "a very very very long line that goes over the paper limit the "
    "quick brown fox jumps over the lazy dog, you see".

too_long() ->
    "a very very very long line that goes over the paper limit the "
    "quick brown fox jumps over the lazy dog, you see"
    "a very very very long line that goes over the paper limit the "
    "quick brown fox jumps over the lazy dog, you see".

bad_indent() ->
    "misguided"
    "indentation".

bad_indent_2() ->
    "misguided indentation 2".
