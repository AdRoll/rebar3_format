-module(strings).

-export([all/0]).

-format #{inline_expressions => true}.

-attr({with, "a string"}).

all() ->
    heredoc(), superlong(), repeat(), multiple_calls(), characters(), multiline_with_spaces().

superlong() ->
    "This is a super super super super super super super super super super super super super super super super super super super super super super super super super super super super super long string!"
    "Shouldn't be truncated since truncate_strings => false by default".

heredoc() ->
    {ok,
     "" "
This is
a multiline
heredoc but there are
no multiline heredocs in Erlang :'(
"
     ""}.

repeat() ->
    ["hello", "there", "hello", "there", "hello", "there" | repeat_more()].

repeat_more() ->
    ["hello",
     "there",
     "hello",
     "there",
     "hello",
     "there",
     "hello",
     "there",
     "hello",
     "there",
     "hello",
     "there"].

multiple_calls() ->
    multiple_calls_more(), repeat(), repeat(), repeat().

multiple_calls_more() ->
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat(),
    repeat().

characters() ->
    {"\x63haracters with strange representations are preserved" ++ " in small strings",
     "\x65v\x65n in multiblock strings"}.

multiline_with_spaces() ->
    "This is a multiline string and this line ends with two spaces \s
     and this one ends with two tabs		
    The spaces should not be removed by the formatter.".
