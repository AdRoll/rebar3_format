-module(strings).

-export([all/0]).

-format #{inline_expressions => true}.

all() ->
    heredoc(), superlong(), repeat(), multiple_calls(), characters().

superlong() ->
    "This is a super super super super super super super super super "
    "super super super super super super super super super super "
    "super super super super super super super super super super "
    "long string!".

heredoc() ->
    {ok, "\nThis is\na multiline\nheredoc\n"}.

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
     "but they're not preserved in multiblock strings"}.
