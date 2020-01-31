-module(strings).

-export([all/0]).

all() -> heredoc(), superlong(), repeat(), multiple_calls().

superlong() ->
    "This is a super super super super super super super super super "
    "super super super super super super super super super super "
    "super super super super super super super super super super "
    "long string!".

heredoc() -> {ok, "\nThis is\na multiline\nheredoc\n"}.

repeat() -> ["hello", "there", "hello", "there", "hello", "there" | repeat_more()].

repeat_more() ->
    ["hello", "there", "hello", "there", "hello", "there", "hello", "there", "hello", "there",
     "hello", "there"].

multiple_calls() -> multiple_calls_more(), repeat(), repeat(), repeat().

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

