-module(strings).

-export([all/0]).

-format #{inline_expressions => true}.

all() ->
  heredoc(),
  superlong(),
  repeat(),
  multiple_calls(),
  characters().


superlong() ->
  "This is a super super super super super super super super super super super super super super super super super super super super super super super super super super super super super long string!"
  "Shouldn't be truncated since truncate_strings => false by default".

heredoc() ->
{ok, """
This is
a multiline
heredoc but there are
no multiline heredocs in Erlang :'(
"""}.

repeat() ->
  ["hello", "there",
  "hello", "there",
  "hello", "there" | repeat_more()].
repeat_more() ->
  ["hello", "there",
  "hello", "there",
  "hello", "there",
  "hello", "there",
  "hello", "there",
  "hello", "there"].

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
    {
    "\x63haracters with strange representations are preserved"
    ++ " in small strings",
    "\x65v\x65n in multiblock strings"
    }.
