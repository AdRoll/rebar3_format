-module(strings).

-export([all/0]).

-format #{inline_expressions => true, inline_items => true}.

all() ->
  heredoc(),
  superlong(),
  repeat(),
  multiple_calls().


superlong() ->
  "This is a super super super super super super super super super super super super super super super super super super super super super super super super super super super super super long string!".

heredoc() ->
{ok, """
This is
a multiline
heredoc
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
