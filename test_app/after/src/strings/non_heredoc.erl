-module(strings).

-export([heredoc/0]).

-format #{inline_expressions => true}.

heredoc() ->
    {ok,
     "" "
This is
a multiline
heredoc but there are
no multiline heredocs in Erlang :'(
"
     ""}.
