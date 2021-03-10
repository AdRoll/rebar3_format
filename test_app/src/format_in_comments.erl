-module(format_in_comments).

%%% These two comments below should be correctly parsed and used
% @format #{paper => 20}.
   %% @format #{inline_items => all}.

-export([func/0]).

func() -> [a, list, that, doesnt, fit, in, 20, chars].
