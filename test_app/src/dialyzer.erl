-module(dialyzer).

-dialyzer({nowarn_function, f/0}).
-dialyzer(no_improper_lists).
-dialyzer([{nowarn_function, [f/0]}, no_improper_lists]).
-dialyzer({no_improper_lists, g/0}).
-dialyzer({[no_return, no_match], [g/0, h/0]}).
-dialyzer(unmatched_returns).
