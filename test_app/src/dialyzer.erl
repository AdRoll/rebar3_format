-module(dialyzer).

-dialyzer({nowarn_function, f/0}).
-dialyzer(no_improper_lists).
-dialyzer([{nowarn_function, [f/0]}, no_improper_lists]).
-dialyzer({no_improper_lists, g/0}).
-dialyzer({[no_return, no_match], [g/0, h/0]}).
-dialyzer(unmatched_returns).

-mixin([{a_module, [new/1, feature/2, log_params/2]}]).

-inline([something/4]).

-ignore_xref(metric/4).
