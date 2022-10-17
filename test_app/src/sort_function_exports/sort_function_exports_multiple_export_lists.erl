-module(sort_function_exports_multiple_export_lists).

-format #{sort_function_exports => alphabetically}.

-export [x/1, x/10, x/5].
-export([a_is_first_function/0, z_definitively_second_function/0]).

x(_) ->
    ok.

x(A, B, C, D, E) ->
    x(A, B, C, D, E, 1, 2, 3, 4, 5).

x(_, _, _, _, _, _, _, _, _, _) ->
    ok.

z_definitively_second_function() ->
    ok.

a_is_first_function() ->
    ok.
