-module(sort_function_exports_multiple_export_lists).

-format #{sort_function_exports => alphabetically}.

-export([second_function/3, abc_first_function/1, second_function/2]).
-export([z_definitively_second_function/0, a_is_first_function/0]).

abc_first_function(_) ->
    ok.

second_function(_, _, _) ->
    second_function().

second_function() ->
    ok.

second_function(_, _) ->
    ok.

z_definitively_second_function() ->
    ok.

a_is_first_function() ->
    ok.
