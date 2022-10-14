-module(sort_exported_funcs).

%% order of the function bodies is not relevant, what the format is the
%% export list
-format #{sort_exported_funcs => alphabetically}.

-export([abc_first_function/1, second_function/2, second_function/3]).

abc_first_function(_) ->
    ok.

second_function(_, _, _) ->
    second_function().

second_function() ->
    ok.

second_function(_, _) ->
    ok.
