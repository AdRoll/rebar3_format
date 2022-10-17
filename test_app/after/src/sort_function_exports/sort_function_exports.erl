-module(sort_function_exports).

%% order of the function bodies is not relevant; what is formatted is the
%% export list
-format #{sort_function_exports => alphabetically}.

-export([abc_first_function/1, second_function/2, second_function/3]).

abc_first_function(_) ->
    ok.

second_function(_, _, _) ->
    second_function().

second_function() ->
    ok.

second_function(_, _) ->
    ok.
