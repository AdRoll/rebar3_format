-module(sort_arity_qualifiers_single_export_list).

%% order of the function bodies is not relevant; what is formatted is the
%% export list
-format #{sort_arity_qualifiers => true}.

-export([abc_first_function/1, second_function/2, second_function/3]).

abc_first_function(_) ->
    ok.

second_function(_, _, _) ->
    second_function().

second_function() ->
    ok.

second_function(_, _) ->
    ok.
