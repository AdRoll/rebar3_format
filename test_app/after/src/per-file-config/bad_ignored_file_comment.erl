-module(bad_ignored_file_comment).

%%%% you have to place the `@` sign exactly one space after the last `%`
%  @format ignore.
%%%% you have to place the whole map with options in a single line...
% @format #{paper => 10
%           ribbon => 9}.
%%%% ...that **has to end** in period (`.`).
% @format ignore

-export([formatted_func/0]).

%% Since every @format above is misconfigured, this will be formatted.
formatted_func() ->
    case 2 > 3 of
        true ->
            ok;
        false ->
            error
    end.
