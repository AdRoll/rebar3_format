-module(ignored_file_comment).

% @format ignore.

-export([bad_formatted_func/0]).

bad_formatted_func() -> case 2 > 3 of
    true -> ok; false -> error 
    end.
