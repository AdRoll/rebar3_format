-module(comments).   % module attribute comment

-export([fact/1]).   % export attribute comment

-some(thing).        % some attribute comment

-format #{inline_clause_bodies => true}.

% Regular comment
-export([dummy_fn/1, multiline/0]).

  % Plz keep me! << this comment will be misplaced

-record(small,
        {f1, % The first field
         f2}).  % The second field
-record(large,
        {large_field_1, % The first field
         large_field_2, % The second field
         large_field_3, % The third field
         large_field_4,
         large_field_5}).  % The previous one didn't have a comment but this one has a very long one (really really long)

%% @doc This will all be part of the first paragraph.
%% It can stretch over several lines and contain <em>any XHTML markup</em>.
%%
%% This is the second paragraph. The above line is
%% regarded as "empty" by EDoc, even though it ends with a space.
fact(N)
    when N > 0 ->  % beginning of function declaration
    N * fact(N - 1);   %  |
fact(0) ->           %  |
    1.               % end of function declaration

%% @doc This is a oneline @doc comment
dummy_fn(A) ->
    Fn = fun (B) -> B end, % Inline comment
    % Newline comment
    Fn(A).

% This is
% a multiline
% comment
multiline() ->
    X = "\nThis is\na multiline\nstring\n",
    {ok, X}.

