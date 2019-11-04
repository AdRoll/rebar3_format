-module(comments).   % module attribute
-export([fact/1]).   % module attribute
-some(thing).        % module attribute

% Regular comment
-export([
  dummy_fn/1,
  % Plz keep me!
  heredoc/0
]).


%% @doc This will all be part of the first paragraph.
%% It can stretch over several lines and contain <em>any XHTML markup</em>.
%%
%% This is the second paragraph. The above line is
%% regarded as "empty" by EDoc, even though it ends with a space.
fact(N) when N>0 ->  % beginning of function declaration
    N * fact(N-1);   %  |
fact(0) ->           %  |
    1.               % end of function declaration


%% @doc This is a oneline @doc comment
dummy_fn(A) ->
  Fn = fun(B) -> B end, % Inline comment
  % Newline comment
  Fn(A).


% This is
% a multiline
% comment
heredoc() ->
  X="""
This is
a multiline
heredoc
""",
{ok, X}.

