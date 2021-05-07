-module(macros_in_binary).

-export([chunkify/1, p/0]).

-define(CHUNK_SIZE, 65535 - 1).
-define(CONCAT(A, B), <<A/binary, B/binary>>).
-define(A_MACRO(X), X + 1).
-define(A_FUNCTION, rand:uniform).
-define(NO_PARENS, <<"no_parens">>).

chunkify(<<Binary:?CHUNK_SIZE/binary, Rest/binary>>) ->
    {<<(?CONCAT(Binary, Rest))/binary>>, <<?CONCAT(Binary, Rest)/binary>>}.

p() ->
    <<begin
          X = (?A_FUNCTION(10)),
          X + ?A_MACRO(X)
      end/integer,
      ?NO_PARENS/binary>>.

j(A) -> ?A_FUNCTION(A) + ?A_MACRO(A).

