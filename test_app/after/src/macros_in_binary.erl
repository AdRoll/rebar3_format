-module(macros_in_binary).

-export([chunkify/1, p/0]).

-define(CHUNK_SIZE, 65535 - 1).
-define(CONCAT(A, B), <<A/binary, B/binary>>).
-define(A_MACRO(X), X + 1).

chunkify(<<Binary:(?CHUNK_SIZE)/binary, Rest/binary>>) ->
    {<<(?CONCAT(Binary, Rest))/binary>>, <<(?CONCAT(Binary, Rest))/binary>>}.

p() ->
    <<begin
          X = rand:uniform(10),
          X + (?A_MACRO(X))
      end/integer>>.
