-module(macros_in_binary).

-export([chunkify/1, p/0, comments/1]).

-define(CHUNK_SIZE, 65535 - 1).
-define(other_chunk, 10).
-define(CONCAT(A, B), <<A/binary, B/binary>>).
-define(A_MACRO(X), X + 1).
-define(A_FUNCTION, rand:uniform).
-define(NO_PARENS, <<"no_parens">>).

chunkify(<<B1:(?CHUNK_SIZE)/binary, B2:(?other_chunk)/binary, Rest/binary>>) ->
    {<<(?CONCAT(B1, Rest))/binary>>, <<(?CONCAT(B2, Rest))/binary>>}.

p() ->
    <<begin
          X = ?A_FUNCTION(10),
          X + ?A_MACRO(X)
      end/integer,
      ?NO_PARENS/binary>>.

j(A) ->
    ?A_FUNCTION(A) + ?A_MACRO(A).

comments(X) ->
    % COMMENT 1
    % COMMENT 1.1
    ?CONCAT(% COMMENT 2
            % COMMENT 2.2
            "1", % COMMENT 3
            % COMMENT 3.3
            "2").
