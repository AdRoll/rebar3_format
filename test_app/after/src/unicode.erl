-module(unicode).

-export([unicode_words/0]).

unicode_words() ->
    Word1 = <<"Hwæt! Wē Gār‐Dena in geār‐dagum">>,
    Word2 = <<"þēod‐cyninga þrym gefrūnon,">>,
    <<Word1/binary, Word2/binary>>.

