-module(unicode_support).

-export([unicode_words/0, unicode_strings/0]).

unicode_words() ->
    Word1 = <<"Hwæt! Wē Gār‐Dena in geār‐dagum">>,
    Word2 = <<"þēod‐cyninga þrym gefrūnon,">>,
    <<Word1/binary, Word2/binary>>.

unicode_strings() ->
    Str1 = "Hwæt! Wē Gār‐Dena in geār‐dagum",
    Str2 = "þēod‐cyninga þrym gefrūnon,",
    Str1 ++ Str2.

