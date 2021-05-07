-module(binary_fields).

-export([p/0]).

-define(FOUR, 4).
-define(SIX, 6).
-define(seven, 7).
-define(eight(X), X).
-define(NINE(X), X - 1).
-define(TEN, 5*2).

p() ->
    Four = 4,
    <<1, 2:2, 3:3/integer, 4:Four, 5:begin 5 end, 6:?SIX,
        7:?seven, 8:(?eight(8))/binary, 9:(?NINE(10))/binary, 10:(?TEN)>>.
