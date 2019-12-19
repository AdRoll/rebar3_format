%% @doc All the lists of items in this module should be placed
%%      in a single line if they're small enough, but one item
%%      per line if they're large.s
-module inline_items.

-record(short_rec, {x, y, z}).
-record(long_rec, {
    x1 = x1 :: x1,
    x2 = x2 :: x2,
    x3 = x3 :: x3,
    y1 = y1 :: y1,
    y2 = y2 :: y2,
    y3 = y3 :: y3,
    very_very_long_name_1 = very_very_long_name_1 :: very_very_long_name_1,
    very_very_long_name_2 = very_very_long_name_2 :: very_very_long_name_2,
    very_very_long_name_3 = very_very_long_name_3 :: very_very_long_name_3
}).

-format [{inline_items, false}, {inline_expressions, false}, {paper, 80}].

-export [short_tuple/0, short_list/0, short_fun/0].
-export [short_bin/0, short_guard/1, short_lc/0].
-export [short_bc/0, short_arglist/3, short_rec/0].
-export [short_map/0].

-export [long_tuple/0, long_list/0, long_fun/0, long_bin/0, long_guard/1, long_lc/0, long_bc/0,
         long_arglist/7, long_rec/0, long_map/0].

-spec short_tuple() -> {T, T, T} when T :: {x, y, z}.
short_tuple() ->
    X = {x, y, z},
    Y = {x, y, z},
    {X, Y, {x, y, z}}.

-spec short_list() -> [T | [T]] when T :: [x | y | z].
short_list() ->
    X = [x, y, z],
    Y = [x, y| z],
    [X, Y, [x | y:z()]].

-spec short_fun() -> fun((X, Y, Z) -> {X, Y, Z}).
short_fun() ->
    fun(X,Y,Z) ->
        {X, Y, Z}
    end.

-spec short_bin() -> binary().
short_bin() ->
    X = <<1, 2, 3>>,
    Y = <<1:1, 7:7, 2:2/little-integer-unit:32, 3/float>>,
    <<X/binary, Y/binary>>.

-spec short_guard(integer()) -> integer().
short_guard(X) when is_integer(X), X < 2 ->
    case X of
        X when X >= -1 -> X + 1;
        X -> X
    end.

-spec short_lc() -> [{_, _, _}].
short_lc() ->
    [{X, Y, Z} || {X, Y, Z} <- x:y(z), Z < Y].

-spec short_bc() -> binary().
short_bc() ->
    << <<X, Y, Z>> || <<X, Y, Z>> <= x:y(z), Z < Y >>.

-spec short_arglist(number(), number(), number()) -> number().
short_arglist(X, Y, Z) -> X+Y+Z.

-spec short_rec() -> #short_rec{x :: x, y :: y, z :: z}.
short_rec() -> #short_rec{x = x, y = y, z = z}.

-spec short_map() -> #{x => x, y => y, z := z}.
short_map() -> #{x => x, y => y, z => z}.



-spec long_tuple() -> {T, T, T} when T :: {x, y, z}.
long_tuple() ->
    X = {x1, x2, x3, x4, y1, y2, y3, y4, very_very_long_name_1, very_very_long_name_2, very_very_long_name_3},
    Y = {x1, x2, x3, x4, y1, y2, y3, y4, very_very_long_name_1, very_very_long_name_2, very_very_long_name_3},
    {X, Y, {x1, x2, x3, x4, y1, y2, y3, y4, very_very_long_name_1, very_very_long_name_2, very_very_long_name_3}}.

-spec long_list() -> [T | [T]] when T :: [x1 | x2 | x3 | x4 | y1 | y2 | y3 | y4 | very_very_long_name_1 | very_very_long_name_2 | very_very_long_name_3].
long_list() ->
    X = [x1, x2, x3, x4, y1, y2, y3, y4, very_very_long_name_1, very_very_long_name_2, very_very_long_name_3],
    Y = [x1, x2, x3, x4, y1, y2, y3, y4, very_very_long_name_1, very_very_long_name_2| very_very_long_name_3],
    [X, Y, [x1, x2, x3, x4, y1, y2, y3, y4, very_very_long_name_1, very_very_long_name_2, very_very_long_name_3 | y:z()]].

-spec long_fun() -> fun((X1, X2, X3, X4, Y1, Y2, Y3, Y4, VeryVeryLongName1, VeryVeryLongName2, VeryVeryLongName3) -> {X1, X2, X3, X4, Y1, Y2, Y3, Y4, VeryVeryLongName1, VeryVeryLongName2, VeryVeryLongName3}).
long_fun() ->
    fun(X1, X2, X3, X4, Y1, Y2, Y3, Y4, VeryVeryLongName1, VeryVeryLongName2, VeryVeryLongName3) ->
        {X1, X2, X3, X4, Y1, Y2, Y3, Y4, VeryVeryLongName1, VeryVeryLongName2, VeryVeryLongName3}
    end.

-spec long_bin() -> binary().
long_bin() ->
    X = <<1, 1, 1, 1, 2, 2, 2, 2, 333333333333333333, 333333333333333333, 333333333333333333, 333333333333333333>>,
    Y = <<1:1, 1:1, 1:1, 1:1, 4:4, 2:2/little-integer-unit:32, 2:2/little-integer-unit:32, 2:2/little-integer-unit:32, 3/float, 3/float, 3/float, 3/float>>,
    <<X/binary, Y/binary, X/binary, Y/binary, X/binary, Y/binary, X/binary, Y/binary, X/binary, Y/binary>>.

-spec long_guard(integer()) -> integer().
long_guard(VeryVeryLongName) when is_integer(VeryVeryLongName), VeryVeryLongName < 2, VeryVeryLongName < 3; VeryVeryLongName > 2 andalso VeryVeryLongName > 3 ->
    if VeryVeryLongName >= -1; VeryVeryLongName < 1, VeryVeryLongName > 0; VeryVeryLongName == 0 -> VeryVeryLongName + 1;
        VeryVeryLongName -> VeryVeryLongName
    end.

-spec long_lc() -> [{_, _, _}].
long_lc() ->
    [{X, Y, Z} || X <- generator:x(), X < 1, Y <- generator:y(), Z <- generator:z(), filter:x(Y, Z), X + Y == Z].

-spec long_bc() -> binary().
long_bc() ->
    << <<X, Y, Z>> || <<X>> <- generator:x(), X < 1, <<Y:2/signed-integer-unit:8>> <- generator:y(), Z <- generator:z(), filter:x(Y, Z), X + Y == Z >>.

-spec long_arglist(number(), number(), number(), float(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> number().
long_arglist(X1, X2, X3, Y1, VeryVeryLongName1, VeryVeryLongName2, VeryVeryLongName3) ->
    X1 + X2 + X3 + Y1 + VeryVeryLongName1 + VeryVeryLongName2 + VeryVeryLongName3.

-spec long_rec() -> #long_rec{
    x1 :: x1,
    x2 :: x2,
    x3 :: x3,
    y1 :: y1,
    y2 :: y2,
    y3 :: y3,
    very_very_long_name_1 :: very_very_long_name_1,
    very_very_long_name_2 :: very_very_long_name_2,
    very_very_long_name_3 :: very_very_long_name_3
}.
long_rec() -> #long_rec{
    x1 = x1,
    x2 = x2,
    x3 = x3,
    y1 = y1,
    y2 = y2,
    y3 = y3,
    very_very_long_name_1 = very_very_long_name_1,
    very_very_long_name_2 = very_very_long_name_2,
    very_very_long_name_3 = very_very_long_name_3}.

-spec long_map() -> #{
    x1 := x1,
    x2 := x2,
    x3 := x3,
    y1 := y1,
    y2 := y2,
    y3 := y3,
    very_very_long_name_1 := very_very_long_name_1,
    very_very_long_name_2 := very_very_long_name_2,
    very_very_long_name_3 := very_very_long_name_3
}.
long_map() -> #{
    x1 => x1,
    x2 => x2,
    x3 => x3,
    y1 => y1,
    y2 => y2,
    y3 => y3,
    very_very_long_name_1 => very_very_long_name_1,
    very_very_long_name_2 => very_very_long_name_2,
    very_very_long_name_3 => very_very_long_name_3}.
