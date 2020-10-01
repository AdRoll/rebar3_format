-module(parentheses_yes).

-compile(export_all).

-format #{parenthesize_infix_operations => true}.

numbers() ->
    Number = (((+1 + 2) + 3) - ((4 * 5) / 6)) + ((7 * 8) / (9 - 10)),
    Modulo = (100 div (7 div 3)) rem 5,
    Bits =
        ((((bnot 2#001 band 2#010) bor 2#100) bxor 2#101) bsl 2#110) bsr
            (2#111 band (2#100 bxor 2#101)),
    (Number + Modulo) - Bits.

booleans(A, B) when not B, A; A xor B ->
    bleh;
booleans(A, B) when (not B and A) or (A xor B) ->
    blip;
booleans(A, B) ->
    (((not B and A) or (A xor B)) andalso not B) orelse (A andalso B).

lists() ->
    A = [1, 2, 3] -- ([1, 2] -- [3]),
    B = ([1, 2, 3] -- [1, 2]) -- [3],
    A ++ (B -- (A ++ B)).

others(Something) ->
    C = case C_ = (catch <<Something:1/big-integer-unit:3>>) of
            _ ->
                C_
        end,
    A = B = (catch C = <<Something:1/big-integer-unit:3>>),
    {A, B, C}.

comparers(A, B) when (is_integer(A) and A) /= B ->
    bad;
comparers(A, B) when is_integer(A) andalso (A /= B) ->
    good;
comparers(A, B)
    when (A =:= B) orelse (B =/= A) -> % :P
    (A == B) orelse (((B /= (A and (A =< 1))) or B) < (0 or ((B > 100) and (A >= ugly)))).
