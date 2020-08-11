%% @doc New stuff introduced in OTP23.
-module(otp23).

-export([underscores/0, eep52_1/1, eep52_3/1, eep52_5/2, eep52_6/2]).

underscores() ->
    #{1_2_3_4 := X} = #{1_2_3_4 => 5_6_7},
    X = 567.

eep52_1(<<Size:8, Payload:((Size - 1) * 8)/binary, Rest/binary>>) ->
    {Payload, Rest}.

eep52_3(<<X:(1 / 0)>>) ->
    X;
eep52_3(<<X:not_integer>>) ->
    X;
eep52_3(_) ->
    no_match.

eep52_5(M, X) ->
    #{{tag, X} := Value} = M,
    Value.

eep52_6(Bin, V) ->
    <<X:(is_list(V))>> = Bin,
    X.
