%% @doc New stuff introduced in OTP26.
-module(otp26).

-export([map_comprehensions/3]).

map_comprehensions(Map, List, Binary) ->
    MapFromList = #{ {k, Key} => {value, Value} || Key <- List, Value <- List },
    MapFromBinary = #{ {k, Key} => {value, Value} || <<Key/float>> <- Binary, Key >= 8.24551123345, <<Value/binary>> <- Binary},
    MapFromMap = #{ #{key => Key} => [value, Value, is_a_value] || {k, Key} := {value, Value} <- MapFromList, is_integer(Key) },
    MapFromCombo = #{ Key => binary_to_atom(Value) || Key <- List, <<Value/binary>> <= Binary},
    ListFromMap = [{Key, Value} || {k, Key} := Value <- MapFromBinary, with:one_filter(Key), with:another_filter(Value) == <<"a good value">>],
    BinaryFromMap = << <<Key:64, $|, Value/binary>> || #{key := Key} := [value, Value | _] <- MapFromMap, is_binary(Value) >>,
    NoGenerator = #{k => v || this:is_true()},
    #{MapFromCombo => ListFromMap, BinaryFromMap => NoGenerator}.
