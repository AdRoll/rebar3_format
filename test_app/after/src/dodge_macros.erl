-module(dodge_macros).

-export([slots/0]).

-define(TOTAL_SLOTS, 10).
-define(SLOTS_FUN, slots).
-define(SLOTS_TYPE, slot_id).

-on_load ?SLOTS_FUN/0.

-type ?SLOTS_TYPE() :: other_slot_id().
-type other_slot_id() :: 1..?TOTAL_SLOTS.

-spec slots() -> ?TOTAL_SLOTS.
-export_type([?SLOTS_TYPE/0]).

slots() ->
    ?TOTAL_SLOTS.

-spec slot(?SLOTS_TYPE()) -> {[$?], '?'}.
slot(?SLOTS_TYPE) ->
    {"?", '?'}.
