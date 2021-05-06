-module(records).

-format #{ spaces_around_fields => true }.

-record(record, {with_another :: #record{}, in_it}).
-record(rec, {}).

-type record() :: #record{ in_it :: 1..3 }.
-type rec() :: #rec{}.

f(A) ->
    _ = #rec{},
    B = C = A#record.with_another#record.with_another#record.in_it,
    D = A#record{ with_another = #record{ with_another = #record{ in_it = B }, in_it = C } },
    (normalize(D))#record.with_another#record.in_it.
