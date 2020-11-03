-module(records).

-record(record, {with_another :: #record{}, in_it}).

-type record() :: #record{in_it :: 1..3}.

f(A) ->
    B = C = A#record.with_another#record.with_another#record.in_it,
    D = A#record{with_another =
                     #record{with_another = #record{in_it = B},
                             in_it = C}},
    (normalize(D))#record.with_another#record.in_it.
