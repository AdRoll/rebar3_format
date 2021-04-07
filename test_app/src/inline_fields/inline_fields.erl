-module(inline_fields).

-format #{inline_fields => all}.

-export [f/1].

-record(a, {small, with}).

-type a() :: #a{small :: record, with :: {some, fields}}.
-type b() :: #{a => small, map => with, some => fields}.

f(A) ->
    W = #just{one = field},
    X = #a{small = record, with = {some, fields}},
    Y = #{a => small, map => with, some => fields},
    #{one => {[very, W], [large, X], <<field, Y>>, that, exceeds, a, single, line, [no, matter, "what", you, do]}}.
