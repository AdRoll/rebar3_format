-module(inline_attributes_when_under).

-format #{inline_attributes => {when_under, 4}}.

-export [these/0, functions/0, shouldnt/0, be/0, inlined/0].

-callback these() -> {elements, should, be, inlined}.
-callback functions() -> [should | be | inlined | too].
-callback should() -> should.
-callback be() -> ok.
-callback inlined(even, If, they, have, arguments) -> If.
-callback either() -> either.

-optional_callbacks [these/0, functions/0, shouldnt/0, be/0, inlined/5, either/0].

-type these() :: {types, shouldnt, be, inlined}.
-type types() :: [all | 'of' | them].
-type shouldnt() :: shouldnt.
-type be() :: be.
-type inlined() :: inlined.
-type either() :: either.

-export_type [these/0, types/0, shouldnt/0, be/0, inlined/0, either/0].

-type the_attributes(Of, This, Type) :: #{should => Of, be => This, inlined => Type, but => the, map => respects, inline_fields => none}.

these() -> ok.
functions() -> ok.
shouldnt() -> ok.
be() -> ok.
inlined() -> ok.
