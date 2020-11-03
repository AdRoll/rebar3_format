-module(inline_attributes).

-format #{inline_attributes => all}.

-export [these/0, functions/0, should/0, be/0, inlined/0].

-callback these() -> {elements, should, be, inlined}.
-callback functions() -> [should | be | inlined | too].
-callback should() -> should.
-callback be() -> ok.
-callback inlined(even, If, they, have, arguments) -> If.
-callback too() -> too.

-optional_callbacks [these/0, functions/0, should/0, be/0, inlined/5, too/0].

-type these() :: {types, should, be, inlined, as, well}.
-type types() :: [all | 'of' | them].
-type should() :: should.
-type be() :: be.
-type inlined() :: inlined.
-type too() :: too.

-export_type [these/0, types/0, should/0, be/0, inlined/0, too/0].

-type the_attributes(Of, This, Type) :: #{should => Of, be => This, inlined => Type, but => the, map => respects, inline_fields => none}.

-record(the_fields, {'of', this, record, should, be, inlined, too}).

these() -> ok.
functions() -> ok.
should() -> ok.
be() -> ok.
inlined() -> ok.
too() -> ok.
