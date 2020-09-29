-module(rec).

-record(typed_record_field, {typed :: record_field}).

-type attribute() :: attribute.

-record(rec, {record_type_field}).

-type rec() :: #rec{record_type_field :: w}.

-spec x(Constraint, AnnotatedType :: annotated_type) -> w  when Constraint :: constraint.
