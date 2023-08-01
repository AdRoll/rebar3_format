-module(sort_arity_qualifiers_minimal_export_type).

-format #{sort_arity_qualifiers => true}.

-type wololo() :: ok.
-type ninjalui() :: ok.

-export_type([ninjalui/0, wololo/0]).
