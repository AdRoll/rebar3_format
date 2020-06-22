-module(unquoted_atoms).

-format #{unquote_atoms => true}.

-export([qf/1, 'QF'/1]).

-type qt() :: qp.
-type 'QT'() :: 'QT'.

-export_type([qt/0, 'QT'/0]).

-spec qf(qp) -> qr.
qf(qp) ->
    qr.

-spec 'QF'(qt()) -> qr.
'QF'(qp) ->
    qr.
