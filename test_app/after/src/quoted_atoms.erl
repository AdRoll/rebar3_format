%% Module names can't be _forcefully_ quoted
-module(quoted_atoms).

-format #{unquote_atoms => false}.

%% Function names can't be _forcefully_ quoted, either
%% But if the quotes are "required", they stay.
-export([qf/1, 'QF'/1]).

%% Type names can't be _forcefully_ quoted, either
-type qt() :: 'qp'.
%% But if the quotes are "required", they stay.
-type 'QT'() :: 'QT'.

-export_type([qt/0, 'QT'/0]).

-spec qf('qp') -> 'qr'.
qf('qp') ->
    'qr'.

-spec 'QF'(qt()) -> qr.
'QF'(qp) ->
    qr.
