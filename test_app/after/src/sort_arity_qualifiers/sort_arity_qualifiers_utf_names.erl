-module(sort_arity_qualifiers_utf_names).

-format #{sort_arity_qualifiers => true}.

-export([abc/0, '👁👄👁'/0, '😎'/0]).

'👁👄👁'() ->
    ok.

'😎'() ->
    ok.

abc() ->
    ok.
