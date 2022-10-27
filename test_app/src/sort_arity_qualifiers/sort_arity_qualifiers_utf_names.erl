-module(sort_arity_qualifiers_utf_names).

-format #{sort_arity_qualifiers => true}.

-export(['😎'/0, '👁👄👁'/0, abc/0]).

'👁👄👁'() ->
    ok.

'😎'() ->
    ok.

abc() ->
    ok.
