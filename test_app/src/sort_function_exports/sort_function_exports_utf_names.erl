-module(sort_function_exports_utf_names).

-format #{sort_function_exports => true}.

-export(['😎'/0, '👁👄👁'/0, abc/0]).

'👁👄👁'() ->
    ok.

'😎'() ->
    ok.

abc() ->
    ok.
