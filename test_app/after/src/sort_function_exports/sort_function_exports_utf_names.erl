-module(sort_function_exports_utf_names).

-format #{sort_function_exports => alphabetically}.

-export([abc/0, '👁👄👁'/0, '😎'/0]).

'👁👄👁'() ->
    ok.

'😎'() ->
    ok.

abc() ->
    ok.
