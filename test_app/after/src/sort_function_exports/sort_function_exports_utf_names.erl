-module(sort_function_exports_utf_names).

-format #{sort_function_exports => true}.

-export([abc/0, '👁👄👁'/0, '😎'/0]).

'👁👄👁'() ->
    ok.

'😎'() ->
    ok.

abc() ->
    ok.
