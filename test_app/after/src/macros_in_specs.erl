-module(macros_in_specs).

-type t() :: t.
-type foo() :: foo.

-define(FOO, foo).

%% @doc Is properly formatted
-spec ?MODULE:f() -> t().
f() ->
    t.

%% @doc Was crashing the formatter
-spec g() -> ?MODULE:t().
g() ->
    t.

%% @doc Can't be parsed
-spec a_module : ?FOO( ) -> t( ) .

foo() ->
    t.

%% @doc Was crashing the formatter
-spec h() -> a_module:?FOO().
h() ->
    foo.
