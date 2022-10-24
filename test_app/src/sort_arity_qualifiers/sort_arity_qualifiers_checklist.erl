-module(sort_arity_qualifiers_checklist).

-format #{sort_arity_qualifiers => true}.

-type a_first_type() :: any().
-type type_a() :: any().
-type type_b() :: any().
-type type_c() :: any().

%% will be sorted
-export_type([type_c/0, type_a/0, type_b/0, a_first_type/0]).

%% will be sorted
-export([b_fun/0, a_fun/0]).

%% will NOT be sorted
-dialyzer([{nowarn_function, a_fun/0}, no_return]).
-dialyzer([{nowarn_function, [b_fun/0, a_fun/0]}, no_return]).
-dialyzer({no_improper_lists, a_fun/0}).
-dialyzer({[no_return, no_match], [b_fun/0, a_fun/0]}).

%% will NOT be sorted
-compile({inline, [b_fun/0, a_fun/0]}).

-callback c_fun() -> ok.
-callback d_fun() -> ok.

%% will be sorted
-optional_callbacks([d_fun/0, c_fun/0]).

a_fun() ->
    ok.

b_fun() ->
    ok.
