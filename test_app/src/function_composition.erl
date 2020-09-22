-module(function_composition).

-export([local_calls/3, external_calls/0]).

-type t() :: only:function(application:is(affected(by:this(change)))) | not_types.

local_calls(should, be, unaffected) ->
    g(f(b, h(a), w(x(y)))).

external_calls() ->
    should:be(indented:every(singe:time([{even, "when", inlcuding}]), local_calls(?AND_MACROS, #{}, undefined)),
        local_calls(?MACROS(should), be, unaffected)),
    the_idea_is_to_force:long_module_and_function_names(to_be_put:in_the_next_row([{so, that}, "their", <<"parameters">>, can:be(), read, more, easily])),
    this_one(will:be(more(complex:since(it:combines(), local:and_remote(calls))), hopefully:it(is(a, rare, thing)))).
