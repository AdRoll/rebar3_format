-module(function_composition_spaces).

-format #{inline_qualified_function_composition => true}.
-format #{spaces_around_arguments => true}.

-export([local_calls/3, external_calls/0]).

-type t() :: only:function(application:is(affected(by:this(change)))) | not_types.

local_calls(should, be, unaffected) ->
    g( f( b, h( a ), w( x( y ) ) ) ).

external_calls() ->
    shouldnt:be(
        indented:every(
            singe:time( [{even, "when", including}] ), local_calls( ?AND_MACROS, #{}, undefined )
        ),
        local_calls( ?MACROS(should), be, unaffected )
    ),
    but:the(
        whole:code( [{should, be}, "nicely", <<"indented">>, now:that(), we, have, spaces] )
    ),
    within( the( parenthesis() ) ).
