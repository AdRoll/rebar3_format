-module(trailing_spaces).

-compile(export_all).

-format(#{inline_expressions => false, preserve_empty_lines => true,
          remove_trailing_spaces => false}).

this_function() ->
    has:multiple_lines(),
    
    with:whitespace(on, their, right),
    those:spaces(should:be(preserved)).

