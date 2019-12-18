-module trailing_spaces.

-compile export_all.
-format [{remove_trailing_spaces, false}, {inline_expressions, false}, {preserve_empty_lines, true}].

this_function() ->    
    has:multiple_lines(),   
     
    with:whitespace(on, their, right),    
    those:spaces(should:be(preserved)).
