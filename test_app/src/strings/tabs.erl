-module(tabs).

-compile(export_all).

tab(A, B) ->
    case A of
        "	" ->
            case B of
                "	" ->
                    {super, indented}
            end;
        but ->
            "what if I place a
	 at the very beginning
"
    end.
