-module(tabs).

-compile(export_all).

-format([{paper,
	  50},
	 {ribbon,
	  10},
	 {remove_tabs,
	  false}]).

this_module() ->
    should:be(indented:using(a:mix([between,
				    tabs and
				      spaces,
				    everywhere]))).

