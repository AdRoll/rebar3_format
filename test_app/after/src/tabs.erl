-module(tabs).

-compile(export_all).

-format(#{paper
	      => 50,
	  remove_tabs
	      => false,
	  ribbon =>
	      10}).

this_module() ->
    should:be(indented:using(a:mix([between,
				    tabs and
				      spaces,
				    everywhere]))).

