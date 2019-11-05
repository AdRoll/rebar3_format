-module(per_file).

-export([this/0]).

-format([{paper,
	  20}]).

this() ->
    [file, should,
     be, very, very,
     narrow].

