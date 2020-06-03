%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([format/3]).

-type opts() :: #{output_dir => none | current | file:filename_all(),
                  encoding => none | epp:source_encoding(),
                  action => verify | format,
                  _ => _}.
-type result() :: changed | unchanged.

-export_type([opts/0, result/0]).

-callback format(file:filename_all(), opts()) -> result().

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), module(), opts()) -> result().
format(File, Formatter, Opts) ->
    Formatter:format(File, Opts).
