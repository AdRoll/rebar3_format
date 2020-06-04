%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([format/3, ignore/3]).

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

%% @doc Process an ignored file.
%% @doc if output dir is not the current one we need to copy the files that we
%%      are not formatting to it
-spec ignore(file:filename_all(), module(), opts()) -> ok.
ignore(File, _Formatter, Opts) ->
    case maps:get(output_dir, Opts, current) of
      current ->
          ok;
      none ->
          ok;
      OutputDir ->
          copy_file(File, OutputDir)
    end.

copy_file(File, OutputDir) ->
    OutFile = filename:join(filename:absname(OutputDir), File),
    ok = filelib:ensure_dir(OutFile),
    {ok, _} = file:copy(File, OutFile),
    ok.
