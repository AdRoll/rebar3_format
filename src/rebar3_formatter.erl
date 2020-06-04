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
    case apply_per_file_opts(File, Opts) of
      ignore ->
          ignore(File, Formatter, Opts),
          unchanged;
      FileOpts ->
          Formatter:format(File, FileOpts)
    end.

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

%% @doc We need to use quick_parse_file/1 here because the returned format
%%      is much more manageable than the one returned by parse_file/1
apply_per_file_opts(File, Opts) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    FileOpts = [Opt || {attribute, _, format, Opt} <- AST],
    case lists:member(ignore, FileOpts) of
      true ->
          ignore;
      false ->
          MergeF = fun (Map, Acc) ->
                           maps:merge(Acc, Map)
                   end,
          lists:foldl(MergeF, Opts, FileOpts)
    end.
