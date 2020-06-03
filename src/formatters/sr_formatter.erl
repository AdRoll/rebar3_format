%%% @doc Formatter to integrate with
%%%      <a target="_blank" href="https://github.com/old-reliable/steamroller">steamroller</a>.
-module(sr_formatter).

-behaviour(rebar3_formatter).

-export([format/2]).

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), rebar3_formatter:opts()) -> rebar3_formatter:result().
format(File, OptionsMap) ->
    Opts = maps:fold(fun parse_opt/3, [], OptionsMap),
    FileToFormat = case maps:get(output_dir, OptionsMap, current) of
                     current ->
                         File;
                     none ->
                         File;
                     OutputDir ->
                         copy_file(File, OutputDir)
                   end,
    {ok, Code} = file:read_file(FileToFormat),
    case steamroller_formatter:format(iolist_to_binary(FileToFormat), Opts) of
      ok ->
          case file:read_file(FileToFormat) of
            {ok, Code} ->
                unchanged;
            {ok, _} ->
                changed
          end;
      {error, <<"Check failed", _/binary>>} ->
          changed;
      {error, Reason} ->
          erlang:error(Reason)
    end.

parse_opt(action, format, Opts) ->
    Opts;
parse_opt(action, verify, Opts) ->
    [check | Opts];
parse_opt(output_dir, none, Opts) ->
    [check | Opts]; %% To avoid writing any output
parse_opt(output_dir, _, Opts) ->
    Opts;
parse_opt(K, V, Opts) ->
    [{K, V} | Opts].

copy_file(File, OutputDir) ->
    OutFile = filename:join(filename:absname(OutputDir), File),
    ct:pal("Copy ~s to ~s", [File, OutFile]),
    ok = filelib:ensure_dir(OutFile),
    {ok, _} = file:copy(File, OutFile),
    OutFile.
