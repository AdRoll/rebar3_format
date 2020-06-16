%%% @doc Formatter to integrate with
%%%      <a target="_blank" href="https://github.com/old-reliable/steamroller">steamroller</a>.
-module(sr_formatter).

-behaviour(rebar3_formatter).

-export([init/1, format_file/3]).

%% @doc Initialize the formatter and generate a state that will be passed in when
%%      calling other callbacks.
%% @todo Actually implement what @dtip pointed to on
%%      https://github.com/AdRoll/rebar3_format/pull/112
-spec init(rebar3_formatter:opts()) -> nostate.
init(_) ->
    nostate.

%% @doc Format a file.
%%      Note that opts() are not the same as the global ones passed in on init/1.
%%      These opts include per-file options specified with the -format attribute.
-spec format_file(file:filename_all(),
                  nostate,
                  rebar3_formatter:opts()) -> rebar3_formatter:result().
format_file(File, nostate, OptionsMap) ->
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
    case steamroller:format_file(iolist_to_binary(FileToFormat), Opts) of
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
    ok = filelib:ensure_dir(OutFile),
    {ok, _} = file:copy(File, OutFile),
    OutFile.
