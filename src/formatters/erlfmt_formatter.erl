%%% @doc Formatter to integrate with
%%%      <a target="_blank" href="https://github.com/whatsapp/erlfmt">erlfmt</a>.
-module(erlfmt_formatter).

-behaviour(rebar3_formatter).

-export([format/2]).

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), rebar3_formatter:opts()) -> rebar3_formatter:result().
format(File, OptionsMap) ->
    {ok, _} = application:ensure_all_started(erlfmt),
    Out = case maps:get(output_dir, OptionsMap, current) of
            current -> %% Action can only be 'format'
                replace;
            none ->
                %% Action can only be 'verify'
                %% We need to dump the output somewhere since erlfmt has no
                %% concept of verify / check / etc.
                filename:join("/tmp", File);
            OutputDir ->
                filename:join(filename:absname(OutputDir), File)
          end,
    OutFile = case Out of
                replace ->
                    File;
                Out ->
                    Out
              end,

    {ok, Code} = file:read_file(File),

    try erlfmt:format_file(File, Out) of
      {ok, _} ->
          case file:read_file(OutFile) of
            {ok, Code} ->
                unchanged;
            {ok, _} ->
                changed
          end;
      {error, Reason} ->
          erlang:error(Reason)
    catch
      _:{error, Reason} ->
          erlang:error(Reason)
    end.
