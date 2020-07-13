%%% @doc Formatter to integrate with
%%%      <a target="_blank" href="https://github.com/whatsapp/erlfmt">erlfmt</a>.
-module(erlfmt_formatter).

-behaviour(rebar3_formatter).

-export([init/2, format_file/3]).

%% Initialize the formatter and generate a state that will be passed in when
%% calling other callbacks
-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) -> nostate.
init(_, _) ->
    {ok, _} = application:ensure_all_started(erlfmt),
    nostate.

%% @doc Format a file.
%%      Note that opts() are not the same as the global ones passed in on init/1.
%%      These opts include per-file options specified with the -format attribute.
-spec format_file(file:filename_all(), nostate, rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, nostate, OptionsMap) ->
    Out =
        case maps:get(output_dir, OptionsMap, current) of
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
    OutFile =
        case Out of
          replace ->
              File;
          Out ->
              Out
        end,

    {ok, Code} = file:read_file(File),

    Pragma =
        case maps:get(require_pragma, OptionsMap, false) of
          true ->
              require;
          false ->
              ignore
        end,
    try erlfmt:format_file(File, {Pragma, Out}) of
      skip ->
          unchanged;
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
      error:function_clause ->
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
          end;
      _:{error, Reason} ->
          erlang:error(Reason)
    end.
