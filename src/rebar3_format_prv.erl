%%% @doc Plugin provider for rebar3
-module(rebar3_format_prv).

-export([init/1, do/1, format_error/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, format},
                                 {module, rebar3_format_prv},
                                 {bare, true},
                                 {deps, [app_discovery]},
                                 {example, "rebar3 format"},
                                 {opts,
                                  [{files,
                                    $f,
                                    "files",
                                    string,
                                    "List of files and directories to be formatted"},
                                   {verify, $v, "verify", boolean, "Just verify, don't format"},
                                   {output,
                                    $o,
                                    "output",
                                    string,
                                    "Output directory for the formatted files"}]},
                                 {short_desc, "A rebar plugin for code formatting"},
                                 {desc, ""}]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, iodata()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Action = get_action(Args),
    OutputDirOpt = get_output_dir(Action, Args),
    Formatter = get_formatter(State),
    Opts = maps:put(action, Action, maps:put(output_dir, OutputDirOpt, get_opts(State))),
    rebar_api:debug("Formatter options: ~p", [Opts]),
    Files = get_files(Args, State),
    rebar_api:debug("Found ~p files: ~p", [length(Files), Files]),
    case format(Files, Formatter, Opts) of
      ok ->
          {ok, State};
      {error, Error} ->
          {error, format_error(Error)}
    end.

%% @private
-spec format_error(any()) -> string().
format_error({unformatted_files, Files}) ->
    Msg = "The following files are not properly formatted:\n~p",
    io_lib:format(Msg, [Files]);
format_error({erl_parse, File, Error}) ->
    Msg = "Error while parsing ~s: ~p.\n\tTry running with DEBUG=1 for "
          "more information",
    io_lib:format(Msg, [File, Error]);
format_error(Reason) ->
    io_lib:format("Unknown Formatting Error: ~p", [Reason]).

-spec get_action(proplists:proplist()) -> format | verify.
get_action(Args) ->
    case lists:keyfind(verify, 1, Args) of
      {verify, true} ->
          verify;
      _ ->
          format
    end.

-spec get_files(proplists:proplist(), rebar_state:t()) -> [file:filename_all()].
get_files(Args, State) ->
    Patterns = case lists:keyfind(files, 1, Args) of
                 {files, Wildcard} ->
                     [Wildcard];
                 false ->
                     case proplists:get_value(files, rebar_state:get(State, format, []), undefined)
                         of
                       undefined ->
                           ["src/**/*.[he]rl"];
                       Wildcards ->
                           Wildcards
                     end
               end,
    [File || Pattern <- Patterns, File <- filelib:wildcard(Pattern)].

-spec get_output_dir(format | verify, proplists:proplist()) -> none |
                                                               current |
                                                               file:filename_all().
get_output_dir(Action, Args) ->
    case {lists:keyfind(output, 1, Args), Action} of
      {{output, OutputDir}, _} ->
          OutputDir;
      {false, format} ->
          current;
      {false, verify} ->
          none
    end.

-spec get_formatter(rebar_state:t()) -> module().
get_formatter(State) ->
    proplists:get_value(formatter, rebar_state:get(State, format, []), default_formatter).

-spec get_opts(rebar_state:t()) -> rebar3_formatter:opts().
get_opts(State) ->
    proplists:get_value(options, rebar_state:get(State, format, []), #{}).

-spec format([file:filename_all()], module(), rebar3_formatter:opts()) -> ok |
                                                                          {error, term()}.
format(Files, Formatter, Opts) ->
    try lists:filter(fun (File) ->
                             rebar_api:debug("Formatting ~p with ~p", [File, Opts]),
                             changed == rebar3_formatter:format(File, Formatter, Opts)
                     end,
                     Files)
    of
      [] ->
          ok;
      ChangedFiles ->
          case maps:get(action, Opts) of
            format ->
                ok;
            verify ->
                {error, {unformatted_files, ChangedFiles}}
          end
    catch
      _:{cant_parse, File, {Line, erl_parse, Error}} ->
          rebar_api:warn("Couldn't parse ~s:~p ~p", [Line, File, Error]),
          {error, {erl_parse, File, Error}};
      _:Error:Stack ->
          rebar_api:warn("Error parsing files: ~p~nStack: ~p", [Error, Stack]),
          {error, Error}
    end.
