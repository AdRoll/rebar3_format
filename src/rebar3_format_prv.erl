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
                                 {example, "rebar3 format [file(s)]"},
                                 {opts,
                                  [{files,
                                    $f,
                                    "files",
                                    string,
                                    "List of files and directories to be formatted"},
                                   {output,
                                    $o,
                                    "output",
                                    string,
                                    "Output directory for the formatted files"}]},
                                 {short_desc, "A rebar plugin for code formatting"},
                                 {desc, ""}]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, binary()}.
do(State) ->
    OutputDirOpt = get_output_dir(State),
    Formatter = get_formatter(State),
    Opts = maps:put(output_dir, OutputDirOpt, get_opts(State)),
    rebar_api:debug("Formatter options: ~p", [Opts]),
    Files = get_files(State),
    rebar_api:debug("Found ~p files: ~p", [length(Files), Files]),
    case format(Files, Formatter, Opts) of
      ok -> {ok, State};
      {error, Error} -> {error, format_error(Error)}
    end.

%% @private
-spec format_error(any()) -> binary().
format_error({erl_parse, Error}) ->
    Msg = "Formatting error: ~p.Try running with DEBUG=1 for more information",
    iolist_to_binary(io_lib:format(Msg, [Error]));
format_error(Reason) ->
    iolist_to_binary(io_lib:format("Unknown Formatting Error: ~p", [Reason])).

-spec get_files(rebar_state:t()) -> [file:filename_all()].
get_files(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Patterns = case lists:keyfind(files, 1, Args) of
                 {files, Wildcard} -> [Wildcard];
                 false ->
                     case proplists:get_value(files, rebar_state:get(State, format, []), undefined)
                         of
                       undefined -> ["src/**/*.?rl"];
                       Wildcards -> Wildcards
                     end
               end,
    [File || Pattern <- Patterns, File <- filelib:wildcard(Pattern)].

-spec get_output_dir(rebar_state:t()) -> undefined | string().
get_output_dir(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case lists:keyfind(output, 1, Args) of
      {output, OutputDir} -> OutputDir;
      false -> undefined
    end.

-spec get_formatter(rebar_state:t()) -> module().
get_formatter(State) ->
    proplists:get_value(formatter, rebar_state:get(State, format, []), default_formatter).

-spec get_opts(rebar_state:t()) -> rebar3_formatter:opts().
get_opts(State) -> proplists:get_value(options, rebar_state:get(State, format, []), #{}).

-spec format([file:filename_all()], module(), rebar3_formatter:opts()) -> ok |
                                                                          {error,
                                                                           {atom(), string()}}.
format(Files, Formatter, Opts) ->
    try
      lists:foreach(fun (File) ->
                            rebar_api:debug("Formatting ~p with ~p", [File, Opts]),
                            rebar3_formatter:format(File, Formatter, Opts)
                    end,
                    Files)
    catch
      _:{cant_parse, File, {_, erl_parse, Error}} ->
          rebar_api:debug("Couldn't parse ~s: ~p", [File, Error]),
          {error, {erl_parse, Error}};
      _:Error:Stack ->
          rebar_api:warn("Error parsing files: ~p~nStack: ~p", [Error, Stack]),
          {error, Error}
    end.

