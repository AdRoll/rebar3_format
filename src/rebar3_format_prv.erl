-module(rebar3_format_prv).

-export([init/1, do/1, format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, format},
        {module, rebar3_format_prv},
        {bare, true},
        {deps, [app_discovery]},
        {example, "rebar3 format [file(s)]"},
        {opts, [{
            files,
            $f,
            "files",
            {string, "src/**/*.?rl"},
            "List of files and folders to be formatted"
        }]},
        {short_desc, "A rebar plugin for code formatting"},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Files = get_files(State),
    rebar_api:debug("Found ~p files: ~p", [length(Files), Files]),
    Opts = get_opts(State),
    rebar_api:debug("Formatter options: ~p", [Opts]),
    case format(Files, Opts) of
        ok ->
            {ok, State};
        {error, Error} ->
            {error, format_error(Error)}
    end.

-spec format_error(any()) ->  iodata().
format_error({_, erl_parse, Error}) ->
    io_lib:format("Formatting error: ~s. Try running with DEBUG=1 for more information", [Error]);
format_error(Reason) ->
    ["Unknown Formatting Error: ", io_lib:format("~p", [Reason])].

-spec get_files(rebar_state:t()) -> [file:filename_all()].
get_files(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    {files, Wildcard} = lists:keyfind(files, 1, Args),
    filelib:wildcard(Wildcard).

-spec get_opts(rebar_state:t()) -> rebar3_formatter:opts().
get_opts(State) ->
    maps:from_list(rebar_state:get(State, format, [])).

%% @todo Actually format the files
-spec format([file:filename_all()], rebar3_formatter:opts()) -> ok | {error, term()}.
format(Files, Opts) ->
    try lists:foreach(fun(File) -> rebar3_formatter:format(File, Opts) end, Files)
    catch
        _:Error -> {error, Error}
    end.
