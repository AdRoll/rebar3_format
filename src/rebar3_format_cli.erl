-module(rebar3_format_cli).
-export([main/1]).

-type opts() :: rebar3_formatter:opts() |
                #{help => true,
                  version => true}.

-spec main([string()]) -> ok.
main(Args) ->
    case handle_args(Args) of
        help    -> print_usage();
        version -> print_version();
        {error, Message} ->
            io:format(standard_error, "~s~n", [Message]),
            halt(1);
        {ok, Files, Opts} ->
            case format(Files, Opts) of
                ok -> ok;
                {error, _} -> halt(2)
            end
    end.

-spec handle_args([string()]) -> help | version | {error, string()} |
                                 {ok, [string()], opts()}.
handle_args([]) -> help;
handle_args(Args) ->
    Defaults = #{output_dir => undefined},
    try parse_opts(Args, Defaults) of
      {Files, Opts} ->
          HasHelp = maps:get(help, Opts, false),
          HasVersion = maps:get(version, Opts, false),
          if HasHelp -> help;
             HasVersion -> version;
             true -> {ok, Files, Opts}
          end
    catch
      error:Message when is_list(Message) -> {error, Message}
    end.

-spec get_ver(atom()) -> string().
get_ver(App) ->
    {_, _, Ver} = lists:keyfind(App, 1, application:loaded_applications()),
    Ver.

print_version() ->
    application:load(rebar3_format),
    io:format("rebar3_format v~s~n", [get_ver(rebar3_format)]).

print_usage() ->
    io:format("Usage: rebar3_format [options] [PATH...]~n"
    "A CLI and rebar plugin for code formatting~n~n"
    "  PATH                        Files or directories to type check~n"
    "  --                          Signals that no more options will follow. The following~n"
    "                              arguments are treated as filenames, even if~n"
    "                              they start with hyphens.~n"
    "  --output-dir                Output directory for the formatted files. Original file~n"
    "                              is overwritten if this parameter is not specified.~n"
    "  --encoding                  Encoding to use when writing files. latin1 or utf8. ~n"
    "  --paper                     The preferred maximum number of characters on any line,~n"
    "                              including indentation.~n"
    "                              Default value is 100."
    "  --ribbon                    The preferred maximum number of characters on any line,~n"
    "                              not counting indentation."
    "                              Default value is 80.~n"
    "  --break-indent              The preferred number of characters to use to indent a~n"
    "                              line that \"breaks\" from the previous one~n"
    "                              (for instance, a clause body after a clause head).~n"
    "                              Default value is 4."
    "  --sub-indent                The preferred number of characters to use to indent"
    "                              a line that \"follows\" the current one (for instance,~n"
    "                              a long clause head or a long function application)."
    "                              Default value is 2."
    "  --no-remove-tabs            Erlang's prettypr inserts a tab character each time it~n"
    "                              has to insert 8 spaces for indentation and that code is in~n"
    "                              a 100% unconfigurable/unreplaceable/unhookable function.~n"
    "                              If this flag is not used,"
    "                              the formatter will turn those tabs into 8 spaces again."
    "  --keep-trailing-spaces      The formatter will not remove trailing whitespace.~n"
    "  --avoid-inline-expressions  Always places each sequential expression on its own line.~n"
    "  --preserve-empty-lines      Preserves blank lines when formatting.~n"
    "                              One empty line will preserved for each group of empty~n"
    "                              lines that are placed between expressions in a clause.~n"
    "                              Implicitly sets --avoid-inline-expressions~n"
    "  --help, -h                  display this help and exit~n"
    "  --version                   Print the app version'~n").

-spec parse_opts([string()], opts()) -> {[string()], opts()}.
parse_opts([], Opts) ->
    {[], Opts};
parse_opts([A | Args], Opts) ->
    case A of
        "-h"                         -> {[], maps:put(help, true, Opts)};
        "--help"                     -> {[], maps:put(help, true, Opts)};
        "--version"                  -> {[], maps:put(version, true, Opts)};
        "--output-dir"               -> handle_output_dir(A, Args, Opts);
        "--encoding"                 -> handle_encoding(A, Args, Opts);
        "--paper"                    -> handle_integer_arg(paper, A, Args, Opts);
        "--ribbon"                   -> handle_integer_arg(ribbon, A, Args, Opts);
        "--break-indent"             -> handle_integer_arg(break_indent, A, Args, Opts);
        "--sub-indent"               -> handle_integer_arg(sub_indent, A, Args, Opts);
        "--no-remove-tabs"           -> parse_opts(Args, maps:put(remove_tabs, false, Opts));
        "--keep-trailing-spaces"     -> parse_opts(Args, maps:put(remove_trailing_spaces, false, Opts));
        "--avoid-inline-expressions" -> parse_opts(Args, maps:put(inline_expressions, false, Opts));
        "--preserve-empty-lines"     -> parse_opts(Args, maps:put(preserve_empty_lines, true,
                                                         maps:put(inline_expressions, false, Opts)));
        "--"                         -> {Args, Opts};
        "-" ++ _                     -> erlang:error(string:join(["Unknown parameter:", A], " "));
        _                            -> {[A | Args], Opts}
    end.

-spec handle_output_dir(string(), [string()], opts()) -> {[string()], opts()}.
handle_output_dir(_, [Dir | Args], Opts) ->
    parse_opts(Args, maps:put(output_dir, Dir, Opts));
handle_output_dir(A, [], _Opts) ->
    erlang:error(string:join(["Missing argument for", A], " ")).

-spec handle_encoding(string(), [string()], opts()) -> {[string()], opts()}.
handle_encoding(_, ["latin1" | Args], Opts) ->
    parse_opts(Args, maps:put(encoding, latin1, Opts));
handle_encoding(_, ["utf8" | Args], Opts) ->
    parse_opts(Args, maps:put(utf8, latin1, Opts));
handle_encoding(A, [], _Opts) ->
    erlang:error(string:join(["Missing argument for", A], " "));
handle_encoding(A, [Encoding | _Args], _Opts) ->
    erlang:error(string:join(["Invalid argument for", A, Encoding], " ")).

-spec handle_integer_arg(atom(), string(), [string()], opts()) -> {[string()], opts()}.
handle_integer_arg(Key, _, [IntS | Args], Opts) ->
    parse_opts(Args, maps:put(Key, list_to_integer(IntS), Opts));
handle_integer_arg(_Key, A, [], _Opts) ->
    erlang:error(string:join(["Missing argument for", A], " ")).

format(Files, Opts) ->
    try lists:foreach(fun (File) -> rebar3_formatter:format(File, Opts) end, Files)
    catch
      _:{cant_parse, File, {_, erl_parse, Error}} ->
          io:format("Couldn't parse ~s: ~p~n.", [File, Error]),
          {error, {erl_parse, Error}};
      _:Error -> io:format("Error parsing files: ~p~n.", [Error]), {error, Error}
    end.
