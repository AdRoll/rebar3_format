%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([format/2]).

-type opts() :: #{
    output_dir => undefined | string(),
    encoding => none | epp:source_encoding(),
    paper => pos_integer(),
    ribbon => pos_integer()
}.
-export_type [opts/0].

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), opts()) -> ok.
format(File, Opts) ->
    rebar_api:debug("Formatting ~p with ~p", [File, Opts]),
    AST = get_ast(File),
    Comments = get_comments(File),
    FileOpts = maps:merge(Opts, get_per_file_opts(File)),
    format(File, AST, Comments, FileOpts).

get_ast(File) ->
    case epp_dodger:parse_file(File) of
        {ok, AST} ->
            case [Error || {error, Error} <- AST] of
                [] ->
                    AST;
                [Error|_] ->
                    rebar_api:debug("Couldn't parse ~s: ~p", [File, Error]),
                    erlang:error(Error)
            end;
        {error, OpenError} -> erlang:error(OpenError)
    end.

get_comments(File) ->
    erl_comment_scan:file(File).

%% @doc We need to use quick_parse_file/1 here because the returned format
%%      is much more manageable than the one returned by parse_file/1
get_per_file_opts(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    maps:from_list([Opt || {attribute, _, format, Opts} <- AST, Opt <- Opts]).

format(File, AST, Comments, Opts) ->
    Paper = maps:get(paper, Opts, 100),
    Ribbon = maps:get(ribbon, Opts, 80),
    Encoding = maps:get(encoding, Opts, latin1),
    FinalFile =
        case maps:get(output_dir, Opts) of
            undefined -> File;
            OutputDir -> filename:join(filename:absname(OutputDir), File)
        end,
    ok = filelib:ensure_dir(FinalFile),
    FormatOpts = [
        {paper, Paper},
        {ribbon, Ribbon},
        {encoding, Encoding}
    ],
    ExtendedAST = AST ++ [{eof, 0}],
    WithComments =
        erl_recomment:recomment_forms(
            erl_syntax:form_list(ExtendedAST), Comments),
    Formatted = rebar3_prettypr:format(WithComments, FormatOpts),
    rebar_api:debug("~s NOW looks like:~n~s", [File, Formatted]),
    file:write_file(FinalFile, Formatted).
