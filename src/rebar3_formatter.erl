-module(rebar3_formatter).

-export([format/2]).

-type opts() :: #{
    output_dir => undefined | string(),
    includes => [file:name()],
    macros => epp:macros(),
    encoding => none | epp:source_encoding(),
    paper => pos_integer(),
    ribbon => pos_integer()
}.
-export_type [opts/0].

-spec format(file:filename_all(), opts()) -> ok.
format(File, Opts) ->
    rebar_api:debug("Formatting ~p with ~p", [File, Opts]),
    AST = get_ast(File, Opts),
    Comments = get_comments(File),
    FileOpts = maps:merge(Opts, get_per_file_opts(AST)),
    format(File, AST, Comments, FileOpts).

get_ast(File, Opts) ->
    Includes = maps:get(includes, Opts, []),
    Macros = maps:get(macros, Opts, []),
    case epp:parse_file(File, Includes, Macros) of
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

get_per_file_opts(AST) ->
    maps:from_list([Opt || {attribute, _, format, Opts} <- AST, Opt <- Opts]).

format(File, AST, Comments, Opts) ->
    Hook = fun format_hook/3,
    User = Opts,
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
        {hook, Hook},
        {user, User},
        {paper, Paper},
        {ribbon, Ribbon},
        {encoding, Encoding}
    ],
    FilteredAST = lists:filter(fun is_original/1, AST),
    %NOTE: only annotated nodes go through the hooks
    AnnotatedAST = [erl_syntax:add_ann(ann, F) || F <- FilteredAST],
    rebar_api:debug("~s looks like:~n~p", [File, AnnotatedAST]),
    rebar_api:debug("~s comments are:~n~p", [File, Comments]),
    WithComments =
        erl_recomment:recomment_forms(
            erl_syntax:form_list(AnnotatedAST), Comments),
    Formatted = erl_prettypr:format(WithComments, FormatOpts),
    rebar_api:debug("~s NOW looks like:~n~p", [File, Formatted]),
    file:write_file(FinalFile, Formatted).

is_original({attribute, 1, file, _}) -> false;
is_original({attribute, [{generated, true} | _], _, _}) -> false;
is_original(_) -> true.

%% @todo Apply OUR rules
%% @todo Remove the debug line, it's calling Continuation twice
format_hook(AST, Ctx, Continuation) ->
    rebar_api:debug(
        "\nAST=~p\nCtx=~p\nResult=~p", [AST, Ctx, Continuation(AST, Ctx)]),
    Continuation(AST, Ctx).
