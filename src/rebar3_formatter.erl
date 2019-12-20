%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([format/2]).

-type opts() :: #{files => [file:filename_all()],
                  output_dir => undefined | string(), encoding => none | epp:source_encoding(),
                  paper => pos_integer(), ribbon => pos_integer(), break_indent => pos_integer(),
                  sub_indent => pos_integer(), remove_tabs => boolean(),
                  remove_trailing_spaces => boolean(), inline_items => boolean(),
                  inline_expressions => boolean(), preserve_empty_lines => boolean()}.

-export_type([opts/0]).

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), opts()) -> ok.

format(File, Opts) ->
    AST = get_ast(File),
    Comments = get_comments(File),
    FileOpts = maps:merge(Opts, get_per_file_opts(File)),
    format(File, AST, Comments, FileOpts).

get_ast(File) ->
    case ktn_dodger:parse_file(File) of
      {ok, AST} ->
          case [Error || {error, Error} <- AST] of
            [] -> AST;
            [Error | _] -> erlang:error({cant_parse, File, Error})
          end;
      {error, OpenError} -> erlang:error({cant_parse, File, OpenError})
    end.

get_comments(File) -> erl_comment_scan:file(File).

%% @doc We need to use quick_parse_file/1 here because the returned format
%%      is much more manageable than the one returned by parse_file/1
get_per_file_opts(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    maps:from_list([Opt || {attribute, _, format, Opts} <- AST, Opt <- Opts]).

format(File, AST, Comments, Opts) ->
    Paper = maps:get(paper, Opts, 100),
    Ribbon = maps:get(ribbon, Opts, 80),
    Encoding = maps:get(encoding, Opts, utf8),
    BreakIndent = maps:get(break_indent, Opts, 4),
    SubIndent = maps:get(sub_indent, Opts, 2),
    RemoveTabs = maps:get(remove_tabs, Opts, true),
    RemoveTrailingSpaces = maps:get(remove_trailing_spaces, Opts, true),
    InlineItems = maps:get(inline_items, Opts, true),
    InlineExpressions = maps:get(inline_expressions, Opts, true),
    PreserveEmptyLines = maps:get(preserve_empty_lines, Opts, false),
    FinalFile = case maps:get(output_dir, Opts) of
                  undefined -> File;
                  OutputDir -> filename:join(filename:absname(OutputDir), File)
                end,
    ok = filelib:ensure_dir(FinalFile),
    FormatOpts = [{paper, Paper}, {ribbon, Ribbon}, {encoding, Encoding},
                  {break_indent, BreakIndent}, {sub_indent, SubIndent},
                  {inline_items, InlineItems}, {inline_expressions, InlineExpressions}],
    ExtendedAST = AST ++ [{eof, 0}],
    WithComments = erl_recomment:recomment_forms(erl_syntax:form_list(ExtendedAST),
                                                 Comments),
    PreFormatted = rebar3_prettypr:format(WithComments,
                                          empty_lines(InlineExpressions, PreserveEmptyLines, File),
                                          FormatOpts),
    Formatted = maybe_remove_tabs(RemoveTabs,
                                  unicode:characters_to_binary(PreFormatted, Encoding)),
    Clean = maybe_remove_trailing_spaces(RemoveTrailingSpaces, Formatted),
    file:write_file(FinalFile, Clean).

maybe_remove_tabs(false, Formatted) -> Formatted;
maybe_remove_tabs(true, Formatted) ->
    binary:replace(Formatted, <<"\t">>, <<"        ">>, [global]).

maybe_remove_trailing_spaces(false, Formatted) -> Formatted;
maybe_remove_trailing_spaces(true, Formatted) ->
    re:replace(Formatted, <<" +\n">>, <<"\n">>, [global, {return, binary}]).

empty_lines(true, _, _) -> [];
empty_lines(false, false, _) -> [];
empty_lines(false, true, File) ->
    {ok, Data} = file:read_file(File),
    List = binary:split(Data, [<<"\n">>], [global, trim]),
    {ok, NonEmptyLineRe} = re:compile("\\S"),
    {Res, _} = lists:foldl(fun (Line, {EmptyLines, N}) ->
                                   case re:run(Line, NonEmptyLineRe) of
                                     {match, _} -> {EmptyLines, N + 1};
                                     nomatch -> {[N | EmptyLines], N + 1}
                                   end
                           end,
                           {[], 1}, List),
    lists:reverse(Res).

