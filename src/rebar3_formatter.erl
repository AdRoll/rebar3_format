%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([format/3]).

-type opts() :: #{output_dir => undefined | file:filename_all(),
                  encoding => none | epp:source_encoding(),
                  _ => _}.

-export_type([opts/0]).

-callback format(erl_syntax:forms(), [pos_integer()], opts()) -> string().

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), module(), opts()) -> ok.
format(File, Formatter, Opts) ->
    FileOpts = apply_per_file_opts(File, Opts),
    % io:format("~p", [FileOpts]),
    case maps:get(ignore, FileOpts, false) of
      true -> ok;
      false ->
          AST = get_ast(File),
          QuickAST = get_quick_ast(File),
          Comments = get_comments(File),
          NewFile = format(File, AST, Formatter, Comments, FileOpts),
          case get_quick_ast(NewFile) of
            QuickAST -> ok;
            _ -> erlang:error({modified_ast, File, NewFile})
          end
    end.

get_ast(File) ->
    case ktn_dodger:parse_file(File, [{scan_opts, [text]}]) of
      {ok, AST} ->
          case [Error || {error, Error} <- AST] of
            [] -> AST;
            [Error | _] -> erlang:error({cant_parse, File, Error})
          end;
      {error, OpenError} -> erlang:error({cant_parse, File, OpenError})
    end.

get_quick_ast(File) ->
    case ktn_dodger:quick_parse_file(File) of
      {ok, AST} -> remove_line_numbers(AST);
      {error, OpenError} -> erlang:error({cant_parse, File, OpenError})
    end.

%% @doc Removes line numbers from ASTs to allow for "semantic" comparison
remove_line_numbers(AST) when is_list(AST) -> lists:map(fun remove_line_numbers/1, AST);
remove_line_numbers(AST) when is_tuple(AST) ->
    [Type, _Line | Rest] = tuple_to_list(AST),
    case Type of
      error -> AST;
      Type -> list_to_tuple([Type, no | remove_line_numbers(Rest)])
    end;
remove_line_numbers(AST) -> AST.

get_comments(File) -> erl_comment_scan:file(File).

%% @doc We need to use quick_parse_file/1 here because the returned format
%%      is much more manageable than the one returned by parse_file/1
apply_per_file_opts(File, Opts) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    FileOpts = [Opt || {attribute, _, format, Opt} <- AST],
    lists:foldl(fun (Map, Acc) -> maps:merge(Acc, Map) end, Opts, FileOpts).

format(File, AST, Formatter, Comments, Opts) ->
    FinalFile = case maps:get(output_dir, Opts) of
                  undefined -> File;
                  OutputDir -> filename:join(filename:absname(OutputDir), File)
                end,
    ok = filelib:ensure_dir(FinalFile),
    ExtendedAST = AST ++ [{eof, 0}],
    WithComments = erl_recomment:recomment_forms(erl_syntax:form_list(ExtendedAST), Comments),
    Formatted = Formatter:format(WithComments, empty_lines(File), Opts),
    ok = file:write_file(FinalFile, Formatted),
    FinalFile.

empty_lines(File) ->
    {ok, Data} = file:read_file(File),
    List = binary:split(Data, [<<"\n">>], [global, trim]),
    {ok, NonEmptyLineRe} = re:compile("\\S"),
    {Res, _} = lists:foldl(fun (Line, {EmptyLines, N}) ->
                                   case re:run(Line, NonEmptyLineRe) of
                                     {match, _} -> {EmptyLines, N + 1};
                                     nomatch -> {[N | EmptyLines], N + 1}
                                   end
                           end,
                           {[], 1},
                           List),
    lists:reverse(Res).

