%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([format/3]).

-type opts() :: #{output_dir => none | current | file:filename_all(),
                  encoding => none | epp:source_encoding(),
                  _ => _}.
-type result() :: changed | unchanged.

-export_type([opts/0, result/0]).

-callback format(erl_syntax:forms(), [pos_integer()], opts()) -> string().

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format(file:filename_all(), module(), opts()) -> result().
format(File, Formatter, Opts) ->
    AST = get_ast(File),
    Comments = get_comments(File),
    FileOpts = apply_per_file_opts(File, Opts),
    {ok, Original} = file:read_file(File),
    Formatted = format(File, AST, Formatter, Comments, FileOpts),
    Result = case Formatted of
            Original ->
                unchanged;
            _ ->
                changed
            end,
    OutputDir = maps:get(output_dir, FileOpts),
    case maybe_save_file(OutputDir, File, Formatted) of
        none ->
            Result;
        NewFile ->
            check_quick_ast(File, NewFile, Result)
    end.

check_quick_ast(File1, File2, Status) ->
    case get_quick_ast(File1) == get_quick_ast(File2) of
        true ->
            Status;
        false ->
            erlang:error({modified_ast, File1, File2})
    end.

get_ast(File) ->
    case ktn_dodger:parse_file(File, [{scan_opts, [text]}]) of
      {ok, AST} ->
          case [Error || {error, Error} <- AST] of
            [] ->
                AST;
            [Error | _] ->
                erlang:error({cant_parse, File, Error})
          end;
      {error, OpenError} ->
          erlang:error({cant_parse, File, OpenError})
    end.

get_quick_ast(File) ->
    case ktn_dodger:quick_parse_file(File) of
      {ok, AST} ->
          remove_line_numbers(AST);
      {error, OpenError} ->
          erlang:error({cant_parse, File, OpenError})
    end.

%% @doc Removes line numbers from ASTs to allow for "semantic" comparison
remove_line_numbers(AST) when is_list(AST) ->
    lists:map(fun remove_line_numbers/1, AST);
remove_line_numbers(AST) when is_tuple(AST) ->
    [Type, _Line | Rest] = tuple_to_list(AST),
    case Type of
      error ->
          AST;
      Type ->
          list_to_tuple([Type, no | remove_line_numbers(Rest)])
    end;
remove_line_numbers(AST) ->
    AST.

get_comments(File) ->
    erl_comment_scan:file(File).

%% @doc We need to use quick_parse_file/1 here because the returned format
%%      is much more manageable than the one returned by parse_file/1
apply_per_file_opts(File, Opts) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    FileOpts = [Opt || {attribute, _, format, Opt} <- AST],
    case lists:member(ignore, FileOpts) of
        true ->
            maps:put(ignore, true, Opts);
        false ->
            MergeF = fun (Map, Acc) -> maps:merge(Acc, Map) end,
            lists:foldl(MergeF, Opts, FileOpts)
    end.

format(File, _AST, _Formatter, _Comments, #{ignore := true}) ->
    {ok, Contents} = file:read_file(File),
    Contents;
format(File, AST, Formatter, Comments, Opts) ->
    WithComments = erl_recomment:recomment_forms(erl_syntax:form_list(AST), Comments),
    Formatted = Formatter:format(WithComments, empty_lines(File), Opts),
    insert_last_line(iolist_to_binary(Formatted)).

empty_lines(File) ->
    {ok, Data} = file:read_file(File),
    List = binary:split(Data, [<<"\n">>], [global, trim]),
    {ok, NonEmptyLineRe} = re:compile("\\S"),
    {Res, _} = lists:foldl(fun (Line, {EmptyLines, N}) ->
                                   case re:run(Line, NonEmptyLineRe) of
                                     {match, _} ->
                                         {EmptyLines, N + 1};
                                     nomatch ->
                                         {[N | EmptyLines], N + 1}
                                   end
                           end,
                           {[], 1},
                           List),
    lists:reverse(Res).

insert_last_line(Formatted) ->
    {ok, Re} = re:compile("[\n]+$"),
    case re:run(Formatted, Re) of
      {match, _} ->
          re:replace(Formatted, Re, "\n", [{return, binary}]);
      nomatch ->
          <<Formatted/binary, "\n">>
    end.

maybe_save_file(none, _File, _Formatted) ->
    none;
maybe_save_file(current, File, Formatted) ->
    ok = file:write_file(File, Formatted),
    File;
maybe_save_file(OutputDir, File, Formatted) ->
    OutFile = filename:join(filename:absname(OutputDir), File),
    ok = filelib:ensure_dir(OutFile),
    ok = file:write_file(OutFile, Formatted),
    OutFile.
