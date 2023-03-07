%% @doc Rebar3 Pretty Printing of abstract Erlang syntax trees, following our own preferred style.
%% @reference Check
%%  <a target="_blank" href="https://github.com/AdRoll/rebar3_format#configuration">README.md</a>
%%  for more information on the available options.
-module(default_formatter).

-behaviour(rebar3_formatter).
-behaviour(rebar3_ast_formatter).

%% Allow erl_syntax:syntaxTree/0 type spec
-elvis([{elvis_style, atom_naming_convention, #{regex => "^([a-zA-Z][a-z0-9]*_?)*$"}}]).

%% 'maybe' and 'else', among others
-format #{unquote_atoms => false}.

%% erl_syntax functions that only appear in OTP25
-dialyzer([no_missing_calls]).

-export([init/2, format_file/3, format/3]).

-define(PADDING, 2).
-define(PAPER, 100).
-define(RIBBON, 90).
-define(BREAK_INDENT, 4).

-type clause_t() ::
    case_expr |
    simple_fun_expr |
    fun_expr |
    if_expr |
    maybe_expr |
    receive_expr |
    try_expr |
    {function, prettypr:document()} |
    spec.
-type inlining() :: all | none | {when_over, pos_integer()} | {when_under, pos_integer()}.

-record(ctxt,
        {prec = 0 :: integer(),
         sub_indent = ?BREAK_INDENT :: non_neg_integer(),
         break_indent = ?BREAK_INDENT :: non_neg_integer(),
         clause = undefined :: clause_t() | undefined,
         paper = ?PAPER :: integer(),
         ribbon = ?RIBBON :: integer(),
         inline_items = {when_over, 25} :: inlining(),
         inline_fields = {when_under, 3} :: inlining(),
         inline_attributes = all :: inlining(),
         within_disjunction = false :: boolean(),
         force_indentation = false :: boolean(),
         force_arity_qualifiers = false :: boolean(),
         inline_simple_funs = true :: boolean(),
         inline_clause_bodies = false :: boolean(),
         inline_qualified_function_composition = false :: boolean(),
         inline_expressions = false :: boolean(),
         spaces_around_arguments = false :: boolean(),
         spaces_around_fields = false :: boolean(),
         sort_arity_qualifiers = false :: boolean(),
         sort_arity_qualifiers_match = false :: boolean(),
         unquote_atoms = true :: boolean(),
         truncate_strings = false :: boolean(),
         parenthesize_infix_operations = false :: boolean(),
         empty_lines = [] :: [pos_integer()],
         encoding = epp:default_encoding() :: epp:source_encoding()}).

set_prec(Ctxt, Prec) ->
    Ctxt#ctxt{prec = Prec}.    % used internally

reset_prec(Ctxt) ->
    set_prec(Ctxt, 0).    % used internally

%% =====================================================================
%% @doc Pretty-prints/formats an abstract Erlang syntax tree as text in the style of NextRoll.
%%
%% @see erl_syntax
%% @see format/1
%% @see layout/2
-spec format(erl_syntax:syntaxTree(), [pos_integer()], rebar3_formatter:opts()) ->
                string().
format(Node, EmptyLines, Options) ->
    W = maps:get(paper, Options, ?PAPER),
    L = maps:get(ribbon, Options, ?RIBBON),
    E = maps:get(encoding, Options, utf8),
    FinalEmptyLines =
        case maps:get(preserve_empty_lines, Options, true) of
            true ->
                EmptyLines;
            false ->
                []
        end,
    PreFormatted = prettypr:format(layout(Node, FinalEmptyLines, Options), W, L),
    Formatted = remove_tabs(unicode:characters_to_binary(PreFormatted, E)),
    remove_trailing_spaces(Formatted).

%% @doc Initialize the formatter and generate a state that will be passed in when
%%      calling other callbacks.
-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) -> nostate.
init(_, _) ->
    nostate.

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format_file(file:filename_all(), nostate, rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, nostate, Opts) ->
    rebar3_ast_formatter:format(File, ?MODULE, Opts).

remove_tabs(Formatted) ->
    case re:replace(Formatted, <<"(\n *)\t">>, <<"\\1        ">>, [global, {return, binary}])
    of
        Formatted ->
            Formatted;
        Replaced ->
            remove_tabs(Replaced)
    end.

remove_trailing_spaces(Formatted) ->
    re:replace(Formatted, <<" +\n">>, <<"\n">>, [global, {return, list}]).

%% =====================================================================
%% @doc Creates an abstract document layout for a syntax tree. The
%% result represents a set of possible layouts (cf. module `prettypr').
%% For information on the options, see {@link format/2}; note, however,
%% that the `paper' and `ribbon' options are ignored by this function.
%%
%% This function provides a low-level interface to the pretty printer,
%% returning a flexible representation of possible layouts, independent
%% of the paper width eventually to be used for formatting. This can be
%% included as part of another document and/or further processed
%% directly by the functions in the `prettypr' module (see `format/2'
%% for details).
%%
%% @see prettypr
%% @see format/2
-spec layout(erl_syntax:syntaxTree(), [pos_integer()], rebar3_formatter:opts()) ->
                prettypr:document().
layout(Node, EmptyLines, Options) ->
    BreakIndent = maps:get(break_indent, Options, ?BREAK_INDENT),
    lay(Node,
        #ctxt{paper = maps:get(paper, Options, ?PAPER),
              ribbon = maps:get(ribbon, Options, ?RIBBON),
              break_indent = BreakIndent,
              sub_indent = maps:get(sub_indent, Options, BreakIndent),
              inline_simple_funs = maps:get(inline_simple_funs, Options, true),
              inline_clause_bodies = maps:get(inline_clause_bodies, Options, false),
              inline_qualified_function_composition =
                  maps:get(inline_qualified_function_composition, Options, false),
              inline_expressions = maps:get(inline_expressions, Options, false),
              inline_items = maps:get(inline_items, Options, {when_over, 25}),
              inline_fields = maps:get(inline_fields, Options, {when_under, 3}),
              inline_attributes = maps:get(inline_attributes, Options, all),
              parenthesize_infix_operations =
                  maps:get(parenthesize_infix_operations, Options, false),
              unquote_atoms = maps:get(unquote_atoms, Options, true),
              truncate_strings = maps:get(truncate_strings, Options, false),
              spaces_around_arguments = maps:get(spaces_around_arguments, Options, false),
              spaces_around_fields = maps:get(spaces_around_fields, Options, false),
              sort_arity_qualifiers = maps:get(sort_arity_qualifiers, Options, false),
              empty_lines = EmptyLines,
              encoding = maps:get(encoding, Options, epp:default_encoding())}).

lay(Node, Ctxt) ->
    case erl_syntax:has_comments(Node) of
        true ->
            D1 = lay_no_comments(Node, Ctxt),
            D2 = lay_postcomments(erl_syntax:get_postcomments(Node), D1),
            lay_precomments(erl_syntax:get_precomments(Node), D2);
        false ->
            lay_no_comments(Node, Ctxt)
    end.

%% For pre-comments, all padding is ignored.
lay_precomments([], D) ->
    D;
lay_precomments(Cs, D) ->
    prettypr:above(
        prettypr:floating(
            prettypr:break(stack_comments(Cs, false)), -1, -1),
        D).

%% For postcomments, individual padding is added.
lay_postcomments([], D) ->
    D;
lay_postcomments(Cs, D) ->
    prettypr:beside(D,
                    prettypr:floating(
                        prettypr:break(stack_comments(Cs, true)), 1, 0)).

%% Format (including padding, if `Pad' is `true', otherwise not)
%% and stack the listed comments above each other.
stack_comments([C | Cs], Pad) ->
    D = stack_comment_lines(erl_syntax:comment_text(C)),
    D1 = case Pad of
             true ->
                 P = case erl_syntax:comment_padding(C) of
                         none ->
                             ?PADDING;
                         P1 ->
                             P1
                     end,
                 prettypr:beside(
                     prettypr:text(spaces(P)), D);
             false ->
                 D
         end,
    case Cs of
        [] ->
            D1; % done
        _ ->
            prettypr:above(D1, stack_comments(Cs, Pad))
    end.

%% Stack lines of text above each other and prefix each string in
%% the list with a single `%' character.
stack_comment_lines([S | Ss]) ->
    D = prettypr:text(add_comment_prefix(S)),
    case Ss of
        [] ->
            D;
        _ ->
            prettypr:above(D, stack_comment_lines(Ss))
    end;
stack_comment_lines([]) ->
    prettypr:empty().

add_comment_prefix(S) ->
    [$% | S].

%% This part ignores annotations and comments:
lay_no_comments(Node, Ctxt) ->
    case erl_syntax:type(Node) of
        %% We list literals and other common cases first.
        variable ->
            prettypr:text(
                erl_syntax:variable_literal(Node));
        atom ->
            prettypr:text(tidy_atom(Node, Ctxt));
        integer ->
            prettypr:text(tidy_integer(Node));
        float ->
            prettypr:text(tidy_float(Node));
        char ->
            prettypr:text(tidy_char(Node, Ctxt#ctxt.encoding));
        string ->
            lay_string(Node, Ctxt);
        nil ->
            prettypr:text("[]");
        tuple ->
            case maybe_convert_to_qualifier(Node, Ctxt) of
                Node -> % it didn't change
                    Es = lay_items(erl_syntax:tuple_elements(Node), reset_prec(Ctxt), fun lay/2),
                    prettypr:beside(lay_text_float("{"), prettypr:beside(Es, lay_text_float("}")));
                NewNode ->
                    lay_no_comments(NewNode, Ctxt)
            end;
        list ->
            Ctxt1 = reset_prec(Ctxt),
            Node1 = erl_syntax:compact_list(Node),
            D1 = lay_items(erl_syntax:list_prefix(Node1), Ctxt1, fun lay/2),
            D = case erl_syntax:list_suffix(Node1) of
                    none ->
                        prettypr:beside(D1, lay_text_float("]"));
                    S ->
                        prettypr:follow(D1,
                                        prettypr:beside(lay_text_float("| "),
                                                        prettypr:beside(lay(S, Ctxt1),
                                                                        lay_text_float("]"))))
                end,
            prettypr:beside(lay_text_float("["), D);
        operator ->
            lay_text_float(erl_syntax:operator_literal(Node));
        infix_expr ->
            {Prec, Docs} = infix_expr_docs(Node, Ctxt),
            D = prettypr:sep(adjust_infix_expr_pars(Docs, Ctxt)),
            maybe_parentheses(D, Prec, Ctxt);
        prefix_expr ->
            Operator = erl_syntax:prefix_expr_operator(Node),
            {{Prec, PrecR}, Name} =
                case erl_syntax:type(Operator) of
                    operator ->
                        N = erl_syntax:operator_name(Operator),
                        {erl_parse:preop_prec(N), N};
                    _ ->
                        {{0, 0}, any}
                end,
            D1 = lay(Operator, reset_prec(Ctxt)),
            D2 = lay(erl_syntax:prefix_expr_argument(Node), set_prec(Ctxt, PrecR)),
            D3 = case Name of
                     '+' ->
                         prettypr:beside(D1, D2);
                     '-' ->
                         prettypr:beside(D1, D2);
                     _ ->
                         prettypr:par([D1, D2], Ctxt#ctxt.break_indent)
                 end,
            maybe_parentheses(D3, Prec, Ctxt);
        application ->
            lay_application(erl_syntax:application_operator(Node),
                            erl_syntax:application_arguments(Node),
                            Ctxt#ctxt.spaces_around_arguments,
                            Ctxt);
        match_expr ->
            {PrecL, Prec, PrecR} = erl_parse:inop_prec('='),
            Pattern = erl_syntax:match_expr_pattern(Node),
            D1 = lay(Pattern, set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:match_expr_body(Node), set_prec(Ctxt, PrecR)),
            D3 = lay_match_expression(" =", Pattern, D1, D2, Ctxt),
            maybe_parentheses(D3, Prec, Ctxt);
        underscore ->
            prettypr:text("_");
        clause ->
            %% The style used for a clause depends on its context
            Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
            D1 = lay_items(erl_syntax:clause_patterns(Node), Ctxt1, fun lay/2),
            D2 = case erl_syntax:clause_guard(Node) of
                     none ->
                         none;
                     G ->
                         lay(G, Ctxt1)
                 end,
            D3 = lay_clause_expressions(erl_syntax:clause_body(Node), Ctxt1, fun lay/2),
            case Ctxt#ctxt.clause of
                fun_expr ->
                    make_fun_clause(D1, D2, D3, Ctxt);
                simple_fun_expr ->
                    make_simple_fun_clause(D1, D2, D3, Ctxt);
                {function, N} ->
                    make_fun_clause(N, D1, D2, D3, Ctxt);
                if_expr ->
                    make_if_clause(D2, D3, Ctxt);
                case_expr ->
                    make_case_clause(D1, D2, D3, Ctxt);
                maybe_expr ->
                    make_case_clause(D1, D2, D3, Ctxt);
                receive_expr ->
                    make_case_clause(D1, D2, D3, Ctxt);
                try_expr ->
                    make_case_clause(D1, D2, D3, Ctxt);
                undefined ->
                    %% If a clause is formatted out of context, we
                    %% use a "fun-expression" clause style.
                    make_fun_clause(D1, D2, D3, Ctxt)
            end;
        function ->
            %% Comments on the name itself will be repeated for each
            %% clause, but that seems to be the best way to handle it.
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:function_name(Node), Ctxt1),
            D2 = lay_clauses(erl_syntax:function_clauses(Node), {function, D1}, Ctxt1),
            prettypr:beside(D2, lay_text_float("."));
        case_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:case_expr_argument(Node), Ctxt1),
            D2 = lay_clauses(erl_syntax:case_expr_clauses(Node), case_expr, Ctxt1),
            prettypr:sep([prettypr:par([prettypr:follow(
                                            prettypr:text("case"), D1, Ctxt1#ctxt.break_indent),
                                        prettypr:text("of")]),
                          prettypr:nest(Ctxt1#ctxt.break_indent, D2),
                          prettypr:text("end")]);
        if_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D = lay_clauses(erl_syntax:if_expr_clauses(Node), if_expr, Ctxt1),
            prettypr:sep([prettypr:follow(
                              prettypr:text("if"), D, Ctxt1#ctxt.break_indent),
                          prettypr:text("end")]);
        fun_expr ->
            Ctxt1 = reset_prec(Ctxt),
            case erl_syntax:fun_expr_clauses(Node) of
                [Clause] -> % Just one clause
                    % We force inlining here, to prevent fun() -> x end to use 3 lines
                    % if inline_simple_funs is true. Otherwise treat them as the rest
                    % of the code
                    DClause = lay(Clause, Ctxt1#ctxt{clause = simple_fun_expr}),
                    prettypr:sep([prettypr:beside(
                                      prettypr:text("fun"), DClause),
                                  prettypr:text("end")]);
                Clauses ->
                    lay_fun_sep(lay_clauses(Clauses, fun_expr, Ctxt1), Ctxt1)
            end;
        named_fun_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:named_fun_expr_name(Node), Ctxt1),
            Clauses = lay_clauses(erl_syntax:named_fun_expr_clauses(Node), {function, D1}, Ctxt1),
            lay_fun_sep(Clauses, Ctxt1);
        module_qualifier ->
            {PrecL, _Prec, PrecR} = erl_parse:inop_prec(':'),
            D1 = lay(erl_syntax:module_qualifier_argument(Node), set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:module_qualifier_body(Node), set_prec(Ctxt, PrecR)),
            prettypr:beside(D1,
                            prettypr:beside(
                                prettypr:text(":"), D2));
        %%
        %% The rest is in alphabetical order (except map and types)
        %%
        arity_qualifier ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:arity_qualifier_body(Node), Ctxt1),
            D2 = lay(erl_syntax:arity_qualifier_argument(Node), Ctxt1),
            prettypr:beside(D1,
                            prettypr:beside(
                                prettypr:text("/"), D2));
        attribute ->
            %% The attribute name and arguments are formatted similar to
            %% a function call, but prefixed with a "-" and followed by
            %% a period. If the arguments is `none', we only output the
            %% attribute name, without following parentheses.
            Ctxt1 = reset_prec(Ctxt),
            Tag = attribute_name(Node),

            %% NOTE: The preceding $- must be part of the name of the attribute.
            %%       That's because we want indentation to start counting from
            %%       that character on, and not from the first character on the
            %%       attribute name.
            N = erl_syntax:variable([$- | atom_to_list(Tag)]),
            D = case {Tag, erl_syntax:attribute_arguments(Node)} of
                    {Tag, [SpecTuple]} when Tag =:= spec; Tag =:= callback ->
                        [FuncName, FuncTypes] = erl_syntax:tuple_elements(SpecTuple),
                        Name = get_func_node(FuncName),
                        Types = concrete_dodging_macros(FuncTypes),
                        case Types of
                            [Type] ->
                                lay_simple_spec(prettypr:follow(lay(N, Ctxt1),
                                                                lay(Name, Ctxt1),
                                                                Ctxt1#ctxt.break_indent),
                                                Type,
                                                Ctxt1);
                            Types ->
                                D1 = lay_clauses(Types, spec, Ctxt1),
                                prettypr:beside(
                                    prettypr:follow(lay(N, Ctxt1),
                                                    lay(Name, Ctxt1),
                                                    Ctxt1#ctxt.break_indent),
                                    D1)
                        end;
                    {Tag, [TypeTuple]} when Tag =:= type; Tag =:= opaque ->
                        [Name, Type, Elements] = erl_syntax:tuple_elements(TypeTuple),
                        As = concrete_dodging_macros(Elements),
                        D1 = prettypr:follow(lay(N, Ctxt1), lay_application(Name, As, Ctxt1)),
                        D2 = lay(concrete_dodging_macros(Type), Ctxt1),
                        lay_double_colon(D1, D2, Ctxt1);
                    {Tag, [FuncNames]} when Tag =:= export_type; Tag =:= optional_callbacks ->
                        As0 = unfold_function_names(FuncNames),
                        Ctxt2 = Ctxt1#ctxt{sort_arity_qualifiers_match = true},
                        As = maybe_sort_arity_qualifiers(As0, Ctxt2),

                        %% We force inlining of list items and use inline_attributes to
                        %% format the list of functions
                        Ctxt3 =
                            Ctxt2#ctxt{force_indentation = true,
                                       inline_items = Ctxt1#ctxt.inline_attributes},
                        prettypr:beside(lay(N, Ctxt2),
                                        prettypr:beside(
                                            prettypr:text("("),
                                            prettypr:beside(lay(As, Ctxt3), lay_text_float(")"))));
                    {on_load, [FuncName]} ->
                        As = unfold_function_name(FuncName),
                        prettypr:beside(lay(N, Ctxt1),
                                        prettypr:beside(lay_text_float(" "), lay(As, Ctxt1)));
                    {format, [Opts]} -> % Always a single map
                        D1 = lay(N, Ctxt),
                        As = lay(Opts, Ctxt),
                        prettypr:beside(D1, prettypr:beside(lay_text_float(" "), As));
                    {export, Args} ->
                        %% We force inlining of list items and use inline_attributes to
                        %% format the lists within these attributes
                        Ctxt2 =
                            Ctxt1#ctxt{force_indentation = true,
                                       sort_arity_qualifiers_match = true,
                                       inline_items = Ctxt1#ctxt.inline_attributes},
                        lay_application(N, Args, Ctxt2);
                    {Tag, Args}
                        when Tag =:= dialyzer;
                             Tag =:= mixin;
                             Tag =:= ignore_xref;
                             Tag =:= compile ->
                        %% We need to convert tuples with 2 elements to arity qualifiers here
                        %% because the parser doesn't recognize them as such.
                        Ctxt2 = Ctxt1#ctxt{force_arity_qualifiers = true},
                        lay_application(N, Args, Ctxt2);
                    {_, none} ->
                        lay(N, Ctxt1);
                    {Tag, [Arg]} ->
                        case Tag /= module
                             andalso erl_syntax:type(Arg) == atom
                             andalso erl_syntax:concrete(Arg) == ignore
                        of
                            true ->
                                % handle stuff like -elvis ignore.
                                D1 = lay(N, Ctxt),
                                As = lay(Arg, Ctxt),
                                prettypr:beside(D1, prettypr:beside(lay_text_float(" "), As));
                            false ->
                                lay_application(N, [Arg], Ctxt1)
                        end;
                    {_, Args} ->
                        lay_application(N, Args, Ctxt1)
                end,
            prettypr:beside(D, lay_text_float("."));
        binary ->
            Ctxt1 = reset_prec(Ctxt),
            Es = lay_items(erl_syntax:binary_fields(Node), Ctxt1, fun lay/2),
            prettypr:beside(lay_text_float("<<"), prettypr:beside(Es, lay_text_float(">>")));
        binary_field ->
            Ctxt1 = set_prec(Ctxt, erl_parse:max_prec()),
            D1 = lay(erl_syntax:binary_field_body(Node), Ctxt1),
            D2 = case erl_syntax:binary_field_types(Node) of
                     [] ->
                         prettypr:empty();
                     Ts ->
                         prettypr:beside(lay_text_float("/"), lay_bit_types(Ts, Ctxt1))
                 end,
            prettypr:beside(D1, D2);
        block_expr ->
            Ctxt1 = reset_prec(Ctxt),
            Es = lay_clause_expressions(erl_syntax:block_expr_body(Node), Ctxt1, fun lay/2),
            prettypr:sep([prettypr:text("begin"),
                          prettypr:nest(Ctxt1#ctxt.break_indent, Es),
                          prettypr:text("end")]);
        catch_expr ->
            {_, PrecR} = erl_parse:preop_prec('catch'),
            D = lay(erl_syntax:catch_expr_body(Node), set_prec(Ctxt, PrecR)),
            D1 = prettypr:follow(
                     prettypr:text("catch"), D, Ctxt#ctxt.break_indent),
            maybe_parentheses(D1, 0, Ctxt);
        class_qualifier ->
            Ctxt1 = set_prec(Ctxt, erl_parse:max_prec()),
            D1 = lay(erl_syntax:class_qualifier_argument(Node), Ctxt1),
            D2 = lay(erl_syntax:class_qualifier_body(Node), Ctxt1),
            Stacktrace = erl_syntax:class_qualifier_stacktrace(Node),
            case erl_syntax:variable_name(Stacktrace) of
                '_' ->
                    prettypr:beside(D1,
                                    prettypr:beside(
                                        prettypr:text(":"), D2));
                _ ->
                    D3 = lay(Stacktrace, Ctxt1),
                    prettypr:beside(D1,
                                    prettypr:beside(
                                        prettypr:beside(
                                            prettypr:text(":"), D2),
                                        prettypr:beside(
                                            prettypr:text(":"), D3)))
            end;
        comment ->
            D = stack_comment_lines(erl_syntax:comment_text(Node)),

            %% Default padding for standalone comments is empty.
            case erl_syntax:comment_padding(Node) of
                none ->
                    prettypr:floating(
                        prettypr:break(D));
                P ->
                    prettypr:floating(
                        prettypr:break(
                            prettypr:beside(
                                prettypr:text(spaces(P)), D)))
            end;
        conjunction ->
            ExprDocs =
                seq(erl_syntax:conjunction_body(Node),
                    lay_text_float(","),
                    reset_prec(Ctxt),
                    fun lay/2),
            case Ctxt#ctxt.within_disjunction of
                true ->
                    %% We're in a clause guard, since all clause guards are lists
                    %% of (at least one) disjunction items.
                    prettypr:par(ExprDocs);
                false ->
                    %% We're in a "fake" conjunction, i.e. the 'when' piece of a
                    %% constrained_function_type and we want those types laid out
                    %% one in each row.
                    vertical(ExprDocs)
            end;
        disjunction ->
            %% For clarity, we don't paragraph-format disjunctions;
            %% only conjunctions within disjunctions (see above).
            prettypr:sep(seq(erl_syntax:disjunction_body(Node),
                             lay_text_float(";"),
                             reset_prec(Ctxt#ctxt{within_disjunction = true}),
                             fun lay/2));
        error_marker ->
            E = erl_syntax:error_marker_info(Node),
            prettypr:beside(
                prettypr:text("** "),
                prettypr:beside(lay_error_info(E, reset_prec(Ctxt)), prettypr:text(" **")));
        eof_marker ->
            prettypr:empty();
        form_list ->
            Es = seq(erl_syntax:form_list_elements(Node), none, reset_prec(Ctxt), fun lay/2),
            AddEmptyLines = empty_lines_to_add(erl_syntax:form_list_elements(Node), Ctxt),
            vertical_sep(lists:zip(Es, AddEmptyLines));
        generator ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:generator_pattern(Node), Ctxt1),
            D2 = lay(erl_syntax:generator_body(Node), Ctxt1),
            prettypr:par([D1,
                          prettypr:beside(
                              prettypr:text("<- "), D2)],
                         Ctxt1#ctxt.break_indent);
        binary_generator ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:binary_generator_pattern(Node), Ctxt1),
            D2 = lay(erl_syntax:binary_generator_body(Node), Ctxt1),
            prettypr:par([D1,
                          prettypr:beside(
                              prettypr:text("<= "), D2)],
                         Ctxt1#ctxt.break_indent);
        implicit_fun ->
            D = lay(erl_syntax:implicit_fun_name(Node), reset_prec(Ctxt)),
            prettypr:beside(lay_text_float("fun "), D);
        list_comp ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:list_comp_template(Node), Ctxt1),
            D2 = lay_items(erl_syntax:list_comp_body(Node), Ctxt1, fun lay/2),
            D3 = prettypr:beside(lay_text_float("|| "), prettypr:beside(D2, lay_text_float("]"))),
            prettypr:beside(lay_text_float("["), prettypr:par([D1, D3]));
        binary_comp ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:binary_comp_template(Node), Ctxt1),
            D2 = lay_items(erl_syntax:binary_comp_body(Node), Ctxt1, fun lay/2),
            D3 = prettypr:beside(lay_text_float("|| "), prettypr:beside(D2, lay_text_float(" >>"))),
            prettypr:beside(lay_text_float("<< "), prettypr:par([D1, D3]));
        macro ->
            %% This is formatted similar to a normal function call or a variable
            N = macro_name(Node, variable),
            case erl_syntax:macro_arguments(Node) of
                none ->
                    lay(N, Ctxt);
                Args ->
                    lay_application(N, Args, Ctxt)
            end;
        parentheses ->
            D = lay(erl_syntax:parentheses_body(Node), reset_prec(Ctxt)),
            lay_parentheses(D);
        maybe_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D0 = lay_clause_expressions(erl_syntax:maybe_expr_body(Node), Ctxt1, fun lay/2),
            D1 = vertical([prettypr:text("maybe"), prettypr:nest(Ctxt1#ctxt.break_indent, D0)]),
            case erl_syntax:maybe_expr_else(Node) of
                none ->
                    prettypr:par([D1, prettypr:text("end")]);
                ElseNode ->
                    ElseCs = erl_syntax:else_expr_clauses(ElseNode),
                    D3 = lay_clauses(ElseCs, maybe_expr, Ctxt1),
                    prettypr:sep([prettypr:par([D1, prettypr:text("else")]),
                                  prettypr:nest(Ctxt1#ctxt.break_indent, D3),
                                  prettypr:text("end")])
            end;
        maybe_match_expr ->
            {PrecL, Prec, PrecR} = erl_parse:inop_prec('='),
            Pattern = erl_syntax:maybe_match_expr_pattern(Node),
            D1 = lay(Pattern, set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:maybe_match_expr_body(Node), set_prec(Ctxt, PrecR)),
            D3 = lay_match_expression(" ?=", Pattern, D1, D2, Ctxt),
            maybe_parentheses(D3, Prec, Ctxt);
        receive_expr ->
            Ctxt1 = reset_prec(Ctxt),
            case {erl_syntax:receive_expr_clauses(Node), erl_syntax:receive_expr_timeout(Node)} of
                {Clauses, none} ->
                    D1 = lay_clauses(Clauses, receive_expr, Ctxt1),
                    prettypr:sep([prettypr:text("receive"),
                                  prettypr:nest(Ctxt1#ctxt.break_indent, D1),
                                  prettypr:text("end")]);
                {[], T} ->
                    D1 = prettypr:beside(lay_text_float("receive after "), lay(T, Ctxt1)),
                    D2 = lay_clause_expressions(erl_syntax:receive_expr_action(Node),
                                                Ctxt1,
                                                fun lay/2),
                    D3 = append_clause_body(D2, D1, Ctxt1),
                    prettypr:sep([D3, prettypr:text("end")]);
                {Clauses, T} ->
                    D1 = lay_clauses(Clauses, receive_expr, Ctxt1),
                    D2 = prettypr:beside(lay_text_float("after "), lay(T, Ctxt1)),
                    D3 = lay_clause_expressions(erl_syntax:receive_expr_action(Node),
                                                Ctxt1,
                                                fun lay/2),
                    D4 = append_clause_body(D3, D2, Ctxt1),
                    prettypr:sep([prettypr:text("receive"),
                                  prettypr:nest(Ctxt1#ctxt.break_indent, D1),
                                  D4,
                                  prettypr:text("end")])
            end;
        record_access ->
            {PrecL, Prec, PrecR} = erl_parse:inop_prec('#'),
            Argument = erl_syntax:record_access_argument(Node),
            D1 = case erl_syntax:type(Argument) of
                     record_access ->
                         lay(Argument, set_prec(Ctxt, Prec)); % A#b.c#d.e#f.g is valid Erlang
                     _ ->
                         lay(Argument, set_prec(Ctxt, PrecL))
                 end,
            D2 = prettypr:beside(lay_text_float("."),
                                 lay(erl_syntax:record_access_field(Node), set_prec(Ctxt, PrecR))),
            T = erl_syntax:record_access_type(Node),
            D3 = prettypr:beside(
                     prettypr:beside(lay_text_float("#"), lay(T, reset_prec(Ctxt))), D2),
            maybe_parentheses(prettypr:beside(D1, D3), Prec, Ctxt);
        record_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = prettypr:beside(
                     prettypr:beside(lay_text_float("#"),
                                     lay(erl_syntax:record_expr_type(Node), Ctxt1)),
                     prettypr:text("{")),
            D2 = lay_fields(D1, erl_syntax:record_expr_fields(Node), Ctxt1, fun lay/2),
            Arg = erl_syntax:record_expr_argument(Node),
            lay_expr_argument(Arg, D2, Ctxt);
        record_field ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:record_field_name(Node), Ctxt1),
            case erl_syntax:record_field_value(Node) of
                none ->
                    D1;
                V ->
                    prettypr:par([D1, lay_text_float("="), lay(V, Ctxt1)], Ctxt1#ctxt.break_indent)
            end;
        record_index_expr ->
            {Prec, PrecR} = erl_parse:preop_prec('#'),
            D1 = lay(erl_syntax:record_index_expr_type(Node), reset_prec(Ctxt)),
            D2 = lay(erl_syntax:record_index_expr_field(Node), set_prec(Ctxt, PrecR)),
            D3 = prettypr:beside(
                     prettypr:beside(lay_text_float("#"), D1),
                     prettypr:beside(lay_text_float("."), D2)),
            maybe_parentheses(D3, Prec, Ctxt);
        map_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay_fields(prettypr:text("#{"),
                            erl_syntax:map_expr_fields(Node),
                            Ctxt1,
                            fun lay/2),
            Arg = erl_syntax:map_expr_argument(Node),
            lay_expr_argument(Arg, D1, Ctxt);
        map_field_assoc ->
            Name = erl_syntax:map_field_assoc_name(Node),
            Value = erl_syntax:map_field_assoc_value(Node),
            lay_type_assoc(Name, Value, Ctxt);
        map_field_exact ->
            Name = erl_syntax:map_field_exact_name(Node),
            Value = erl_syntax:map_field_exact_value(Node),
            lay_type_exact(Name, Value, Ctxt);
        size_qualifier ->
            Ctxt1 = set_prec(Ctxt, erl_parse:max_prec()),
            D1 = lay(erl_syntax:size_qualifier_body(Node), Ctxt1),
            Arg = erl_syntax:size_qualifier_argument(Node),
            D2 = lay(erl_syntax:size_qualifier_argument(Node), Ctxt1),
            D3 = case erl_syntax:type(Arg) of
                     macro ->
                         case erl_syntax:macro_arguments(Arg) of
                             none ->
                                 %% Something:(?MACRO) gets converted into
                                 %% Something:?MACRO since this formatter treats
                                 %% ?MACRO as a "literal" and sometimes it actually
                                 %% is not. We'll add parentheses here, just in case.
                                 lay_parentheses(D2);
                             _ ->
                                 D2
                         end;
                     _ ->
                         D2
                 end,

            prettypr:beside(D1,
                            prettypr:beside(
                                prettypr:text(":"), D3));
        text ->
            case erl_syntax:get_ann(Node) of
                [expression_dot] -> % see maybe_append/3
                    prettypr:empty();
                _ ->
                    prettypr:text(
                        erl_syntax:text_string(Node))
            end;
        typed_record_field ->
            {_, Prec, _} = erl_parse:type_inop_prec('::'),
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:typed_record_field_body(Node), Ctxt1),
            D2 = lay(erl_syntax:typed_record_field_type(Node), set_prec(Ctxt, Prec)),
            D3 = lay_double_colon(D1, D2, Ctxt1),
            maybe_parentheses(D3, Prec, Ctxt);
        try_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D0 = lay_clause_expressions(erl_syntax:try_expr_body(Node), Ctxt1, fun lay/2),
            D1 = case erl_syntax:try_expr_clauses(Node) of
                     [] ->
                         vertical([prettypr:text("try"),
                                   prettypr:nest(Ctxt1#ctxt.break_indent, D0)]);
                     _ ->
                         prettypr:follow(
                             prettypr:text("try"), D0, Ctxt1#ctxt.break_indent)
                 end,
            Es0 = [prettypr:text("end")],
            Es1 = case erl_syntax:try_expr_after(Node) of
                      [] ->
                          Es0;
                      As ->
                          D2 = lay_clause_expressions(As, Ctxt1, fun lay/2),
                          [prettypr:text("after"), prettypr:nest(Ctxt1#ctxt.break_indent, D2) | Es0]
                  end,
            Es2 = case erl_syntax:try_expr_handlers(Node) of
                      [] ->
                          Es1;
                      Hs ->
                          D3 = lay_clauses(Hs, try_expr, Ctxt1),
                          [prettypr:text("catch"), prettypr:nest(Ctxt1#ctxt.break_indent, D3) | Es1]
                  end,
            Es3 = case erl_syntax:try_expr_clauses(Node) of
                      [] ->
                          Es2;
                      Cs ->
                          D4 = lay_clauses(Cs, try_expr, Ctxt1),
                          [prettypr:text("of"), prettypr:nest(Ctxt1#ctxt.break_indent, D4) | Es2]
                  end,
            prettypr:sep([prettypr:par([D1, hd(Es3)]) | tl(Es3)]);
        warning_marker ->
            E = erl_syntax:warning_marker_info(Node),
            prettypr:beside(
                prettypr:text("%% WARNING: "), lay_error_info(E, reset_prec(Ctxt)));
        %%
        %% Types
        %%
        annotated_type ->
            {_, Prec, _} = erl_parse:type_inop_prec('::'),
            D1 = lay(erl_syntax:annotated_type_name(Node), reset_prec(Ctxt)),
            D2 = lay(erl_syntax:annotated_type_body(Node), set_prec(Ctxt, Prec)),
            D3 = lay_double_colon(D1, D2, Ctxt),
            maybe_parentheses(D3, Prec, Ctxt);
        type_application ->
            Name = erl_syntax:type_application_name(Node),
            Arguments = erl_syntax:type_application_arguments(Node),

            %% Prefer shorthand notation.
            try erl_syntax_lib:analyze_type_application(Node) of
                {nil, 0} ->
                    prettypr:text("[]");
                {list, 1} ->
                    [A] = Arguments,
                    D1 = lay(A, reset_prec(Ctxt)),
                    prettypr:beside(
                        prettypr:text("["), prettypr:beside(D1, prettypr:text("]")));
                {nonempty_list, 1} ->
                    [A] = Arguments,
                    D1 = lay(A, reset_prec(Ctxt)),
                    prettypr:beside(
                        prettypr:text("["), prettypr:beside(D1, prettypr:text(", ...]")));
                _ ->
                    lay_application(Name, Arguments, Ctxt)
            catch
                syntax_error -> % Damn macros!!
                    lay_application(Name, Arguments, Ctxt)
            end;
        bitstring_type ->
            Ctxt1 = set_prec(Ctxt, erl_parse:max_prec()),
            M = erl_syntax:bitstring_type_m(Node),
            N = erl_syntax:bitstring_type_n(Node),
            D1 = [prettypr:beside(
                      prettypr:text("_:"), lay(M, Ctxt1))
                  || erl_syntax:type(M) =/= integer orelse erl_syntax:integer_value(M) =/= 0],
            D2 = [prettypr:beside(
                      prettypr:text("_:_*"), lay(N, Ctxt1))
                  || erl_syntax:type(N) =/= integer orelse erl_syntax:integer_value(N) =/= 0],
            F = fun(D, _) -> D end,
            D = lay_items(D1 ++ D2, Ctxt1, F),
            prettypr:beside(lay_text_float("<<"), prettypr:beside(D, lay_text_float(">>")));
        fun_type ->
            prettypr:text("fun()");
        constrained_function_type ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:constrained_function_type_body(Node), Ctxt1),
            Ctxt2 = Ctxt1#ctxt{clause = undefined},
            D2 = lay(erl_syntax:constrained_function_type_argument(Node), Ctxt2),
            prettypr:par([D1, prettypr:beside(lay_text_float("when "), D2)],
                         Ctxt#ctxt.break_indent);
        function_type ->
            {Before, After} =
                case Ctxt#ctxt.clause of
                    spec ->
                        {"", ""};
                    _ ->
                        {"fun(", ")"}
                end,
            Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
            D1 = case erl_syntax:function_type_arguments(Node) of
                     any_arity ->
                         prettypr:text("(...)");
                     Arguments ->
                         As = lay_items(Arguments, Ctxt1, fun lay/2),
                         prettypr:beside(
                             prettypr:text("("), prettypr:beside(As, lay_text_float(")")))
                 end,
            D2 = lay(erl_syntax:function_type_return(Node), Ctxt1),
            prettypr:beside(lay_text_float(Before),
                            prettypr:sep([prettypr:beside(D1, lay_text_float(" ->")),
                                          prettypr:nest(Ctxt#ctxt.break_indent,
                                                        prettypr:beside(D2,
                                                                        lay_text_float(After)))]));
        constraint ->
            Name = erl_syntax:constraint_argument(Node),
            Args = erl_syntax:constraint_body(Node),
            case is_subtype(Name, Args) of
                true ->
                    [Var, Type] = Args,
                    {PrecL, Prec, PrecR} = erl_parse:type_inop_prec('::'),
                    D1 = lay(Var, set_prec(Ctxt, PrecL)),
                    D2 = lay(Type, set_prec(Ctxt, PrecR)),
                    D3 = lay_double_colon(D1, D2, Ctxt),
                    maybe_parentheses(D3, Prec, Ctxt);
                false ->
                    lay_application(Name, Args, Ctxt)
            end;
        map_type ->
            case erl_syntax:map_type_fields(Node) of
                any_size ->
                    prettypr:text("map()");
                Fs ->
                    Ctxt1 = reset_prec(Ctxt),
                    D = lay_fields(lay_text_float("#{"), Fs, Ctxt1, fun lay/2),
                    {Prec, _PrecR} = erl_parse:type_preop_prec('#'),
                    maybe_parentheses(D, Prec, Ctxt)
            end;
        map_type_assoc ->
            Name = erl_syntax:map_type_assoc_name(Node),
            Value = erl_syntax:map_type_assoc_value(Node),
            lay_type_assoc(Name, Value, Ctxt);
        map_type_exact ->
            Name = erl_syntax:map_type_exact_name(Node),
            Value = erl_syntax:map_type_exact_value(Node),
            lay_type_exact(Name, Value, Ctxt);
        integer_range_type ->
            {PrecL, Prec, PrecR} = erl_parse:type_inop_prec('..'),
            D1 = lay(erl_syntax:integer_range_type_low(Node), set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:integer_range_type_high(Node), set_prec(Ctxt, PrecR)),
            D3 = prettypr:beside(D1,
                                 prettypr:beside(
                                     prettypr:text(".."), D2)),
            maybe_parentheses(D3, Prec, Ctxt);
        record_type ->
            {Prec, _PrecR} = erl_parse:type_preop_prec('#'),
            D1 = prettypr:beside(
                     prettypr:beside(
                         prettypr:text("#"),
                         lay(erl_syntax:record_type_name(Node), reset_prec(Ctxt))),
                     prettypr:text("{")),
            D2 = lay_fields(D1, erl_syntax:record_type_fields(Node), reset_prec(Ctxt), fun lay/2),
            maybe_parentheses(D2, Prec, Ctxt);
        record_type_field ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:record_type_field_name(Node), Ctxt1),
            D2 = lay(erl_syntax:record_type_field_type(Node), Ctxt1),
            lay_double_colon(D1, D2, Ctxt1);
        tuple_type ->
            case erl_syntax:tuple_type_elements(Node) of
                any_size ->
                    prettypr:text("tuple()");
                Elements ->
                    Es = lay_items(Elements, reset_prec(Ctxt), fun lay/2),
                    prettypr:beside(lay_text_float("{"), prettypr:beside(Es, lay_text_float("}")))
            end;
        type_union ->
            {_, Prec, PrecR} = erl_parse:type_inop_prec('|'),
            Es = lay_items(erl_syntax:type_union_types(Node),
                           lay_text_float(" |"),
                           set_prec(Ctxt, PrecR),
                           fun lay/2),
            maybe_parentheses(Es, Prec, Ctxt);
        user_type_application ->
            lay_application(erl_syntax:user_type_application_name(Node),
                            erl_syntax:user_type_application_arguments(Node),
                            Ctxt)
    end.

attribute_name(Node) ->
    N = erl_syntax:attribute_name(Node),
    Name =
        case erl_syntax:type(N) of
            macro ->
                macro_name(N, atom);
            _ ->
                N
        end,
    erl_syntax:concrete(Name).

is_subtype(Name, [Var, _]) ->
    erl_syntax:is_atom(Name, is_subtype) andalso erl_syntax:type(Var) =:= variable;
is_subtype(_, _) ->
    false.

get_func_node(Node) ->
    case erl_syntax:type(Node) of
        tuple ->
            case erl_syntax:tuple_elements(Node) of
                [F0, _] ->
                    F0;
                [M0, F0, _] ->
                    erl_syntax:module_qualifier(M0, F0);
                _ ->
                    Node
            end;
        _ ->
            Node
    end.

unfold_function_names(Ns) ->
    erl_syntax_lib:map_subtrees(fun unfold_function_name/1, Ns).

unfold_function_name(Tuple) ->
    [Name, Arity] = erl_syntax:tuple_elements(Tuple),
    case erl_syntax:type(Name) of
        atom ->
            erl_syntax:arity_qualifier(Name, Arity);
        macro ->
            erl_syntax:arity_qualifier(macro_name(Name, variable), Arity)
    end.

macro_name(Macro) ->
    MacroName = erl_syntax:macro_name(Macro),
    case erl_syntax:type(MacroName) of
        atom ->
            erl_syntax:atom_name(MacroName);
        variable ->
            erl_syntax:variable_literal(MacroName)
    end.

macro_name(Node, Type) ->
    Source = erl_syntax:macro_name(Node),
    FullName = [$? | macro_name(Node)],
    Target =
        case Type of
            variable ->
                erl_syntax:variable(FullName);
            atom ->
                erl_syntax:atom(FullName)
        end,
    erl_syntax:copy_pos(Source, erl_syntax:copy_comments(Source, Target)).

concrete_dodging_macros(Nodes) ->
    undodge_macros(erl_syntax:concrete(dodge_macros(Nodes))).

%% Macros are not handled well.
dodge_macros(Type) ->
    erl_syntax_lib:map(fun dodge_macro/1, Type).

dodge_macro(T) ->
    case erl_syntax:type(T) of
        macro ->
            erl_syntax:atom(macro_name(T));
        _ ->
            T
    end.

undodge_macros(Type) when is_list(Type) ->
    lists:map(fun undodge_macros/1, Type);
undodge_macros(Type) ->
    erl_syntax_lib:map(fun undodge_macro/1, Type).

undodge_macro(T) ->
    case erl_syntax:type(T) of
        atom ->
            case get_node_text(T) of
                "?" ->
                    erl_syntax:macro(
                        erl_syntax:variable(
                            erl_syntax:atom_name(T)));
                _ ->
                    T
            end;
        _ ->
            T
    end.

%% @doc This is a particular edge case for those places where the parser
%%      treats func/1 as {func, 1}... particularly -dialyzer(...)
maybe_convert_to_qualifier(Node, #ctxt{force_arity_qualifiers = false}) ->
    Node;
maybe_convert_to_qualifier(Node, #ctxt{force_arity_qualifiers = true}) ->
    case erl_syntax:tuple_elements(Node) of
        [FuncName, Arity] ->
            case {erl_syntax:type(FuncName), erl_syntax:type(Arity)} of
                {atom, integer} ->
                    erl_syntax:arity_qualifier(FuncName, Arity);
                _ ->
                    Node
            end;
        _ ->
            Node
    end.

lay_nested_infix_expr(Node, #ctxt{parenthesize_infix_operations = false} = Ctxt) ->
    lay(Node, Ctxt);
lay_nested_infix_expr(Node, #ctxt{parenthesize_infix_operations = true} = Ctxt) ->
    D1 = lay(Node, Ctxt),
    case erl_syntax:type(Node) of
        infix_expr ->
            Operator = erl_syntax:infix_expr_operator(Node),
            Prec =
                case erl_syntax:type(Operator) of
                    operator ->
                        {_, P, _} =
                            erl_parse:inop_prec(
                                erl_syntax:operator_name(Operator)),
                        P;
                    _ ->
                        0
                end,

            % If we *should* add parentheses semantic-wise, lay/2 will take care
            % of that, thanks to maybe_parentheses/3
            case needs_parentheses(Prec, Ctxt) of
                true ->
                    D1;
                false ->
                    lay_parentheses(D1)
            end;
        _ ->
            D1
    end.

lay_text_float(Str) ->
    prettypr:floating(
        prettypr:text(Str)).

lay_fun_sep(Clauses, Ctxt) ->
    prettypr:sep([prettypr:follow(
                      prettypr:text("fun"), Clauses, Ctxt#ctxt.break_indent),
                  prettypr:text("end")]).

lay_expr_argument(none, D, Ctxt) ->
    {_, Prec, _} = erl_parse:inop_prec('#'),
    maybe_parentheses(D, Prec, Ctxt);
lay_expr_argument(Arg, D, Ctxt) ->
    {PrecL, Prec, _} = erl_parse:inop_prec('#'),
    D1 = prettypr:beside(lay(Arg, set_prec(Ctxt, PrecL)), D),
    maybe_parentheses(D1, Prec, Ctxt).

lay_parentheses(D) ->
    prettypr:beside(lay_text_float("("), prettypr:beside(D, lay_text_float(")"))).

maybe_parentheses(D, Prec, Ctxt) ->
    case needs_parentheses(Prec, Ctxt) of
        true ->
            lay_parentheses(D);
        false ->
            D
    end.

needs_parentheses(Prec, Ctxt) ->
    Ctxt#ctxt.prec > Prec.

lay_string(Node, Ctxt) ->
    S0 = erl_syntax:string_literal(Node, Ctxt#ctxt.encoding),
    Txt = get_node_text(Node),
    S = case {interpret_string(S0), interpret_string(Txt)} of
            {Same, Same} ->
                %% They're 'semantically' the same, but syntactically different
                Txt;
            {_, _} ->
                %% They're 'semantically' different. We couldn't parse the text
                %% correctly.
                S0
        end,
    lay_string_lines(string_lines(S), Ctxt).

interpret_string(undefined) ->
    undefined;
interpret_string(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    erl_parse:parse_exprs(Tokens ++ [{dot, 0}]).

string_lines([$\" | S0]) ->
    [$\" | S1] = lists:reverse(S0),
    Ls = string:split(
             lists:reverse(S1), "\"\n\"", all),
    [[$\" | L] ++ [$\"] || L <- Ls].

lay_string_lines([S], Ctxt) ->
    lay_string_line(S, Ctxt);
lay_string_lines([S | Ss], Ctxt) ->
    prettypr:above(lay_string_line(S, Ctxt), lay_string_lines(Ss, Ctxt)).

lay_string_line(S, #ctxt{truncate_strings = true, ribbon = Ribbon}) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    lay_string(S, length(S), Ribbon * 2 div 3);
lay_string_line(S, _) ->
    prettypr:text(prepare_string_line(S)).

%% @doc We need to replace \n\t here as a work around for how remove_tabs/1 works
%%      It's a hack, but it makes the formatter consistent.
%%      And it only affects strings that start with tab right after a newline.
%%      We truly hope that there are not too many of those.
%%      We also need to convert the rightmost space to \s to prevent
%%      remove_trailing_spaces/1 from removing it. Again another workaround.
prepare_string_line(S) ->
    switch_tabs_after_newline(switch_trailing_spaces(S)).

switch_trailing_spaces(String) ->
    re:replace(String, "\s\n", "\\\\s\n", [global, {return, list}, unicode]).

switch_tabs_after_newline(String) ->
    case re:replace(String,
                    <<$\n, $\t>>,
                    <<$\n, $\\, $\\, $t>>,
                    [global, {return, list}, unicode])
    of
        String ->
            String;
        Replaced ->
            switch_tabs_after_newline(Replaced)
    end.

lay_string(S, L, W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S, W - 1, L) of
        {_S1, ""} ->
            prettypr:text(S);
        {S1, S2} ->
            prettypr:above(
                prettypr:text(S1 ++ "\""), lay_string([$" | S2], L - W + 1, W))
    end;
lay_string(S, _L, _W) ->
    prettypr:text(S).

split_string(Xs, N, L) ->
    split_string_first(Xs, N, L, []).

%% We only split strings at whitespace, if possible. We must make sure
%% we do not split an escape sequence.
split_string_first([$\s | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$\s | As]), Xs};
split_string_first([$\t | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$t, $\\ | As]), Xs};
split_string_first([$\n | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$n, $\\ | As]), Xs};
split_string_first([$\\ | Xs], N, L, As) ->
    split_string_second(Xs, N - 1, L - 1, [$\\ | As]);
split_string_first(Xs, N, L, As) when N =< -10, L >= 5 ->
    {lists:reverse(As), Xs};
split_string_first([_ | _] = S, N, L, As) ->
    split_string_next(S, N, L, As);
split_string_first([], _N, _L, As) ->
    {lists:reverse(As), ""}.

split_string_second([$^, X | Xs], N, L, As) ->
    split_string_first(Xs, N - 2, L - 2, [X, $^ | As]);
split_string_second([$x, ${ | Xs], N, L, As) ->
    split_string_third(Xs, N - 2, L - 2, [${, $x | As]);
split_string_second([X1, X2, X3 | Xs], N, L, As)
    when X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7, X3 >= $0, X3 =< $7 ->
    split_string_first(Xs, N - 3, L - 3, [X3, X2, X1 | As]);
split_string_second([X1, X2 | Xs], N, L, As)
    when X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7 ->
    split_string_first(Xs, N - 2, L - 2, [X2, X1 | As]);
split_string_second(S, N, L, As) ->
    split_string_next(S, N, L, As).

split_string_third([$} | Xs], N, L, As) ->
    split_string_first(Xs, N - 1, L - 1, [$} | As]);
split_string_third([X | Xs], N, L, As)
    when X >= $0, X =< $9; X >= $a, X =< $z; X >= $A, X =< $Z ->
    split_string_third(Xs, N - 1, L - 1, [X | As]);
split_string_third([X | _Xs] = S, N, L, As) when X >= $0, X =< $9 ->
    split_string_next(S, N, L, As).

split_string_next([X | Xs], N, L, As) ->
    split_string_first(Xs, N - 1, L - 1, [X | As]);
split_string_next([], N, L, As) ->
    split_string_first([], N, L, As).

%% @doc Produces the layout for a spec/callback with a single clause that has a
%%      when... Which is a scenario that's common enough to deserve its own
%%      implementation. That allows us to place the when, if indented, closer to
%%      the margin and not floating below the parameters of the function in question
lay_simple_spec(NameDoc, Node, Ctxt) ->
    case erl_syntax:type(Node) of
        constrained_function_type ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:constrained_function_type_body(Node), Ctxt1#ctxt{clause = spec}),
            D2 = lay(erl_syntax:constrained_function_type_argument(Node), Ctxt1),
            prettypr:par([prettypr:beside(NameDoc, D1),
                          prettypr:beside(lay_text_float("when "), D2)],
                         Ctxt1#ctxt.break_indent);
        function_type ->
            prettypr:beside(NameDoc, lay(Node, Ctxt#ctxt{clause = spec}))
    end.

%% Note that there is nothing in `lay_clauses' that actually requires
%% that the elements have type `clause'; it just sets up the proper
%% context and arranges the elements suitably for clauses.
lay_clauses(Cs, Type, Ctxt) ->
    vertical(seq(Cs, lay_text_float(";"), Ctxt#ctxt{clause = Type}, fun lay/2)).

%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.
make_simple_fun_clause(P, G, B, Ctxt) ->
    D = make_fun_clause_head(none, P),

    % Since this anonymous fun has a single clause, we don't need to indent its
    % body that much
    append_clause_body(B,
                       append_guard(G, D, Ctxt),
                       Ctxt#ctxt{inline_clause_bodies =
                                     Ctxt#ctxt.inline_simple_funs
                                     orelse Ctxt#ctxt.inline_clause_bodies,
                                 break_indent = 0}).

make_fun_clause(P, G, B, Ctxt) ->
    make_fun_clause(none, P, G, B, Ctxt).

make_fun_clause(N, P, G, B, Ctxt) ->
    D = make_fun_clause_head(N, P),
    make_case_clause(D, G, B, Ctxt).

make_fun_clause_head(none, P) ->
    lay_parentheses(P);
make_fun_clause_head(N, P) ->
    prettypr:beside(N, lay_parentheses(P)).

make_case_clause(P, G, B, Ctxt) ->
    append_clause_body(B, append_guard(G, P, Ctxt), Ctxt).

make_if_clause(G, B, Ctxt) ->
    G1 = case G of
             none ->
                 prettypr:text("true");
             _ ->
                 G
         end,
    append_clause_body(B, G1, Ctxt).

append_clause_body(B, D, Ctxt) ->
    D1 = [prettypr:beside(D, lay_text_float(" ->")),
          prettypr:nest(Ctxt#ctxt.break_indent, B)],
    case Ctxt#ctxt.inline_clause_bodies of
        false ->
            vertical(D1);
        true ->
            prettypr:sep(D1)
    end.

append_guard(none, D, _) ->
    D;
append_guard(G, D, Ctxt) ->
    prettypr:par([D,
                  prettypr:follow(
                      prettypr:text("when"), G, Ctxt#ctxt.sub_indent)],
                 Ctxt#ctxt.break_indent).

lay_bit_types([T], Ctxt) ->
    lay(T, Ctxt);
lay_bit_types([T | Ts], Ctxt) ->
    prettypr:beside(lay(T, Ctxt),
                    prettypr:beside(lay_text_float("-"), lay_bit_types(Ts, Ctxt))).

lay_error_info({L, M, T} = T0, Ctxt) when is_integer(L), is_atom(M) ->
    try apply(M, format_error, [T]) of
        S when is_list(S) ->
            case L > 0 of
                true ->
                    prettypr:beside(
                        prettypr:text(
                            io_lib:format("~w: ", [L])),
                        prettypr:text(S));
                _ ->
                    prettypr:text(S)
            end;
        _ ->
            lay_concrete(T0, Ctxt)
    catch
        _:_ ->
            lay_concrete(T0, Ctxt)
    end;
lay_error_info(T, Ctxt) ->
    lay_concrete(T, Ctxt).

lay_concrete(T, Ctxt) ->
    lay(erl_syntax:abstract(T), Ctxt).

lay_type_assoc(Name, Value, Ctxt) ->
    lay_type_par_text(Name, Value, "=>", Ctxt).

lay_type_exact(Name, Value, Ctxt) ->
    lay_type_par_text(Name, Value, ":=", Ctxt).

lay_type_par_text(Name, Value, Text, Ctxt) ->
    Ctxt1 = reset_prec(Ctxt),
    D1 = lay(Name, Ctxt1),
    D2 = lay(Value, Ctxt1),
    prettypr:par([D1, lay_text_float(Text), D2], Ctxt1#ctxt.break_indent).

lay_application(Name, Arguments, Ctxt) ->
    lay_application(Name, Arguments, false, Ctxt).

lay_application(Name, Arguments, SpacesWithinParentheses, Ctxt) ->
    case erl_syntax:type(Name) of
        macro ->
            [Arg | Args] = Arguments,
            MacroVar = erl_syntax:variable([$? | atom_to_list(erl_syntax:variable_name(Arg))]),
            lay_application(MacroVar, Args, SpacesWithinParentheses, Ctxt);
        _ ->
            {PrecL, Prec} = erl_parse:func_prec(),
            MaybeSortedArgs = maybe_sort_arity_qualifiers(Arguments, Ctxt),
            {CommentedName, CommentedArgs} = move_comments(Name, MaybeSortedArgs),
            DName = prettypr:beside(lay(CommentedName, set_prec(Ctxt, PrecL)), prettypr:text("(")),
            DArgs = lay_items(CommentedArgs, reset_prec(Ctxt), fun lay/2),
            DClosingParen = lay_text_float(")"),
            D = case not Ctxt#ctxt.inline_qualified_function_composition
                     andalso is_qualified_function_composition(Name, Arguments)
                of
                    true ->
                        vertical([DName,
                                  prettypr:nest(Ctxt#ctxt.break_indent,
                                                prettypr:beside(DArgs, DClosingParen))]);
                    _ ->
                        case SpacesWithinParentheses andalso CommentedArgs /= [] of
                            false ->
                                prettypr:beside(DName, prettypr:beside(DArgs, DClosingParen));
                            true ->
                                prettypr:par([prettypr:par([DName, DArgs], Ctxt#ctxt.break_indent),
                                              DClosingParen])
                        end
                end,
            maybe_parentheses(D, Prec, Ctxt)
    end.

%% @doc Might produce a new AST node if the following criteria is met:
%%        1. we know that the AST node is ONLY a list of arity qualifiers
%%           (e.g.: [name/0]), and nothing else (no other kind of AST nodes
%%           in the list)
%%        2. the 'sort_arity_qualifiers' option was set to true
%%      In short, we only will sort arity qualifiers present in the '-export',
%%      '-export_type', and '-optional_callbacks' attributes.
maybe_sort_arity_qualifiers(OriginalAST, Ctxt) ->
    %% The 'OriginalAST' variable is a #list{} AST node for export lists,
    %% and a list of #tree{} AST nodes for both export_type and optional_callbacks
    %% lists. In order to unify the code handling the sorting for any of the cases,
    %% we have to wrap the list of #tree{} AST nodes in a #list{} AST node, sort it,
    %% and then unwrap it.
    case Ctxt#ctxt.sort_arity_qualifiers_match andalso Ctxt#ctxt.sort_arity_qualifiers of
        false ->
            OriginalAST;
        true when is_list(OriginalAST) ->
            [UnwrappedAST] = OriginalAST,
            do_sort_arity_qualifiers(UnwrappedAST);
        true ->
            ArityQualifiers = erl_syntax:list_elements(OriginalAST),
            SortedArityQualifiers = do_sort_arity_qualifiers(ArityQualifiers),
            erl_syntax:update_tree(OriginalAST, [SortedArityQualifiers])
    end.

%% @doc Might produce a new AST on which the arity qualifiers are sorted
%%      alphabetically if 'sort_arity_qualifiers' was set to 'true'.
%%      These arity qualifiers are the items in the export lists, export_type lists,
%%      and other module attributes that contain function references in the form of
%%      '[fun1/0, fun2/1]'.
do_sort_arity_qualifiers(Arguments0) when is_list(Arguments0) ->
    [Sorted] = do_sort_arity_qualifiers(erl_syntax:list(Arguments0)),
    erl_syntax:list_elements(Sorted);
do_sort_arity_qualifiers(Arguments0) ->
    case erl_syntax:subtrees(Arguments0) of
        [] ->
            %% node was a leaf node, skip
            [Arguments0];
        [SubTrees0] ->
            SubTrees1 = lists:sort(fun sort_arity_qualifiers_alphabetically/2, SubTrees0),
            [erl_syntax:update_tree(Arguments0, [SubTrees1])]
    end.

%% @doc Returns an altered AST with the arity qualifiers list
%%      sorted first by name and then by arity.
sort_arity_qualifiers_alphabetically(FuncInfoA, FuncInfoB) ->
    %% We get the relevant function info from the AST, namely its name and arity
    {FuncNameA, FuncArityA} = func_name_and_arity_from_ast(FuncInfoA),
    {FuncNameB, FuncArityB} = func_name_and_arity_from_ast(FuncInfoB),

    %% If we are comparing two functions with the same name,
    %% they should be ordered by their arity instead.
    {FuncNameA, FuncArityA} < {FuncNameB, FuncArityB}.

func_name_and_arity_from_ast(FuncSubTree) ->
    FuncName =
        erl_syntax:data(
            erl_syntax:arity_qualifier_body(FuncSubTree)),
    FuncArity =
        erl_syntax:data(
            erl_syntax:arity_qualifier_argument(FuncSubTree)),
    {FuncName, FuncArity}.

%% @doc Recursive function that groups nested applications of the same infix
%%      expression as a single list of docs.
infix_expr_docs(Node, Ctxt) ->
    Operator = erl_syntax:infix_expr_operator(Node),
    {OperatorName, {PrecL, Prec, PrecR}} =
        case erl_syntax:type(Operator) of
            operator ->
                ON = erl_syntax:operator_name(Operator),
                {ON, erl_parse:inop_prec(ON)};
            _ ->
                {undefined, {0, 0, 0}}
        end,
    OpDoc = lay(Operator, reset_prec(Ctxt)),
    LeftDocs =
        infix_expr_docs(OperatorName, erl_syntax:infix_expr_left(Node), set_prec(Ctxt, PrecL)),
    RightDocs =
        infix_expr_docs(OperatorName, erl_syntax:infix_expr_right(Node), set_prec(Ctxt, PrecR)),
    Ds = LeftDocs ++ [OpDoc | RightDocs],
    {Prec, Ds}.

infix_expr_docs(_, Node, #ctxt{parenthesize_infix_operations = true} = Ctxt) ->
    [lay_nested_infix_expr(Node, Ctxt)];
infix_expr_docs(OperatorName, Node, Ctxt) ->
    case infix_expr_operator_name(Node) of
        OperatorName ->
            {InnerPrecL, DsL} = infix_expr_docs(Node, Ctxt),
            case needs_parentheses(InnerPrecL, Ctxt) of
                false ->
                    DsL;
                true ->
                    [lay_nested_infix_expr(Node, Ctxt)]
            end;
        _ ->
            [lay_nested_infix_expr(Node, Ctxt)]
    end.

infix_expr_operator_name(Node) ->
    case erl_syntax:type(Node) of
        infix_expr ->
            Operator = erl_syntax:infix_expr_operator(Node),
            case erl_syntax:type(Operator) of
                operator ->
                    erl_syntax:operator_name(Operator);
                _ ->
                    not_an_operator
            end;
        _ ->
            not_an_operator
    end.

adjust_infix_expr_pars([Doc | Docs], Ctxt) ->
    [Doc | adjust_infix_expr_pars(Docs, Ctxt, [])].

adjust_infix_expr_pars([], _, Acc) ->
    lists:reverse(Acc);
adjust_infix_expr_pars([OpDoc, ExprDoc | Docs], Ctxt, Acc) ->
    adjust_infix_expr_pars(Docs,
                           Ctxt,
                           [prettypr:beside(
                                prettypr:beside(OpDoc, prettypr:text(" ")), ExprDoc)
                            | Acc]).

%% @doc If the name has postcomments and/or the first argument has precomments
%%      they get moved *too much*. So we convert them all into precomments, since
%%      that's how the look better.
move_comments(Name, []) ->
    {Name, []};
move_comments(Name, [Arg0 | Args]) ->
    case {erl_syntax:get_postcomments(Name), erl_syntax:get_precomments(Arg0)} of
        {[], _} ->
            {Name, [Arg0 | Args]};
        {PostComments, PreComments} ->
            {erl_syntax:set_postcomments(Name, []),
             [erl_syntax:set_precomments(Arg0, PostComments ++ PreComments) | Args]}
    end.

%% @doc Is this a function composition of two fully-qualified names.
%%      i.e. something like a_module:a_fun(another_module:another_func(...))
%%      The idea in this scenario is to indent that with the heuristic thinking that such
%%      a function composition will result in a very long line.
is_qualified_function_composition(_, []) ->
    false;
is_qualified_function_composition(Outside, [FirstArg | _]) ->
    erl_syntax:type(Outside) == module_qualifier
    andalso erl_syntax:type(FirstArg) == application
    andalso erl_syntax:type(
                erl_syntax:application_operator(FirstArg))
            == module_qualifier.

seq([H], _Separator, Ctxt, Fun) ->
    [Fun(H, Ctxt)];
seq([H | T], Separator, Ctxt, Fun) ->
    [maybe_append(hd(T), Separator, Fun(H, Ctxt)) | seq(T, Separator, Ctxt, Fun)];
seq([], _, _, _) ->
    [prettypr:empty()].

maybe_append(Next, Suffix, D) ->
    try erl_syntax:type(Next) of
        text ->
            case erl_syntax:get_ann(Next) of
                % This is introduced by ktn_dodger when parsing non-module files (e.g. app.src)
                [expression_dot] ->
                    prettypr:beside(D, lay_text_float("."));
                _ ->
                    maybe_append(Suffix, D)
            end;
        _ ->
            maybe_append(Suffix, D)
    catch
        error:{badarg, Next} -> % Sometimes lay_items is called with documents, not nodes.
            maybe_append(Suffix, D)
    end.

maybe_append(none, D) ->
    D;
maybe_append(Suffix, D) ->
    prettypr:beside(D, Suffix).

vertical([D]) ->
    D;
vertical([D | Ds]) ->
    prettypr:above(D, vertical(Ds));
vertical([]) ->
    [].

vertical_sep([{D, _}]) ->
    D;
vertical_sep([{D, empty_line} | Ds]) ->
    prettypr:above(
        prettypr:above(D, prettypr:text("")), vertical_sep(Ds));
vertical_sep([{D, no_empty_line} | Ds]) ->
    prettypr:above(D, vertical_sep(Ds));
vertical_sep([]) ->
    [].

empty_lines_to_add([], _Ctxt) ->
    [];
empty_lines_to_add([Node | Nodes], Ctxt) ->
    AfterThisNode =
        case erl_syntax:type(Node) of
            attribute ->
                AttrName = attribute_name(Node),
                case is_last_in_list(AttrName, Nodes) of
                    true ->
                        empty_line;
                    false ->
                        no_empty_line
                end;
            text -> % To handle the initial rows of escripts
                no_empty_line;
            _ ->
                empty_line
        end,
    [AfterThisNode | empty_lines_to_add(Nodes, Ctxt)].

is_last_in_list(_AttrName, []) ->
    true;
is_last_in_list(spec, _) ->
    false; % we never want to add an empty line after spec
is_last_in_list(AttrName, [Node | _]) ->
    erl_syntax:type(Node) /= attribute orelse attribute_name(Node) /= AttrName.

spaces(N) when N > 0 ->
    [$\s | spaces(N - 1)];
spaces(_) ->
    [].

tidy_integer(Node) ->
    tidy_number(Node, erl_syntax:integer_literal(Node)).

tidy_float(Node) ->
    tidy_number(Node, io_lib:format("~p", [erl_syntax:float_value(Node)])).

tidy_char(Node, Encoding) ->
    case get_node_text(Node) of
        undefined ->
            erl_syntax:char_literal(Node, Encoding);
        Text ->
            Text
    end.

tidy_atom(Node, #ctxt{encoding = Encoding, unquote_atoms = true}) ->
    erl_syntax:atom_literal(Node, Encoding);
tidy_atom(Node, #ctxt{encoding = Encoding}) ->
    case erl_syntax:is_tree(Node) of
        true -> %% It's not exactly an atom (e.g. module, export, spec)
            erl_syntax:atom_literal(Node, Encoding);
        false ->
            case get_node_text(Node) of
                undefined ->
                    erl_syntax:atom_literal(Node, Encoding);
                Text ->
                    Text
            end
    end.

%% @doc If we captured the original text for the number, then we use it.
%%      Otherwise, we use the value returned by the parser.
%%      The goal is to preserve things like 16#FADE or -1e-1 instead of turning
%%      them into integers or "pretty printed" floats.
tidy_number(Node, Default) ->
    case get_node_text(Node) of
        undefined ->
            Default;
        Text ->
            number_from_text(Text, Default)
    end.

%% @doc This function covers the corner case when erl_parse:parse_form/1
%%      (used by ktn_dodger) screws up the text for things like fun x/1 or
%%      -vsn(1) and therefore that text, that was actually captured,
%%      can not be used.
%% NOTE: floats work as "integers" according to string:to_integer/1
number_from_text(Text, Default) ->
    case string:to_integer(Text) of
        {error, no_integer} ->
            Default;
        {_, _} ->
            Text
    end.

lay_fields(Opening, Exprs, #ctxt{spaces_around_fields = false} = Ctxt, Fun) ->
    prettypr:beside(Opening,
                    prettypr:beside(lay_fields(Exprs, Ctxt, Fun), lay_text_float("}")));
lay_fields(Opening, [] = Exprs, #ctxt{spaces_around_fields = true} = Ctxt, Fun) ->
    lay_fields(Opening, Exprs, Ctxt#ctxt{spaces_around_fields = false}, Fun);
lay_fields(Opening, Exprs, #ctxt{spaces_around_fields = true} = Ctxt, Fun) ->
    prettypr:par([prettypr:par([Opening, lay_fields(Exprs, Ctxt, Fun)],
                               Ctxt#ctxt.break_indent),
                  lay_text_float("}")]).

lay_fields(Exprs, #ctxt{inline_fields = {when_over, N}} = Ctxt, Fun)
    when length(Exprs) > N ->
    prettypr:par(seq(Exprs, lay_text_float(","), Ctxt, Fun));
lay_fields(Exprs, #ctxt{inline_fields = {when_over, N}} = Ctxt, Fun)
    when length(Exprs) =< N ->
    vertical(seq(Exprs, lay_text_float(","), Ctxt, Fun));
lay_fields(Exprs, #ctxt{inline_fields = {when_under, N}} = Ctxt, Fun)
    when length(Exprs) < N ->
    prettypr:par(seq(Exprs, lay_text_float(","), Ctxt, Fun));
lay_fields(Exprs, #ctxt{inline_fields = {when_under, N}} = Ctxt, Fun)
    when length(Exprs) >= N ->
    vertical(seq(Exprs, lay_text_float(","), Ctxt, Fun));
lay_fields(Exprs, #ctxt{inline_fields = all} = Ctxt, Fun) ->
    prettypr:par(seq(Exprs, lay_text_float(","), Ctxt, Fun));
lay_fields(Exprs, #ctxt{inline_fields = none} = Ctxt, Fun) ->
    vertical(seq(Exprs, lay_text_float(","), Ctxt, Fun)).

lay_items(Exprs, Ctxt, Fun) ->
    lay_items(Exprs, lay_text_float(","), Ctxt, Fun).

lay_items(Exprs, Separator, #ctxt{inline_items = {when_over, N}} = Ctxt, Fun)
    when length(Exprs) > N ->
    prettypr:par(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs,
          Separator,
          #ctxt{force_indentation = true, inline_items = {when_over, N}} = Ctxt,
          Fun)
    when length(Exprs) =< N ->
    vertical(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs, Separator, #ctxt{inline_items = {when_over, N}} = Ctxt, Fun)
    when length(Exprs) =< N ->
    prettypr:sep(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs, Separator, #ctxt{inline_items = {when_under, N}} = Ctxt, Fun)
    when length(Exprs) < N ->
    prettypr:par(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs,
          Separator,
          #ctxt{force_indentation = true, inline_items = {when_under, N}} = Ctxt,
          Fun)
    when length(Exprs) >= N ->
    vertical(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs, Separator, #ctxt{inline_items = {when_under, N}} = Ctxt, Fun)
    when length(Exprs) >= N ->
    prettypr:sep(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs, Separator, #ctxt{inline_items = all} = Ctxt, Fun) ->
    prettypr:par(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs,
          Separator,
          #ctxt{force_indentation = true, inline_items = none} = Ctxt,
          Fun) ->
    vertical(seq(Exprs, Separator, Ctxt, Fun));
lay_items(Exprs, Separator, #ctxt{inline_items = none} = Ctxt, Fun) ->
    prettypr:sep(seq(Exprs, Separator, Ctxt, Fun)).

lay_clause_expressions(Exprs, #ctxt{inline_expressions = true} = Ctxt, Fun) ->
    prettypr:sep(seq(Exprs, lay_text_float(","), Ctxt, Fun));
lay_clause_expressions([H], Ctxt, Fun) ->
    Fun(H, Ctxt);
lay_clause_expressions([H | T], Ctxt, Fun) ->
    Clause = prettypr:beside(Fun(H, Ctxt), lay_text_float(",")),
    Next = lay_clause_expressions(T, Ctxt, Fun),
    case is_last_and_before_empty_line(H, T, Ctxt) of
        true ->
            prettypr:above(
                prettypr:above(Clause, prettypr:text("")), Next);
        false ->
            prettypr:above(Clause, Next)
    end;
lay_clause_expressions([], _, _) ->
    prettypr:empty().

is_last_and_before_empty_line(H, [H2 | _], #ctxt{empty_lines = EmptyLines}) ->
    H2Pos =
        case erl_syntax:get_precomments(H2) of
            [] ->
                get_pos(H2);
            [Comment | _] ->
                get_pos(Comment)
        end,
    H2Pos - get_pos(H) >= 2 andalso lists:member(H2Pos - 1, EmptyLines).

get_pos(Node) ->
    erl_anno:line(
        erl_syntax:get_pos(Node)).

get_node_text(Node) ->
    erl_anno:text(
        erl_syntax:get_pos(Node)).

lay_double_colon(D1, D2, Ctxt) ->
    prettypr:par([prettypr:beside(D1, lay_text_float(" ::")), D2], Ctxt#ctxt.break_indent).

lay_match_expression(Op, Pattern, D1, D2, Ctxt) ->
    case erl_syntax:type(Pattern) == underscore
         orelse erl_syntax:type(Pattern) == variable
                andalso length(erl_syntax:variable_literal(Pattern)) < Ctxt#ctxt.break_indent
    of
        true -> %% Single short variable on the left, don't nest
            prettypr:follow(
                prettypr:beside(D1, lay_text_float(Op)), D2, Ctxt#ctxt.break_indent);
        false -> %% Large pattern, nesting makes sense
            prettypr:sep([prettypr:beside(D1, lay_text_float(Op)),
                          prettypr:nest(Ctxt#ctxt.break_indent, D2)])
    end.
