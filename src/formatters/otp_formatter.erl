%% @doc Rebar3 Pretty Printing of abstract Erlang syntax trees,
%% based on original erl_prettypr.
%%
%% It was taken verbatim from erl_prettypr
%% and it was modified just to fix some bugs.
%%
%% This module is a front end to the pretty-printing library module
%% `prettypr', for text formatting of abstract syntax trees defined by
%% the module `erl_syntax'.
-module(otp_formatter).

-format #{inline_clause_bodies => true}.

-behaviour(rebar3_formatter).
-behaviour(rebar3_ast_formatter).

-export([init/2, format_file/3, format/1, format/3, best/1, best/2, layout/1, layout/2,
         get_ctxt_precedence/1, set_ctxt_precedence/2, get_ctxt_paperwidth/1,
         set_ctxt_paperwidth/2, get_ctxt_linewidth/1, set_ctxt_linewidth/2, get_ctxt_hook/1,
         set_ctxt_hook/2, get_ctxt_user/1, set_ctxt_user/2]).

-import(prettypr,
        [text/1,
         nest/2,
         above/2,
         beside/2,
         sep/1,
         par/1,
         par/2,
         floating/3,
         floating/1,
         break/1,
         follow/2,
         follow/3,
         empty/0]).
-import(erl_parse,
        [preop_prec/1,
         inop_prec/1,
         func_prec/0,
         max_prec/0,
         type_inop_prec/1,
         type_preop_prec/1]).

-define(PADDING, 2).
-define(PAPER, 80).
-define(RIBBON, 56).
-define(NOUSER, undefined).
-define(NOHOOK, none).

-type hook() :: none | fun((erl_syntax:syntaxTree(), _, _) -> prettypr:document()).
-type clause_t() :: case_expr |
                    fun_expr |
                    if_expr |
                    receive_expr |
                    try_expr |
                    {function, prettypr:document()} |
                    spec.

-record(ctxt,
        {prec = 0 :: integer(),
         sub_indent = 2 :: non_neg_integer(),
         break_indent = 4 :: non_neg_integer(),
         clause = undefined :: clause_t() | undefined,
         hook = ?NOHOOK :: hook(),
         paper = ?PAPER :: integer(),
         ribbon = ?RIBBON :: integer(),
         user = ?NOUSER :: term(),
         encoding = epp:default_encoding() :: epp:source_encoding(),
         empty_lines = sets:new() :: sets:set(integer())}).

-type context() :: #ctxt{}.

%% =====================================================================
%% The following functions examine and modify contexts:

%% @spec (context()) -> integer()
%% @doc Returns the operator precedence field of the prettyprinter
%% context.
%%
%% @see set_ctxt_precedence/2

-spec get_ctxt_precedence(context()) -> integer().
get_ctxt_precedence(Ctxt) -> Ctxt#ctxt.prec.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the operator precedence field of the prettyprinter
%% context. See the {@link //stdlib/erl_parse} module for operator precedences.
%%
%% @see //stdlib/erl_parse
%% @see get_ctxt_precedence/1

-spec set_ctxt_precedence(context(), integer()) -> context().
set_ctxt_precedence(Ctxt, Prec) -> set_prec(Ctxt, Prec).

set_prec(Ctxt, Prec) ->
    Ctxt#ctxt{prec = Prec}.    % used internally

reset_prec(Ctxt) ->
    set_prec(Ctxt, 0).    % used internally

%% @spec (context()) -> integer()
%% @doc Returns the paper widh field of the prettyprinter context.
%% @see set_ctxt_paperwidth/2

-spec get_ctxt_paperwidth(context()) -> integer().
get_ctxt_paperwidth(Ctxt) -> Ctxt#ctxt.paper.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the paper widh field of the prettyprinter context.
%%
%% Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.
%%
%% @see get_ctxt_paperwidth/1

-spec set_ctxt_paperwidth(context(), integer()) -> context().
set_ctxt_paperwidth(Ctxt, W) -> Ctxt#ctxt{paper = W}.

%% @spec (context()) -> integer()
%% @doc Returns the line widh field of the prettyprinter context.
%% @see set_ctxt_linewidth/2

-spec get_ctxt_linewidth(context()) -> integer().
get_ctxt_linewidth(Ctxt) -> Ctxt#ctxt.ribbon.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the line widh field of the prettyprinter context.
%%
%% Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.
%%
%% @see get_ctxt_linewidth/1

-spec set_ctxt_linewidth(context(), integer()) -> context().
set_ctxt_linewidth(Ctxt, W) -> Ctxt#ctxt{ribbon = W}.

%% @spec (context()) -> hook()
%% @doc Returns the hook function field of the prettyprinter context.
%% @see set_ctxt_hook/2

-spec get_ctxt_hook(context()) -> hook().
get_ctxt_hook(Ctxt) -> Ctxt#ctxt.hook.

%% @spec (context(), hook()) -> context()
%% @doc Updates the hook function field of the prettyprinter context.
%% @see get_ctxt_hook/1

-spec set_ctxt_hook(context(), hook()) -> context().
set_ctxt_hook(Ctxt, Hook) -> Ctxt#ctxt{hook = Hook}.

%% @spec (context()) -> term()
%% @doc Returns the user data field of the prettyprinter context.
%% @see set_ctxt_user/2

-spec get_ctxt_user(context()) -> term().
get_ctxt_user(Ctxt) -> Ctxt#ctxt.user.

%% @spec (context(), term()) -> context()
%% @doc Updates the user data field of the prettyprinter context.
%% @see get_ctxt_user/1

-spec set_ctxt_user(context(), term()) -> context().
set_ctxt_user(Ctxt, X) -> Ctxt#ctxt{user = X}.

%% =====================================================================
%% rebar3_formatter CALLBACKS
%% =====================================================================
%% @doc Initialize the formatter and generate a state that will be passed in when
%%      calling other callbacks.
-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) -> nostate.
init(_, _) -> nostate.

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
%%      Use <code>Opts</code> to configure the formatter.
-spec format_file(file:filename_all(), nostate, rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, nostate, Opts) -> rebar3_ast_formatter:format(File, ?MODULE, Opts).

%% =====================================================================
%% @spec format(Tree::syntaxTree()) -> string()
%% @equiv format(Tree, [])

-spec format(erl_syntax:syntaxTree()) -> string().
format(Node) -> format(Node, [], #{}).

%% =====================================================================
%% @spec format(Tree::syntaxTree(), [pos_integer()], Options::rebar3_formatter:opts()) -> string()
%%
%% @type syntaxTree() = erl_syntax:syntaxTree().
%%
%% An abstract syntax tree. See the {@link erl_syntax} module for
%% details.
%%
%% @type hook() = (syntaxTree(), context(), Continuation) ->
%%                            prettypr:document()
%%            Continuation = (syntaxTree(), context()) ->
%%                            prettypr:document().
%%
%% A call-back function for user-controlled formatting. See {@link
%% format/2}.
%%
%% @type context(). A representation of the current context of the
%% pretty-printer. Can be accessed in hook functions.
%%
%% @doc Prettyprint-formats an abstract Erlang syntax tree as text. For
%% example, if you have a `.beam' file that has been compiled with
%% `debug_info', the following should print the source code for the
%% module (as it looks in the debug info representation):
%% ```{ok,{_,[{abstract_code,{_,AC}}]}} =
%%            beam_lib:chunks("myfile.beam",[abstract_code]),
%%    io:put_chars(otp_formatter:format(erl_syntax:form_list(AC), [], #{}))
%% '''
%%
%% Available options:
%% <dl>
%%   <dt>{hook, none | {@link hook()}}</dt>
%%       <dd>Unless the value is `none', the given function is called
%%       for each node whose list of annotations is not empty; see below
%%       for details. The default value is `none'.</dd>
%%
%%   <dt>{paper, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, including indentation. The default value is 80.</dd>
%%
%%   <dt>{ribbon, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, not counting indentation. The default value is 65.</dd>
%%
%%   <dt>{user, term()}</dt>
%%       <dd>User-specific data for use in hook functions. The default
%%       value is `undefined'.</dd>
%%   <dt>{encoding, epp:source_encoding()}</dt>
%%       <dd>Specifies the encoding of the generated file.</dd>
%% </dl>
%%
%% A hook function (cf. the {@link hook()} type) is passed the current
%% syntax tree node, the context, and a continuation. The context can be
%% examined and manipulated by functions such as `get_ctxt_user/1' and
%% `set_ctxt_user/2'. The hook must return a "document" data structure
%% (see {@link layout/2} and {@link best/2}); this may be constructed in
%% part or in whole by applying the continuation function. For example,
%% the following is a trivial hook:
%% ```
%%     fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
%% '''
%% which yields the same result as if no hook was given.
%% The following, however:
%% ```
%%     fun (Node, Ctxt, Cont) ->
%%         Doc = Cont(Node, Ctxt),
%%         prettypr:beside(prettypr:text("<b>"),
%%                         prettypr:beside(Doc,
%%                                         prettypr:text("</b>")))
%%     end
%% '''
%% will place the text of any annotated node (regardless of the
%% annotation data) between HTML "boldface begin" and "boldface end"
%% tags.
%%
%% @see erl_syntax
%% @see format/1
%% @see layout/2
%% @see best/2
%% @see get_ctxt_user/1
%% @see set_ctxt_user/2

-spec format(erl_syntax:syntaxTree(), [pos_integer()], rebar3_formatter:opts()) ->
                string().
format(Node, EmptyLines, Options) ->
    W = maps:get(paper, Options, ?PAPER),
    L = maps:get(ribbon, Options, ?RIBBON),
    E = maps:get(encoding, Options, utf8),
    OptList = [{empty_lines, sets:from_list(EmptyLines)} | maps:to_list(Options)],
    PreFormatted = prettypr:format(layout(Node, OptList), W, L),
    binary_to_list(unicode:characters_to_binary(PreFormatted, E)).

%% =====================================================================
%% @spec best(Tree::syntaxTree()) -> empty | prettypr:document()
%% @equiv best(Tree, [])

-spec best(erl_syntax:syntaxTree()) -> empty | prettypr:document().
best(Node) -> best(Node, []).

%% =====================================================================
%% @spec best(Tree::syntaxTree(), Options::[term()]) ->
%%           empty | prettypr:document()
%%
%% @doc Creates a fixed "best" abstract layout for a syntax tree. This
%% is similar to the `layout/2' function, except that here, the final
%% layout has been selected with respect to the given options. The atom
%% `empty' is returned if no such layout could be produced. For
%% information on the options, see the `format/2' function.
%%
%% @see best/1
%% @see layout/2
%% @see format/2
%% @see prettypr:best/3

-spec best(erl_syntax:syntaxTree(), [term()]) -> empty | prettypr:document().
best(Node, Options) ->
    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),
    prettypr:best(layout(Node, Options), W, L).

%% =====================================================================
%% @spec layout(Tree::syntaxTree()) -> prettypr:document()
%% @equiv layout(Tree, [])

-spec layout(erl_syntax:syntaxTree()) -> prettypr:document().
layout(Node) -> layout(Node, []).

%% =====================================================================
%% @spec layout(Tree::syntaxTree(), Options::[term()]) -> prettypr:document()
%%
%% @doc Creates an abstract document layout for a syntax tree. The
%% result represents a set of possible layouts (cf. module `prettypr').
%% For information on the options, see {@link format/2}; note, however,
%% that the `paper' and `ribbon' options are ignored by this function.
%%
%% This function provides a low-level interface to the pretty printer,
%% returning a flexible representation of possible layouts, independent
%% of the paper width eventually to be used for formatting. This can be
%% included as part of another document and/or further processed
%% directly by the functions in the `prettypr' module, or used in a hook
%% function (see `format/2' for details).
%%
%% @see prettypr
%% @see format/2
%% @see layout/1

-spec layout(erl_syntax:syntaxTree(), [term()]) -> prettypr:document().
layout(Node, Options) ->
    lay(Node,
        #ctxt{hook = proplists:get_value(hook, Options, ?NOHOOK),
              paper = proplists:get_value(paper, Options, ?PAPER),
              ribbon = proplists:get_value(ribbon, Options, ?RIBBON),
              user = proplists:get_value(user, Options),
              encoding = proplists:get_value(encoding, Options, epp:default_encoding()),
              empty_lines = proplists:get_value(empty_lines, Options, sets:new())}).

lay(Node, Ctxt) ->
    case erl_syntax:get_ann(Node) of
        [] ->
            %% Hooks are not called if there are no annotations.
            do_lay(Node, Ctxt);
        _As ->
            case Ctxt#ctxt.hook of
                ?NOHOOK -> do_lay(Node, Ctxt);
                Hook -> Hook(Node, Ctxt, fun do_lay/2)
            end
    end.

%% This handles attached comments:

do_lay(Node, Ctxt) ->
    case erl_syntax:has_comments(Node) of
        true ->
            D1 = lay_no_comments(Node, Ctxt),
            D2 = lay_postcomments(erl_syntax:get_postcomments(Node), D1),
            lay_precomments(erl_syntax:get_precomments(Node), D2);
        false -> lay_no_comments(Node, Ctxt)
    end.

%% For pre-comments, all padding is ignored.

lay_precomments([], D) -> D;
lay_precomments(Cs, D) -> above(floating(break(stack_comments(Cs, false)), -1, -1), D).

%% For postcomments, individual padding is added.

lay_postcomments([], D) -> D;
lay_postcomments(Cs, D) -> beside(D, floating(break(stack_comments(Cs, true)), 1, 0)).

%% Format (including padding, if `Pad' is `true', otherwise not)
%% and stack the listed comments above each other.

stack_comments([C | Cs], Pad) ->
    D = stack_comment_lines(erl_syntax:comment_text(C)),
    D1 = case Pad of
             true ->
                 P = case erl_syntax:comment_padding(C) of
                         none -> ?PADDING;
                         P1 -> P1
                     end,
                 beside(text(spaces(P)), D);
             false -> D
         end,
    case Cs of
        [] ->
            D1; % done
        _ -> above(D1, stack_comments(Cs, Pad))
    end.

%% Stack lines of text above each other and prefix each string in
%% the list with a single `%' character.

stack_comment_lines([S | Ss]) ->
    D = text(add_comment_prefix(S)),
    case Ss of
        [] -> D;
        _ -> above(D, stack_comment_lines(Ss))
    end;
stack_comment_lines([]) -> empty().

add_comment_prefix(S) -> [$% | S].

%% This part ignores annotations and comments:

lay_no_comments(Node, Ctxt) ->
    case erl_syntax:type(Node) of
        %% We list literals and other common cases first.
        variable -> text(erl_syntax:variable_literal(Node));
        atom -> text(erl_syntax:atom_literal(Node, Ctxt#ctxt.encoding));
        integer -> text(tidy_integer(Node));
        float -> text(tidy_float(Node));
        char -> text(erl_syntax:char_literal(Node, Ctxt#ctxt.encoding));
        string -> lay_string(erl_syntax:string_literal(Node, Ctxt#ctxt.encoding), Ctxt);
        nil -> text("[]");
        tuple ->
            Es = seq(erl_syntax:tuple_elements(Node),
                     lay_text_float(","),
                     reset_prec(Ctxt),
                     fun lay/2),
            beside(lay_text_float("{"), beside(sep(Es), lay_text_float("}")));
        list ->
            Ctxt1 = reset_prec(Ctxt),
            Node1 = erl_syntax:compact_list(Node),
            D1 = sep(seq(erl_syntax:list_prefix(Node1), lay_text_float(","), Ctxt1, fun lay/2)),
            D = case erl_syntax:list_suffix(Node1) of
                    none -> beside(D1, lay_text_float("]"));
                    S ->
                        follow(D1,
                               beside(lay_text_float("| "),
                                      beside(lay(S, Ctxt1), lay_text_float("]"))))
                end,
            beside(lay_text_float("["), D);
        operator -> lay_text_float(erl_syntax:operator_literal(Node));
        infix_expr ->
            Operator = erl_syntax:infix_expr_operator(Node),
            {PrecL, Prec, PrecR} =
                case erl_syntax:type(Operator) of
                    operator -> inop_prec(erl_syntax:operator_name(Operator));
                    _ -> {0, 0, 0}
                end,
            D1 = lay(erl_syntax:infix_expr_left(Node), set_prec(Ctxt, PrecL)),
            D2 = lay(Operator, reset_prec(Ctxt)),
            D3 = lay(erl_syntax:infix_expr_right(Node), set_prec(Ctxt, PrecR)),
            D4 = par([D1, D2, D3], Ctxt#ctxt.break_indent),
            maybe_parentheses(D4, Prec, Ctxt);
        prefix_expr ->
            Operator = erl_syntax:prefix_expr_operator(Node),
            {{Prec, PrecR}, Name} =
                case erl_syntax:type(Operator) of
                    operator ->
                        N = erl_syntax:operator_name(Operator),
                        {preop_prec(N), N};
                    _ -> {{0, 0}, any}
                end,
            D1 = lay(Operator, reset_prec(Ctxt)),
            D2 = lay(erl_syntax:prefix_expr_argument(Node), set_prec(Ctxt, PrecR)),
            D3 = case Name of
                     '+' -> beside(D1, D2);
                     '-' -> beside(D1, D2);
                     _ -> par([D1, D2], Ctxt#ctxt.break_indent)
                 end,
            maybe_parentheses(D3, Prec, Ctxt);
        application ->
            {PrecL, Prec} = func_prec(),
            D = lay(erl_syntax:application_operator(Node), set_prec(Ctxt, PrecL)),
            As = seq(erl_syntax:application_arguments(Node),
                     floating(text(",")),
                     reset_prec(Ctxt),
                     fun lay/2),
            D1 = beside(D, beside(text("("), beside(sep(As), floating(text(")"))))),
            maybe_parentheses(D1, Prec, Ctxt);
        match_expr ->
            {PrecL, Prec, PrecR} = inop_prec('='),
            D1 = lay(erl_syntax:match_expr_pattern(Node), set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:match_expr_body(Node), set_prec(Ctxt, PrecR)),
            D3 = follow(beside(D1, lay_text_float(" =")), D2, Ctxt#ctxt.break_indent),
            maybe_parentheses(D3, Prec, Ctxt);
        underscore -> text("_");
        clause ->
            %% The style used for a clause depends on its context
            Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
            D1 = par(seq(erl_syntax:clause_patterns(Node), lay_text_float(","), Ctxt1, fun lay/2)),
            D2 = case erl_syntax:clause_guard(Node) of
                     none -> none;
                     G -> lay(G, Ctxt1)
                 end,
            D3 = lay_clause_expressions(erl_syntax:clause_body(Node), Ctxt1),
            case Ctxt#ctxt.clause of
                fun_expr -> make_fun_clause(D1, D2, D3, Ctxt);
                {function, N} -> make_fun_clause(N, D1, D2, D3, Ctxt);
                if_expr -> make_if_clause(D1, D2, D3, Ctxt);
                case_expr -> make_case_clause(D1, D2, D3, Ctxt);
                receive_expr -> make_case_clause(D1, D2, D3, Ctxt);
                try_expr -> make_case_clause(D1, D2, D3, Ctxt);
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
            beside(D2, lay_text_float("."));
        case_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:case_expr_argument(Node), Ctxt1),
            D2 = lay_clauses(erl_syntax:case_expr_clauses(Node), case_expr, Ctxt1),
            sep([par([follow(text("case"), D1, Ctxt1#ctxt.break_indent), text("of")],
                     Ctxt1#ctxt.break_indent),
                 nest(Ctxt1#ctxt.break_indent, D2),
                 text("end")]);
        if_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D = lay_clauses(erl_syntax:if_expr_clauses(Node), if_expr, Ctxt1),
            sep([follow(text("if"), D, Ctxt1#ctxt.break_indent), text("end")]);
        fun_expr ->
            Ctxt1 = reset_prec(Ctxt),
            Clauses = lay_clauses(erl_syntax:fun_expr_clauses(Node), fun_expr, Ctxt1),
            lay_fun_sep(Clauses, Ctxt1);
        named_fun_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:named_fun_expr_name(Node), Ctxt1),
            Clauses = lay_clauses(erl_syntax:named_fun_expr_clauses(Node), {function, D1}, Ctxt1),
            lay_fun_sep(Clauses, Ctxt1);
        module_qualifier ->
            {PrecL, _Prec, PrecR} = inop_prec(':'),
            D1 = lay(erl_syntax:module_qualifier_argument(Node), set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:module_qualifier_body(Node), set_prec(Ctxt, PrecR)),
            beside(D1, beside(text(":"), D2));
        %%
        %% The rest is in alphabetical order (except map and types)
        %%
        arity_qualifier ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:arity_qualifier_body(Node), Ctxt1),
            D2 = lay(erl_syntax:arity_qualifier_argument(Node), Ctxt1),
            beside(D1, beside(text("/"), D2));
        attribute ->
            %% The attribute name and arguments are formatted similar to
            %% a function call, but prefixed with a "-" and followed by
            %% a period. If the arguments is `none', we only output the
            %% attribute name, without following parentheses.
            Ctxt1 = reset_prec(Ctxt),
            Args = erl_syntax:attribute_arguments(Node),
            N = case erl_syntax:attribute_name(Node) of
                    {atom, _, 'if'} -> erl_syntax:variable('if');
                    N0 -> N0
                end,
            D = case attribute_type(Node) of
                    spec ->
                        [SpecTuple] = Args,
                        [FuncName, FuncTypes] = erl_syntax:tuple_elements(SpecTuple),
                        Name = get_func_node(FuncName),
                        Types = dodge_macros(FuncTypes),
                        D1 = lay_clauses(erl_syntax:concrete(Types), spec, Ctxt1),
                        beside(follow(lay(N, Ctxt1), lay(Name, Ctxt1), Ctxt1#ctxt.break_indent),
                               D1);
                    type ->
                        [TypeTuple] = Args,
                        [Name, Type0, Elements] = erl_syntax:tuple_elements(TypeTuple),
                        TypeName = dodge_macros(Name),
                        Type = dodge_macros(Type0),
                        As0 = dodge_macros(Elements),
                        As = erl_syntax:concrete(As0),
                        D1 = lay_type_application(TypeName, As, Ctxt1),
                        D2 = lay(erl_syntax:concrete(Type), Ctxt1),
                        beside(follow(lay(N, Ctxt1),
                                      beside(D1, lay_text_float(" :: ")),
                                      Ctxt1#ctxt.break_indent),
                               D2);
                    Tag when Tag =:= export_type; Tag =:= optional_callbacks ->
                        [FuncNs] = Args,
                        FuncNames = erl_syntax:concrete(dodge_macros(FuncNs)),
                        As = unfold_function_names(FuncNames),
                        beside(lay(N, Ctxt1),
                               beside(text("("), beside(lay(As, Ctxt1), lay_text_float(")"))));
                    _ when Args =:= none -> lay(N, Ctxt1);
                    _ ->
                        D1 = sep(seq(Args, lay_text_float(","), Ctxt1, fun lay/2)),
                        beside(lay(N, Ctxt1), beside(text("("), beside(D1, lay_text_float(")"))))
                end,
            beside(lay_text_float("-"), beside(D, lay_text_float(".")));
        binary ->
            Ctxt1 = reset_prec(Ctxt),
            Es = seq(erl_syntax:binary_fields(Node), lay_text_float(","), Ctxt1, fun lay/2),
            beside(lay_text_float("<<"), beside(par(Es), lay_text_float(">>")));
        binary_field ->
            Ctxt1 = set_prec(Ctxt, max_prec()),
            D1 = lay(erl_syntax:binary_field_body(Node), Ctxt1),
            D2 = case erl_syntax:binary_field_types(Node) of
                     [] -> empty();
                     Ts -> beside(lay_text_float("/"), lay_bit_types(Ts, Ctxt1))
                 end,
            beside(D1, D2);
        block_expr ->
            Ctxt1 = reset_prec(Ctxt),
            Es = seq(erl_syntax:block_expr_body(Node), lay_text_float(","), Ctxt1, fun lay/2),
            sep([text("begin"), nest(Ctxt1#ctxt.break_indent, sep(Es)), text("end")]);
        catch_expr ->
            {Prec, PrecR} = preop_prec('catch'),
            D = lay(erl_syntax:catch_expr_body(Node), set_prec(Ctxt, PrecR)),
            D1 = follow(text("catch"), D, Ctxt#ctxt.break_indent),
            maybe_parentheses(D1, Prec, Ctxt);
        class_qualifier ->
            Ctxt1 = set_prec(Ctxt, max_prec()),
            D1 = lay(erl_syntax:class_qualifier_argument(Node), Ctxt1),
            D2 = lay(erl_syntax:class_qualifier_body(Node), Ctxt1),
            Stacktrace = erl_syntax:class_qualifier_stacktrace(Node),
            case erl_syntax:variable_name(Stacktrace) of
                '_' -> beside(D1, beside(text(":"), D2));
                _ ->
                    D3 = lay(Stacktrace, Ctxt1),
                    beside(D1, beside(beside(text(":"), D2), beside(text(":"), D3)))
            end;
        comment ->
            D = stack_comment_lines(erl_syntax:comment_text(Node)),
            %% Default padding for standalone comments is empty.
            case erl_syntax:comment_padding(Node) of
                none -> floating(break(D));
                P -> floating(break(beside(text(spaces(P)), D)))
            end;
        conjunction ->
            par(seq(erl_syntax:conjunction_body(Node),
                    lay_text_float(","),
                    reset_prec(Ctxt),
                    fun lay/2));
        disjunction ->
            %% For clarity, we don't paragraph-format
            %% disjunctions; only conjunctions (see above).
            sep(seq(erl_syntax:disjunction_body(Node),
                    lay_text_float(";"),
                    reset_prec(Ctxt),
                    fun lay/2));
        error_marker ->
            E = erl_syntax:error_marker_info(Node),
            beside(text("** "), beside(lay_error_info(E, reset_prec(Ctxt)), text(" **")));
        eof_marker -> empty();
        form_list ->
            Es = seq(erl_syntax:form_list_elements(Node), none, reset_prec(Ctxt), fun lay/2),
            vertical_sep(text(""), Es);
        generator ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:generator_pattern(Node), Ctxt1),
            D2 = lay(erl_syntax:generator_body(Node), Ctxt1),
            par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.break_indent);
        binary_generator ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:binary_generator_pattern(Node), Ctxt1),
            D2 = lay(erl_syntax:binary_generator_body(Node), Ctxt1),
            par([D1, beside(text("<= "), D2)], Ctxt1#ctxt.break_indent);
        implicit_fun ->
            D = lay(erl_syntax:implicit_fun_name(Node), reset_prec(Ctxt)),
            beside(lay_text_float("fun "), D);
        list_comp ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:list_comp_template(Node), Ctxt1),
            D2 = par(seq(erl_syntax:list_comp_body(Node), lay_text_float(","), Ctxt1, fun lay/2)),
            beside(lay_text_float("["),
                   par([D1, beside(lay_text_float("|| "), beside(D2, lay_text_float("]")))]));
        binary_comp ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:binary_comp_template(Node), Ctxt1),
            D2 = par(seq(erl_syntax:binary_comp_body(Node), lay_text_float(","), Ctxt1, fun lay/2)),
            beside(lay_text_float("<< "),
                   par([D1, beside(lay_text_float(" || "), beside(D2, lay_text_float(" >>")))]));
        macro ->
            %% This is formatted similar to a normal function call, but
            %% prefixed with a "?".
            Ctxt1 = reset_prec(Ctxt),
            N = erl_syntax:macro_name(Node),
            D = case erl_syntax:macro_arguments(Node) of
                    none -> lay(N, Ctxt1);
                    Args ->
                        As = seq(Args, lay_text_float(","), set_prec(Ctxt1, max_prec()), fun lay/2),
                        beside(lay(N, Ctxt1),
                               beside(text("("), beside(par(As), lay_text_float(")"))))
                end,
            D1 = beside(lay_text_float("?"), D),
            maybe_parentheses(D1,
                              0,
                              Ctxt);    % must be conservative!
        parentheses ->
            D = lay(erl_syntax:parentheses_body(Node), reset_prec(Ctxt)),
            lay_parentheses(D, Ctxt);
        receive_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay_clauses(erl_syntax:receive_expr_clauses(Node), receive_expr, Ctxt1),
            D2 = case erl_syntax:receive_expr_timeout(Node) of
                     none -> D1;
                     T ->
                         D3 = lay(T, Ctxt1),
                         A = erl_syntax:receive_expr_action(Node),
                         D4 = sep(seq(A, lay_text_float(","), Ctxt1, fun lay/2)),
                         sep([D1,
                              follow(lay_text_float("after"),
                                     append_clause_body(D4, D3, Ctxt1),
                                     Ctxt1#ctxt.break_indent)])
                 end,
            sep([text("receive"), nest(Ctxt1#ctxt.break_indent, D2), text("end")]);
        record_access ->
            {PrecL, Prec, PrecR} = inop_prec('#'),
            D1 = lay(erl_syntax:record_access_argument(Node), set_prec(Ctxt, PrecL)),
            D2 = beside(lay_text_float("."),
                        lay(erl_syntax:record_access_field(Node), set_prec(Ctxt, PrecR))),
            T = erl_syntax:record_access_type(Node),
            D3 = beside(beside(lay_text_float("#"), lay(T, reset_prec(Ctxt))), D2),
            maybe_parentheses(beside(D1, D3), Prec, Ctxt);
        record_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:record_expr_type(Node), Ctxt1),
            D2 = par(seq(erl_syntax:record_expr_fields(Node),
                         lay_text_float(","),
                         Ctxt1,
                         fun lay/2)),
            D3 = beside(beside(lay_text_float("#"), D1),
                        beside(text("{"), beside(D2, lay_text_float("}")))),
            Arg = erl_syntax:record_expr_argument(Node),
            lay_expr_argument(Arg, D3, Ctxt);
        record_field ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:record_field_name(Node), Ctxt1),
            case erl_syntax:record_field_value(Node) of
                none -> D1;
                V -> par([D1, lay_text_float("="), lay(V, Ctxt1)], Ctxt1#ctxt.break_indent)
            end;
        record_index_expr ->
            {Prec, PrecR} = preop_prec('#'),
            D1 = lay(erl_syntax:record_index_expr_type(Node), reset_prec(Ctxt)),
            D2 = lay(erl_syntax:record_index_expr_field(Node), set_prec(Ctxt, PrecR)),
            D3 = beside(beside(lay_text_float("#"), D1), beside(lay_text_float("."), D2)),
            maybe_parentheses(D3, Prec, Ctxt);
        map_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = par(seq(erl_syntax:map_expr_fields(Node), lay_text_float(","), Ctxt1, fun lay/2)),
            D2 = beside(text("#{"), beside(D1, lay_text_float("}"))),
            Arg = erl_syntax:map_expr_argument(Node),
            lay_expr_argument(Arg, D2, Ctxt);
        map_field_assoc ->
            Name = erl_syntax:map_field_assoc_name(Node),
            Value = erl_syntax:map_field_assoc_value(Node),
            lay_type_assoc(Name, Value, Ctxt);
        map_field_exact ->
            Name = erl_syntax:map_field_exact_name(Node),
            Value = erl_syntax:map_field_exact_value(Node),
            lay_type_exact(Name, Value, Ctxt);
        size_qualifier ->
            Ctxt1 = set_prec(Ctxt, max_prec()),
            D1 = lay(erl_syntax:size_qualifier_body(Node), Ctxt1),
            D2 = lay(erl_syntax:size_qualifier_argument(Node), Ctxt1),
            beside(D1, beside(text(":"), D2));
        text -> text(erl_syntax:text_string(Node));
        typed_record_field ->
            {_, Prec, _} = type_inop_prec('::'),
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:typed_record_field_body(Node), Ctxt1),
            D2 = lay(erl_syntax:typed_record_field_type(Node), set_prec(Ctxt, Prec)),
            D3 = par([D1, lay_text_float("::"), D2], Ctxt1#ctxt.break_indent),
            maybe_parentheses(D3, Prec, Ctxt);
        try_expr ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = sep(seq(erl_syntax:try_expr_body(Node), lay_text_float(","), Ctxt1, fun lay/2)),
            Es0 = [text("end")],
            Es1 = case erl_syntax:try_expr_after(Node) of
                      [] -> Es0;
                      As ->
                          D2 = sep(seq(As, lay_text_float(","), Ctxt1, fun lay/2)),
                          [text("after"), nest(Ctxt1#ctxt.break_indent, D2) | Es0]
                  end,
            Es2 = case erl_syntax:try_expr_handlers(Node) of
                      [] -> Es1;
                      Hs ->
                          D3 = lay_clauses(Hs, try_expr, Ctxt1),
                          [text("catch"), nest(Ctxt1#ctxt.break_indent, D3) | Es1]
                  end,
            Es3 = case erl_syntax:try_expr_clauses(Node) of
                      [] -> Es2;
                      Cs ->
                          D4 = lay_clauses(Cs, try_expr, Ctxt1),
                          [text("of"), nest(Ctxt1#ctxt.break_indent, D4) | Es2]
                  end,
            sep([par([follow(text("try"), D1, Ctxt1#ctxt.break_indent), hd(Es3)]) | tl(Es3)]);
        warning_marker ->
            E = erl_syntax:warning_marker_info(Node),
            beside(text("%% WARNING: "), lay_error_info(E, reset_prec(Ctxt)));
        %%
        %% Types
        %%
        annotated_type ->
            {_, Prec, _} = type_inop_prec('::'),
            D1 = lay(erl_syntax:annotated_type_name(Node), reset_prec(Ctxt)),
            D2 = lay(erl_syntax:annotated_type_body(Node), set_prec(Ctxt, Prec)),
            D3 = lay_follow_beside_text_float(D1, D2, Ctxt),
            maybe_parentheses(D3, Prec, Ctxt);
        type_application ->
            Name = erl_syntax:type_application_name(Node),
            Arguments = erl_syntax:type_application_arguments(Node),
            %% Prefer shorthand notation.
            case erl_syntax_lib:analyze_type_application(Node) of
                {nil, 0} -> text("[]");
                {list, 1} ->
                    [A] = Arguments,
                    D1 = lay(A, reset_prec(Ctxt)),
                    beside(text("["), beside(D1, text("]")));
                {nonempty_list, 1} ->
                    [A] = Arguments,
                    D1 = lay(A, reset_prec(Ctxt)),
                    beside(text("["), beside(D1, text(", ...]")));
                _ -> lay_type_application(Name, Arguments, Ctxt)
            end;
        bitstring_type ->
            Ctxt1 = set_prec(Ctxt, max_prec()),
            M = erl_syntax:bitstring_type_m(Node),
            N = erl_syntax:bitstring_type_n(Node),
            D1 = [beside(text("_:"), lay(M, Ctxt1))
                  || erl_syntax:type(M) =/= integer orelse erl_syntax:integer_value(M) =/= 0],
            D2 = [beside(text("_:_*"), lay(N, Ctxt1))
                  || erl_syntax:type(N) =/= integer orelse erl_syntax:integer_value(N) =/= 0],
            F = fun(D, _) -> D end,
            D = seq(D1 ++ D2, lay_text_float(","), Ctxt1, F),
            beside(lay_text_float("<<"), beside(par(D), lay_text_float(">>")));
        fun_type -> text("fun()");
        constrained_function_type ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:constrained_function_type_body(Node), Ctxt1),
            Ctxt2 = Ctxt1#ctxt{clause = undefined},
            D2 = lay(erl_syntax:constrained_function_type_argument(Node), Ctxt2),
            beside(D1, beside(lay_text_float(" when "), D2));
        function_type ->
            {Before, After} =
                case Ctxt#ctxt.clause of
                    spec -> {"", ""};
                    _ -> {"fun(", ")"}
                end,
            Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
            D1 = case erl_syntax:function_type_arguments(Node) of
                     any_arity -> text("(...)");
                     Arguments ->
                         As = seq(Arguments, lay_text_float(","), Ctxt1, fun lay/2),
                         beside(text("("), beside(par(As), lay_text_float(")")))
                 end,
            D2 = lay(erl_syntax:function_type_return(Node), Ctxt1),
            beside(lay_text_float(Before),
                   beside(D1, beside(lay_text_float(" -> "), beside(D2, lay_text_float(After)))));
        constraint ->
            Name = erl_syntax:constraint_argument(Node),
            Args = erl_syntax:constraint_body(Node),
            case is_subtype(Name, Args) of
                true ->
                    [Var, Type] = Args,
                    {PrecL, Prec, PrecR} = type_inop_prec('::'),
                    D1 = lay(Var, set_prec(Ctxt, PrecL)),
                    D2 = lay(Type, set_prec(Ctxt, PrecR)),
                    D3 = lay_follow_beside_text_float(D1, D2, Ctxt),
                    maybe_parentheses(D3, Prec, Ctxt);
                false -> lay_type_application(Name, Args, Ctxt)
            end;
        map_type ->
            case erl_syntax:map_type_fields(Node) of
                any_size -> text("map()");
                Fs ->
                    Ctxt1 = reset_prec(Ctxt),
                    Es = seq(Fs, lay_text_float(","), Ctxt1, fun lay/2),
                    D = beside(lay_text_float("#{"), beside(par(Es), lay_text_float("}"))),
                    {Prec, _PrecR} = type_preop_prec('#'),
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
            {PrecL, Prec, PrecR} = type_inop_prec('..'),
            D1 = lay(erl_syntax:integer_range_type_low(Node), set_prec(Ctxt, PrecL)),
            D2 = lay(erl_syntax:integer_range_type_high(Node), set_prec(Ctxt, PrecR)),
            D3 = beside(D1, beside(text(".."), D2)),
            maybe_parentheses(D3, Prec, Ctxt);
        record_type ->
            {Prec, _PrecR} = type_preop_prec('#'),
            D1 = beside(text("#"), lay(erl_syntax:record_type_name(Node), reset_prec(Ctxt))),
            Es = seq(erl_syntax:record_type_fields(Node),
                     lay_text_float(","),
                     reset_prec(Ctxt),
                     fun lay/2),
            D2 = beside(D1, beside(text("{"), beside(par(Es), lay_text_float("}")))),
            maybe_parentheses(D2, Prec, Ctxt);
        record_type_field ->
            Ctxt1 = reset_prec(Ctxt),
            D1 = lay(erl_syntax:record_type_field_name(Node), Ctxt1),
            D2 = lay(erl_syntax:record_type_field_type(Node), Ctxt1),
            par([D1, lay_text_float("::"), D2], Ctxt1#ctxt.break_indent);
        tuple_type ->
            case erl_syntax:tuple_type_elements(Node) of
                any_size -> text("tuple()");
                Elements ->
                    Es = seq(Elements, lay_text_float(","), reset_prec(Ctxt), fun lay/2),
                    beside(lay_text_float("{"), beside(par(Es), lay_text_float("}")))
            end;
        type_union ->
            {_, Prec, PrecR} = type_inop_prec('|'),
            Es = sep(seq(erl_syntax:type_union_types(Node),
                         lay_text_float(" |"),
                         set_prec(Ctxt, PrecR),
                         fun lay/2)),
            maybe_parentheses(Es, Prec, Ctxt);
        user_type_application ->
            lay_type_application(erl_syntax:user_type_application_name(Node),
                                 erl_syntax:user_type_application_arguments(Node),
                                 Ctxt)
    end.

attribute_type(Node) ->
    N = erl_syntax:attribute_name(Node),
    case catch erl_syntax:concrete(N) of
        opaque -> type;
        spec -> spec;
        callback -> spec;
        type -> type;
        export_type -> export_type;
        optional_callbacks -> optional_callbacks;
        _ -> N
    end.

is_subtype(Name, [Var, _]) ->
    erl_syntax:is_atom(Name, is_subtype) andalso erl_syntax:type(Var) =:= variable;
is_subtype(_, _) -> false.

get_func_node(Node) ->
    case erl_syntax:type(Node) of
        tuple ->
            case erl_syntax:tuple_elements(Node) of
                [F0, _] -> F0;
                [M0, F0, _] -> erl_syntax:module_qualifier(M0, F0);
                _ -> Node
            end;
        _ -> Node
    end.

unfold_function_names(Ns) ->
    F = fun({Atom, Arity}) ->
           erl_syntax:arity_qualifier(
               erl_syntax:atom(Atom), erl_syntax:integer(Arity))
        end,
    erl_syntax:list([F(N) || N <- Ns]).

%% Macros are not handled well.
dodge_macros(Type) ->
    F = fun(T) ->
           case erl_syntax:type(T) of
               macro ->
                   Var = erl_syntax:macro_name(T),
                   VarName0 = erl_syntax:variable_name(Var),
                   VarName = list_to_atom("?" ++ atom_to_list(VarName0)),
                   Atom = erl_syntax:atom(VarName),
                   Atom;
               _ -> T
           end
        end,
    erl_syntax_lib:map(F, Type).

lay_text_float(Str) -> floating(text(Str)).

lay_follow_beside_text_float(D1, D2, Ctxt) ->
    follow(beside(D1, lay_text_float(" ::")), D2, Ctxt#ctxt.break_indent).

lay_fun_sep(Clauses, Ctxt) ->
    sep([follow(text("fun"), Clauses, Ctxt#ctxt.break_indent), text("end")]).

lay_expr_argument(none, D, Ctxt) ->
    {_, Prec, _} = inop_prec('#'),
    maybe_parentheses(D, Prec, Ctxt);
lay_expr_argument(Arg, D, Ctxt) ->
    {PrecL, Prec, _} = inop_prec('#'),
    D1 = beside(lay(Arg, set_prec(Ctxt, PrecL)), D),
    maybe_parentheses(D1, Prec, Ctxt).

lay_parentheses(D, _Ctxt) -> beside(lay_text_float("("), beside(D, lay_text_float(")"))).

maybe_parentheses(D, Prec, Ctxt) ->
    case Ctxt#ctxt.prec of
        P when P > Prec -> lay_parentheses(D, Ctxt);
        _ -> D
    end.

lay_string(S, Ctxt) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    W = Ctxt#ctxt.ribbon * 2 div 3,
    lay_string(S, length(S), W).

lay_string(S, L, W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S, W - 1, L) of
        {_S1, ""} -> text(S);
        {S1, S2} ->
            above(text(S1 ++ "\""),
                  lay_string([$" | S2], L - W + 1, W))  %" stupid emacs
    end;
lay_string(S, _L, _W) -> text(S).

split_string(Xs, N, L) -> split_string_first(Xs, N, L, []).

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
split_string_first(Xs, N, L, As) when N =< -10, L >= 5 -> {lists:reverse(As), Xs};
split_string_first([_ | _] = S, N, L, As) -> split_string_next(S, N, L, As);
split_string_first([], _N, _L, As) -> {lists:reverse(As), ""}.

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
split_string_second(S, N, L, As) -> split_string_next(S, N, L, As).

split_string_third([$} | Xs], N, L, As) ->
    split_string_first(Xs, N - 1, L - 1, [$} | As]);
split_string_third([X | Xs], N, L, As)
    when X >= $0, X =< $9; X >= $a, X =< $z; X >= $A, X =< $Z ->
    split_string_third(Xs, N - 1, L - 1, [X | As]);
split_string_third([X | _Xs] = S, N, L, As) when X >= $0, X =< $9 ->
    split_string_next(S, N, L, As).

split_string_next([X | Xs], N, L, As) -> split_string_first(Xs, N - 1, L - 1, [X | As]);
split_string_next([], N, L, As) -> split_string_first([], N, L, As).

%% Note that there is nothing in `lay_clauses' that actually requires
%% that the elements have type `clause'; it just sets up the proper
%% context and arranges the elements suitably for clauses.

lay_clauses(Cs, Type, Ctxt) ->
    vertical(seq(Cs, lay_text_float(";"), Ctxt#ctxt{clause = Type}, fun lay/2)).

%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.

make_fun_clause(P, G, B, Ctxt) -> make_fun_clause(none, P, G, B, Ctxt).

make_fun_clause(N, P, G, B, Ctxt) ->
    D = make_fun_clause_head(N, P, Ctxt),
    make_case_clause(D, G, B, Ctxt).

make_fun_clause_head(N, P, Ctxt) when N =:= none -> lay_parentheses(P, Ctxt);
make_fun_clause_head(N, P, Ctxt) -> beside(N, lay_parentheses(P, Ctxt)).

make_case_clause(P, G, B, Ctxt) -> append_clause_body(B, append_guard(G, P, Ctxt), Ctxt).

make_if_clause(_P, G, B, Ctxt) ->
    %% We ignore the patterns; they should be empty anyway.
    G1 = case G of
             none -> text("true");
             _ -> G
         end,
    append_clause_body(B, G1, Ctxt).

append_clause_body(B, D, Ctxt) -> append_clause_body(B, D, lay_text_float(" ->"), Ctxt).

append_clause_body(B, D, S, Ctxt) -> sep([beside(D, S), nest(Ctxt#ctxt.break_indent, B)]).

append_guard(none, D, _) -> D;
append_guard(G, D, Ctxt) ->
    par([D, follow(text("when"), G, Ctxt#ctxt.sub_indent)], Ctxt#ctxt.break_indent).

lay_bit_types([T], Ctxt) -> lay(T, Ctxt);
lay_bit_types([T | Ts], Ctxt) ->
    beside(lay(T, Ctxt), beside(lay_text_float("-"), lay_bit_types(Ts, Ctxt))).

lay_error_info({L, M, T} = T0, Ctxt) when is_integer(L), is_atom(M) ->
    case catch apply(M, format_error, [T]) of
        S when is_list(S) ->
            case L > 0 of
                true -> beside(text(io_lib:format("~w: ", [L])), text(S));
                _ -> text(S)
            end;
        _ -> lay_concrete(T0, Ctxt)
    end;
lay_error_info(T, Ctxt) -> lay_concrete(T, Ctxt).

lay_concrete(T, Ctxt) -> lay(erl_syntax:abstract(T), Ctxt).

lay_type_assoc(Name, Value, Ctxt) -> lay_type_par_text(Name, Value, "=>", Ctxt).

lay_type_exact(Name, Value, Ctxt) -> lay_type_par_text(Name, Value, ":=", Ctxt).

lay_type_par_text(Name, Value, Text, Ctxt) ->
    Ctxt1 = reset_prec(Ctxt),
    D1 = lay(Name, Ctxt1),
    D2 = lay(Value, Ctxt1),
    par([D1, lay_text_float(Text), D2], Ctxt1#ctxt.break_indent).

lay_type_application(Name, Arguments, Ctxt) ->
    {PrecL, Prec} = func_prec(), %
    D1 = lay(Name, set_prec(Ctxt, PrecL)),
    As = seq(Arguments, lay_text_float(","), reset_prec(Ctxt), fun lay/2),
    D = beside(D1, beside(text("("), beside(par(As), lay_text_float(")")))),
    maybe_parentheses(D, Prec, Ctxt).

seq([H | T], Separator, Ctxt, Fun) ->
    case T of
        [] -> [Fun(H, Ctxt)];
        _ -> [maybe_append(Separator, Fun(H, Ctxt)) | seq(T, Separator, Ctxt, Fun)]
    end;
seq([], _, _, _) -> [empty()].

maybe_append(none, D) -> D;
maybe_append(Suffix, D) -> beside(D, Suffix).

vertical([D]) -> D;
vertical([D | Ds]) -> above(D, vertical(Ds));
vertical([]) -> [].

vertical_sep(_Sep, [D]) -> D;
vertical_sep(Sep, [D | Ds]) -> above(above(D, Sep), vertical_sep(Sep, Ds));
vertical_sep(_Sep, []) -> [].

spaces(N) when N > 0 -> [$\s | spaces(N - 1)];
spaces(_) -> [].

tidy_integer(Node) -> tidy_number(Node, erl_syntax:integer_literal(Node)).

tidy_float(Node) ->
    tidy_number(Node, io_lib:format("~p", [erl_syntax:float_value(Node)])).

%% @doc If we captured the original text for the number, then we use it.
%%      Otherwise, we use the value returned by the parser.
%%      The goal is to preserve things like 16#FADE or -1e-1 instead of turning
%%      them into integers or "pretty printed" floats.
tidy_number(Node, Default) ->
    case erl_syntax:get_pos(Node) of
        L when is_list(L) ->
            case proplists:get_value(text, L, undefined) of
                undefined -> Default;
                Text -> number_from_text(Text, Default)
            end;
        _ -> Default
    end.

%% @doc This function covers the corner case when erl_parse:parse_form/1
%%      (used by ktn_dodger) screws up the text for things like fun x/1 or
%%      -vsn(1) and therefore that text, that was actually captured,
%%      can not be used.
%% NOTE: floats work as "integers" according to string:to_integer/1
number_from_text(Text, Default) ->
    case string:to_integer(Text) of
        {error, no_integer} -> Default;
        {_, _} -> Text
    end.

lay_clause_expressions([H], Ctxt) -> lay(H, Ctxt);
lay_clause_expressions([H | T], Ctxt) ->
    Clause = beside(lay(H, Ctxt), lay_text_float(",")),
    Next = lay_clause_expressions(T, Ctxt),
    case is_last_and_before_empty_line(H, T, Ctxt) of
        true -> above(above(Clause, text("")), Next);
        false -> above(Clause, Next)
    end;
lay_clause_expressions([], _) -> empty().

is_last_and_before_empty_line(H, [], #ctxt{empty_lines = EmptyLines}) ->
    try
        sets:is_element(erl_syntax:get_pos(H) + 1, EmptyLines)
    catch
        error:badarith -> false
    end;
is_last_and_before_empty_line(H, [H2 | _], #ctxt{empty_lines = EmptyLines}) ->
    try
        (erl_syntax:get_pos(H2) - erl_syntax:get_pos(H) >= 2) and
            sets:is_element(erl_syntax:get_pos(H) + 1, EmptyLines)
    catch
        error:badarith -> false
    end.

%% =====================================================================
