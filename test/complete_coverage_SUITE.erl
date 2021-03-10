%% @doc Examples that can't really be covered with files in test_app
-module(complete_coverage_SUITE).

-export([all/0]).
-export([comment_with_no_padding/1, very_empty_comment/1, smooth_operators/1,
         lost_clause/1, markers/1, balanced_parentheses/1, strange_constraint/1, funcy_specs/1]).

all() ->
    [comment_with_no_padding,
     very_empty_comment,
     smooth_operators,
     lost_clause,
     markers,
     balanced_parentheses,
     strange_constraint,
     funcy_specs].

comment_with_no_padding(_) ->
    Comment = erl_syntax:comment(["no padding"]),
    "%no padding\n\nx  %no padding\n" =
        format([Comment, erl_syntax:add_postcomments([Comment], erl_syntax:atom("x"))]).

very_empty_comment(_) ->
    "" = format([erl_syntax:comment([])]).

smooth_operators(_) ->
    Op = erl_syntax:macro(
             erl_syntax:atom("op")),
    "?op 1" = format([erl_syntax:prefix_expr(Op, erl_syntax:integer(1))]),
    "(1 ?op 2) ?op 3" =
        format([erl_syntax:infix_expr(
                    erl_syntax:infix_expr(
                        erl_syntax:integer(1), Op, erl_syntax:integer(2)),
                    Op,
                    erl_syntax:integer(3))],
               #{parenthesize_infix_operations => true}).

lost_clause(_) ->
    "() ->\n" = format([erl_syntax:clause([], none, [])]).

markers(_) ->
    "** err **\n\n%% WARNING: warn\n\n" =
        format([erl_syntax:error_marker(err),
                erl_syntax:warning_marker(warn),
                erl_syntax:eof_marker()]).

balanced_parentheses(_) ->
    "(x)" =
        format([erl_syntax:parentheses(
                    erl_syntax:atom("x"))]).

strange_constraint(_) ->
    "T(x)" =
        format([erl_syntax:constraint(
                    erl_syntax:variable("T"), [erl_syntax:atom("x")])]).

funcy_specs(_) ->
    "-?crashy.\n\n-spec func.\n-spec {?O_o}." =
        format([erl_syntax:attribute(
                    erl_syntax:macro(
                        erl_syntax:atom("crashy"))),
                erl_syntax:attribute(
                    erl_syntax:atom(spec),
                    [erl_syntax:tuple([erl_syntax:atom(func), erl_syntax:list([])])]),
                erl_syntax:attribute(
                    erl_syntax:atom(spec),
                    [erl_syntax:tuple([erl_syntax:tuple([erl_syntax:macro(
                                                             erl_syntax:variable("O_o"))]),
                                       erl_syntax:list([])])])]).

format(Nodes) ->
    format(Nodes, #{}).

format(Nodes, Opts) ->
    default_formatter:format(
        erl_syntax:form_list(Nodes), [], Opts).