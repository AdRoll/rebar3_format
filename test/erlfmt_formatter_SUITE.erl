-module(erlfmt_formatter_SUITE).

-export([all/0]).
-export([action/1, output_dir/1, pragma/1, print_width/1, old_version/1, error/1]).

all() ->
    [action, output_dir, pragma, print_width, old_version, error].

action(_Config) ->
    erlfmt:validator(fun(File, _) ->
                        "minimal.erl" = filename:basename(File),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[{verify, true}], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    erlfmt:validator(fun(File, _) ->
                        "minimal.erl" = filename:basename(File),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args2 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args2),

    erlfmt:validator(fun(_, _) -> {ok, ["-module minimal.\n"], []} end),
    {error, _} = rebar3_format_prv:do(Args1).

output_dir(_Config) ->
    erlfmt:validator(fun(File, _) ->
                        "minimal.erl" = filename:basename(File),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    erlfmt:validator(fun(File, _) ->
                        "minimal.erl" = filename:basename(File),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args2 = rebar_state:command_parsed_args(init(), {[{output, "/tmp/"}], something}),
    {ok, _} = rebar3_format_prv:do(Args2),
    {ok, <<"-module(minimal).\n">>} = file:read_file("/tmp/src/minimal.erl").

pragma(_Config) ->
    % When there is no defined require_pragma nor insert_prgama, Pragma should be ignore
    erlfmt:validator(fun(File, Opts) ->
                        "src/minimal.erl" = File,
                        ignore = proplists:get_value(pragma, Opts),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    % When require_pragma is true, erlfmt's pragma should be require
    erlfmt:validator(fun(File, Opts) ->
                        "minimal.erl" = filename:basename(File),
                        require = proplists:get_value(pragma, Opts),
                        {skip, "-module(minimal).\n"}
                     end),
    Args2 = rebar_state:command_parsed_args(init(#{require_pragma => true}), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args2),

    % When require_pragma is false, erlfmt's pragma depends on insert_pragma
    % If insert_pragma is true, it should be insert
    erlfmt:validator(fun(File, Opts) ->
                        "minimal.erl" = filename:basename(File),
                        insert = proplists:get_value(pragma, Opts),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args3 =
        rebar_state:command_parsed_args(init(#{require_pragma => false, insert_pragma => true}),
                                        {[], something}),
    {ok, _} = rebar3_format_prv:do(Args3),

    % Otherwise it should be ignore
    erlfmt:validator(fun(File, Opts) ->
                        "minimal.erl" = filename:basename(File),
                        ignore = proplists:get_value(pragma, Opts),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args4 =
        rebar_state:command_parsed_args(init(#{require_pragma => false, insert_pragma => false}),
                                        {[], something}),
    {ok, _} = rebar3_format_prv:do(Args4).

print_width(_Config) ->
    % When there is no defined print_width
    erlfmt:validator(fun(File, Opts) ->
                        "src/minimal.erl" = File,
                        false = proplists:is_defined(print_width, Opts),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    % When print_width has a value, erlfmt's print_width should be it
    erlfmt:validator(fun(File, Opts) ->
                        "minimal.erl" = filename:basename(File),
                        50 = proplists:get_value(print_width, Opts, undefined),
                        {skip, "-module(minimal).\n"}
                     end),
    Args2 = rebar_state:command_parsed_args(init(#{print_width => 50}), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args2).

%% @doc support for v0.7.0
old_version(_Config) ->
    % When there is no defined print_width
    erlfmt:validator(fun(File, Opts) ->
                        "src/minimal.erl" = File,
                        false = proplists:is_defined(width, Opts),
                        {ok, ["-module(minimal).\n"], []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    % When print_width has a value, erlfmt's width should be it
    erlfmt:validator(fun(File, Opts) ->
                        "minimal.erl" = filename:basename(File),
                        50 = proplists:get_value(width, Opts, undefined),
                        {skip, "-module(minimal).\n"}
                     end),
    Args2 = rebar_state:command_parsed_args(init(#{print_width => 50}), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args2).

error(_Config) ->
    erlfmt:validator(fun(File, _) -> {error, reason} end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {error, _} = rebar3_format_prv:do(Args1).

init() ->
    init(#{}).

init(Opts) ->
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_format), "../test_app")),
    {ok, State1} =
        rebar3_format:init(
            rebar_state:new()),
    Files = {files, ["src/minimal.erl"]},
    Formatter = {formatter, erlfmt_formatter},
    Out = {options, Opts},
    rebar_state:set(State1, format, [Files, Formatter, Out]).
