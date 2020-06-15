-module(erlfmt_formatter_SUITE).

-export([all/0]).
-export([action/1, output_dir/1]).

all() ->
    [action, output_dir].

action(_Config) ->
    erlfmt:validator(fun (File, Out) ->
                             "brackets.erl" = filename:basename(File),
                             "/tmp/src/brackets.erl" = Out,
                             copy_file(File, Out),
                             {ok, []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[{verify, true}], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    erlfmt:validator(fun (File, Out) ->
                             "brackets.erl" = filename:basename(File),
                             replace = Out,
                             {ok, []}
                     end),
    Args2 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args2),

    erlfmt:validator(fun (_, Out) ->
                             file:write_file(Out, "something different"),
                             {ok, []}
                     end),
    {error, _} = rebar3_format_prv:do(Args1).

output_dir(_Config) ->
    % When there is no expected output, erlfmt's out should be 'replace'
    erlfmt:validator(fun (File, Out) ->
                             "src/brackets.erl" = File,
                             replace = Out,
                             {ok, []}
                     end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    % When there is an expected output, erlfmt's out should be it
    erlfmt:validator(fun (File, Out) ->
                             "brackets.erl" = filename:basename(File),
                             "/tmp/src/brackets.erl" = Out,
                             {ok, []}
                     end),
    Args2 = rebar_state:command_parsed_args(init(), {[{output, "/tmp/"}], something}),
    {ok, _} = rebar3_format_prv:do(Args2).

init() ->
    ok = file:set_cwd(filename:join(code:priv_dir(rebar3_format), "../test_app")),
    {ok, State1} = rebar3_format:init(rebar_state:new()),
    Files = {files, ["src/brackets.erl"]},
    Formatter = {formatter, erlfmt_formatter},
    Out = {options, #{}},
    rebar_state:set(State1, format, [Files, Formatter, Out]).

copy_file(File, OutFile) ->
    ok = filelib:ensure_dir(OutFile),
    {ok, _} = file:copy(File, OutFile),
    ok.
