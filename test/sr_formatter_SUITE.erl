-module(sr_formatter_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([action/1, output_dir/1, options/1, complete_coverage/1]).

all() ->
    [action, output_dir, options, complete_coverage].

action(_Config) ->
    steamroller:validator(fun(File, Opts) ->
                             <<"brackets.erl">> = filename:basename(File),
                             true = lists:member(check, Opts),
                             ok
                          end),
    Args1 = rebar_state:command_parsed_args(init(), {[{verify, true}], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    steamroller:validator(fun(File, Opts) ->
                             <<"brackets.erl">> = filename:basename(File),
                             false = lists:member(check, Opts),
                             ok
                          end),
    Args2 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args2),

    steamroller:validator(fun(_, Opts) ->
                             true = lists:member(check, Opts),
                             {error, <<"Check failed: something">>}
                          end),
    {error, _} = rebar3_format_prv:do(Args1),

    steamroller:validator(fun(_, Opts) ->
                             true = lists:member(check, Opts),
                             {error, other_error}
                          end),
    {error, _} = rebar3_format_prv:do(Args1).

output_dir(_Config) ->
    % When there is no expected output, steamroller should run in the input file
    steamroller:validator(fun(File, _) ->
                             <<"src/brackets.erl">> = File,
                             ok
                          end),
    Args1 = rebar_state:command_parsed_args(init(), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1),

    % When there is an expected output, steamroller should run on the input file
    steamroller:validator(fun(File, _) ->
                             <<"/tmp/src/brackets.erl">> = File,
                             file:write_file(File, <<>>),
                             ok
                          end),
    Args2 = rebar_state:command_parsed_args(init(), {[{output, "/tmp/"}], something}),
    {ok, _} = rebar3_format_prv:do(Args2).

options(_Config) ->
    steamroller:validator(fun(_, Opts) ->
                             100 = proplists:get_value(line_length, Opts),
                             all = proplists:get_value(inline_items, Opts),
                             50 = proplists:get_value(paper, Opts),
                             ok
                          end),
    Args1 = rebar_state:command_parsed_args(init(#{line_length => 100}), {[], something}),
    {ok, _} = rebar3_format_prv:do(Args1).

complete_coverage(_Config) ->
    Formatter = rebar3_formatter:new(sr_formatter, #{}, undefined),
    format = rebar3_formatter:action(Formatter).

init() ->
    init(#{}).

init(Options) ->
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_format), "../test_app")),
    {ok, State1} =
        rebar3_format:init(
            rebar_state:new()),
    Files = {files, ["src/brackets.erl"]},
    Formatter = {formatter, sr_formatter},
    Opts = {options, Options},
    rebar_state:set(State1, format, [Files, Formatter, Opts]).
