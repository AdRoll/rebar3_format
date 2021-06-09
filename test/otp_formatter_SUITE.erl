-module(otp_formatter_SUITE).

-behaviour(ct_suite).

-export([all/0, test_app/1, error/1, modified_ast/1]).

all() ->
    [test_app, error, modified_ast].

test_app(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} = init(),
    Files =
        {files, ["src/*.app.src", "src/*.sh", "src/*.erl", "src/*/*.erl", "include/*.hrl"]},
    Formatter = {formatter, otp_formatter},
    IgnoredFiles =
        case string:to_integer(
                 erlang:system_info(otp_release))
        of
            {N, _} when N >= 23 ->
                {ignore,
                 ["src/*_ignore.erl",
                  "src/comments.erl",
                  "src/ignored_file_config.erl",
                  "src/dodge_macros.erl",
                  "src/macros_in_specs.erl"]};
            _ ->
                {ignore,
                 ["src/*_ignore.erl",
                  "src/comments.erl",
                  "src/ignored_file_config.erl",
                  "src/dodge_macros.erl",
                  "src/macros_in_specs.erl",
                  "src/otp23.erl"]}
        end,
    State2 = rebar_state:set(State1, format, [Files, Formatter, IgnoredFiles]),
    {error, _} = verify(State2),
    {ok, _} = format(State2),
    {error, _} = verify(State2),
    ok = file:set_cwd("formatted_as_otp"),
    {ok, _} = verify(State2),
    {ok, _} = rebar3_format_prv:do(State2).

error(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} = init(),
    Formatter = {formatter, otp_formatter},
    State2 = rebar_state:set(State1, format, [Formatter]),
    %% OTP formatter can't parse some of our files in test_app/src because of macros
    {error, _} = verify(State2).

%% otp_formatter messes up with some files. We have a mechanism to catch that.
modified_ast(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} = init(),
    Files = {files, ["src/dodge_macros.erl"]},
    Formatter = {formatter, otp_formatter},
    State2 = rebar_state:set(State1, format, [Files, Formatter]),
    {error, Error} = format(State2),
    {match, _} = re:run(Error, "modified_ast").

verify(State) ->
    rebar3_format_prv:do(
        rebar_state:command_parsed_args(State, {[{verify, true}], something})).

format(State) ->
    rebar3_format_prv:do(
        rebar_state:command_parsed_args(State, {[{output, "formatted_as_otp"}], something})).

init() ->
    {ok, State} =
        rebar_prv_app_discovery:do(
            rebar_state:new()),
    rebar3_format:init(State).
