-module(otp_formatter_SUITE).

-export([all/0, test_app/1]).

all() ->
    [test_app].

test_app(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} =
        rebar3_format:init(
            rebar_state:new()),
    Files = {files, ["src/*.app.src", "src/*.sh", "src/*.erl", "include/*.hrl"]},
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
    {ok, _} = verify(State2).

verify(State) ->
    rebar3_format_prv:do(
        rebar_state:command_parsed_args(State, {[{verify, true}], something})).

format(State) ->
    rebar3_format_prv:do(
        rebar_state:command_parsed_args(State, {[{output, "formatted_as_otp"}], something})).
