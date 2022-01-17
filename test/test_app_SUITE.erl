-module(test_app_SUITE).

-behaviour(ct_suite).

-export([all/0, test_app/1, no_good_files/1]).

all() ->
    [test_app, no_good_files].

test_app(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    State2 = init_test_app(),
    {error, _} = verify(State2),
    {ok, _} = format(State2),
    {error, _} = verify(State2),
    ok = file:set_cwd("formatted"),
    State3 = init_test_app(),
    {ok, _} = verify(State3),
    ok = file:set_cwd(".."),
    ok = git_diff().

no_good_files(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} = init(),
    Files = {files, ["a.broken.file", "a.non.existent.file"]},
    State2 = rebar_state:set(State1, format, [Files]),
    %% Our parsers don't crash on unparseable or non-existent files
    {ok, _} = verify(State2).

verify(State) ->
    rebar3_format_prv:do(
        rebar_state:command_parsed_args(State, {[{verify, true}], something})).

format(State) ->
    rebar3_format_prv:do(
        rebar_state:command_parsed_args(State, {[{output, "formatted"}], something})).

init() ->
    {ok, State} =
        rebar_prv_app_discovery:do(
            rebar_state:new()),
    rebar3_format:init(State).

init_test_app() ->
    {ok, State1} = init(),
    Files =
        {files,
         ["*.config", "src/*.app.src", "src/*.sh", "src/*.erl", "src/*/*.erl", "include/*.hrl"]},
    IgnoredFiles =
        case string:to_integer(
                 erlang:system_info(otp_release))
        of
            {N, []} when N >= 23 ->
                {ignore, ["src/*_ignore.erl", "src/ignored_file_config.erl"]};
            _ ->
                {ignore, ["src/*_ignore.erl", "src/ignored_file_config.erl", "src/otp23.erl"]}
        end,
    rebar_state:set(State1, format, [Files, IgnoredFiles]).

git_diff() ->
    case os:cmd("git --no-pager diff --no-index -- after formatted") of
        "" ->
            ok;
        Diff ->
            Unicode = unicode:characters_to_binary(Diff),
            ct:pal("Differences: ~n~s", [Unicode]),
            ct:fail(Unicode)
    end.
