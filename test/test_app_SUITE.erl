-module(test_app_SUITE).

-export([all/0, test_app/1]).

all() ->
    [test_app].

test_app(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} = rebar3_format:init(rebar_state:new()),
    Files = {files, ["src/*.erl", "include/*.hrl"]},
    IgnoredFiles =
        case string:to_integer(erlang:system_info(otp_release)) of
          {N, []} when N >= 23 ->
              {ignore, ["src/*_ignore.erl", "src/ignored_file_config.erl"]};
          _ ->
              {ignore, ["src/*_ignore.erl", "src/ignored_file_config.erl", "src/otp23.erl"]}
        end,
    State2 = rebar_state:set(State1, format, [Files, IgnoredFiles]),
    {error, _} = verify(State2),
    {ok, _} = format(State2),
    {error, _} = verify(State2),
    ok = file:set_cwd("formatted"),
    {ok, _} = verify(State2),
    ok = file:set_cwd(".."),
    ok = git_diff().

verify(State) ->
    rebar3_format_prv:do(rebar_state:command_parsed_args(State,
                                                         {[{verify, true}], something})).

format(State) ->
    rebar3_format_prv:do(rebar_state:command_parsed_args(State,
                                                         {[{output, "formatted"}], something})).

git_diff() ->
    case os:cmd("git --no-pager diff --no-index -- after formatted") of
      "" ->
          ok;
      Diff ->
          Unicode = unicode:characters_to_binary(Diff),
          ct:pal("Differences:~n~s", [Unicode]),
          ct:fail(Unicode)
    end.
