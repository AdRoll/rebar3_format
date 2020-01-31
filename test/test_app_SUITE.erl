-module(test_app_SUITE).

-export([all/0, test_app/1]).

all() -> [test_app].

test_app(_Config) ->
    ok = file:set_cwd("../../../../test_app"),
    {ok, State1} = rebar3_format:init(rebar_state:new()),
    State2 = rebar_state:set(State1, format, [{files, ["src/*.erl", "include/*.hrl"]}]),
    State3 = rebar_state:command_parsed_args(State2, {[{output, "formatted"}], something}),
    {ok, State3} = rebar3_format_prv:do(State3),
    case os:cmd("git --no-pager diff --no-index -- after formatted") of
      "" -> ok;
      Diff ->
          Unicode = unicode:characters_to_binary(Diff),
          ct:pal("Differences:~n~s", [Unicode]),
          ct:fail(Unicode)
    end.

