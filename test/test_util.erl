-module(test_util).

-export([init/0, get_app_dir/0]).

init() ->
    {ok, State0} =
        rebar_prv_app_discovery:do(
            rebar_state:new()),
    rebar3_format:init(State0).

get_app_dir() ->
    filename:join(
        filename:dirname(?FILE), "../test_app").
