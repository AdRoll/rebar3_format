-module(rebar3_format).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) -> rebar3_format_prv:init(State).
