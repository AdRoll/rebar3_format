%%% @doc Main entry point for the rebar3 format plugin
-module(rebar3_format).

-export([init/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_format_prv:init(State).
