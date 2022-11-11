%%% @doc Main entry point for the rebar3 format plugin
-module(rebar3_format).

-export([init/1, main/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_format_prv:init(State).

%% @private
-spec main(any()) -> no_return().
main(_) ->
    rebar_api:abort("rebar3_format shall be run as a plugin, not as an escript.", []).
