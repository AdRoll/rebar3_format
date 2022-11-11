%%% @doc Main entry point for the rebar3 format plugin
-module(rebar3_format).

-elvis({elvis_style, no_call, disable}).

-hank({unnecessary_function_argument, [{main, 1, 1}]}).

-export([init/1, main/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_format_prv:init(State).

%% @private
-spec main(any()) -> no_return().
main(_) ->
    Msg = "rebar3_format shall be run as a plugin, not as an escript.~n",

    % Not possible (no other module found in an escript context):
    %rebar_api:abort(Msg, []).
    io:format(Msg),
    exit(escript_not_supported).
