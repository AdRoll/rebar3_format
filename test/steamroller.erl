%%% @doc This module is only used for tests to simulate the actual
%%%      steamroller from old-reliable's steamroller.
-module(steamroller).

-export([opts/2, format_file/2, validator/1]).

-spec opts(proplists:proplist(), rebar_state:t()) -> proplists:proplist().
opts(Opts, _) ->
    Opts.

-spec validator(fun((binary(), [any()]) -> ok | {error, any()})) -> ok.
validator(Fun) ->
    application:set_env(rebar3_format, steamroller_formatter_validator, Fun).

-spec format_file(binary(), [any()]) -> ok | {error, any()}.
format_file(File, Opts) ->
    Validator =
        application:get_env(rebar3_format,
                            steamroller_formatter_validator,
                            fun(_, _) ->
                                   ok
                            end),
    Validator(File, Opts).
