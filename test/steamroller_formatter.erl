%%% @doc This module is only used for tests to simulate the actual
%%%      steamroller_formatter from old-reliable's steamroller.
-module(steamroller_formatter).

-export([format/2, validator/1]).

-spec validator(fun((binary(), [any()]) -> ok | {error, any()})) -> ok.
validator(Fun) ->
    application:set_env(rebar3_format, steamroller_formatter_validator, Fun).

-spec format(binary(), [any()]) -> ok | {error, any()}.
format(File, Opts) ->
    Validator = application:get_env(rebar3_format,
                                    steamroller_formatter_validator,
                                    fun (_, _) ->
                                            ok
                                    end),
    Validator(File, Opts).
