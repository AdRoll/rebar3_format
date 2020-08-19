%%% @doc This module is only used for tests to simulate the actual erlfmt
-module(erlfmt).

-export([format_file/2, format_file/3, validator/1]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.
-type out() :: standard_out | {path, file:name_all()} | replace.
-type pragma() :: require | insert | ignore.
-type config() :: [{pragma, pragma()} | {width, pos_integer()}].

-spec validator(fun((file:name_all(), out()) ->
                        {ok, [error_info()]} | {error, error_info()})) ->
                   ok.
validator(Fun) ->
    application:set_env(rebar3_format, erlfmt_formatter_validator, Fun).

-spec format_file(file:name_all(), out()) -> {ok, [error_info()]} | {error, error_info()}.
format_file(File, Opts) ->
    Validator = validator(),
    Validator(File, Opts).

-spec format_file(file:name_all(), out(), config()) ->
                     {ok, [error_info()]} | {error, error_info()}.
format_file(File, Out, Opts) ->
    case validator() of
        Validator when is_function(Validator, 3) ->
            Validator(File, Out, Opts);
        _ ->
            erlang:error(undef)
    end.

validator() ->
    application:get_env(rebar3_format,
                        erlfmt_formatter_validator,
                        fun (_, _) ->
                                ok
                        end).
