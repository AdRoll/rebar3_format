%%% @doc This module is only used for tests to simulate the actual erlfmt
-module(erlfmt).

-export([format_file/2, validator/1]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.
-type pragma() :: require | insert | ignore.
-type config() :: [{pragma, pragma()} | {width, pos_integer()}].

-spec validator(fun((file:name_all() | stdin, config()) ->
                        {ok, [unicode:chardata()], [error_info()]} |
                        {skip, string()} |
                        {error, error_info()})) ->
                   ok.
validator(Fun) ->
    application:set_env(rebar3_format, erlfmt_formatter_validator, Fun).

%% @doc First clause is 0.7.0, second one 0.6.0
-spec format_file(file:name_all() | stdin, config()) ->
                     {ok, [unicode:chardata()], [error_info()]} |
                     {skip, string()} |
                     {error, error_info()}.
format_file(File, Options) ->
    Validator = validator(),
    Validator(File, Options).

validator() ->
    application:get_env(rebar3_format, erlfmt_formatter_validator, fun(_, _) -> ok end).
