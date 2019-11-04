-module(rebar3_formatter).

-export([format/2]).

-spec format(map(), file:filename_all()) -> ok.
format(File, Opts) ->
    rebar_api:debug("Formatting ~p with ~p", [File, Opts]),
    case filelib:is_regular(File) of
        true -> ok;
        false -> erlang:error({enoent, File})
    end.
