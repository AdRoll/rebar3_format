%%% @doc Formatter to integrate with
%%%      <a target="_blank" href="https://github.com/whatsapp/erlfmt">erlfmt</a>.
-module(erlfmt_formatter).

-behaviour(rebar3_formatter).

-export([init/2, format_file/3]).

%% Initialize the formatter and generate a state that will be passed in when
%% calling other callbacks
-spec init(rebar3_formatter:opts(), undefined | rebar_state:t()) -> nostate.
init(_, _) ->
    {ok, _} = application:ensure_all_started(erlfmt),
    nostate.

%% @doc Format a file.
%%      Note that opts() are not the same as the global ones passed in on init/1.
%%      These opts include per-file options specified with the -format attribute.
-spec format_file(file:filename_all(), nostate, rebar3_formatter:opts()) ->
                     rebar3_formatter:result().
format_file(File, nostate, OptionsMap) ->
    {ok, Code} = file:read_file(File),
    Pragma =
        case maps:get(require_pragma, OptionsMap, false) of
            true ->
                require;
            false ->
                case maps:get(insert_pragma, OptionsMap, false) of
                    true ->
                        insert;
                    false ->
                        ignore
                end
        end,
    Options =
        case maps:get(print_width, OptionsMap, undefined) of
            undefined ->
                [{pragma, Pragma}];
            Width ->
                [{width, Width}, % support for v0.7.0
                 {print_width, Width},
                 {pragma, Pragma}]
        end,

    {Result, NewCode} =
        case erlfmt:format_file(File, Options) of
            {skip, Skipped} ->
                {unchanged, Skipped};
            {ok, Formatted, _} ->
                case unicode:characters_to_binary(Formatted) of
                    Code ->
                        {unchanged, Code};
                    FormattedBin ->
                        {changed, FormattedBin}
                end;
            {error, Reason} ->
                erlang:error(Reason)
        end,

    ok = maybe_save_file(maps:get(output_dir, OptionsMap), File, NewCode),
    Result.

maybe_save_file(none, _File, _Formatted) ->
    ok;
maybe_save_file(current, File, Formatted) ->
    file:write_file(File, Formatted);
maybe_save_file(OutputDir, File, Formatted) ->
    OutFile =
        filename:join(
            filename:absname(OutputDir), File),
    ok = filelib:ensure_dir(OutFile),
    file:write_file(OutFile, Formatted).
