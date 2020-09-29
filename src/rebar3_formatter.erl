%% @doc Automatic formatter for Erlang modules
-module(rebar3_formatter).

-export([new/3, format_file/2, ignore/2, action/1]).

-type opts() :: #{output_dir => none | current | file:filename_all(),
                  encoding => none | epp:source_encoding(),
                  action => verify | format,
                  _ => _}.
-type result() :: changed | unchanged.
-type state() :: term().

-opaque t() :: #{module := module(), opts := opts(), state := state()}.

-export_type([opts/0, result/0, t/0]).

%% Initialize the formatter and generate a state that will be passed in when
%% calling other callbacks
-callback init(opts(), undefined | rebar_state:t()) -> state().
%% Format a file.
%% Note that opts() are not the same as the global ones passed in on init/2.
%% These opts include per-file options specified with the -format attribute.
-callback format_file(file:filename_all(), state(), opts()) -> result().

%% @doc Build a formatter.
-spec new(module(), opts(), undefined | rebar_state:t()) -> t().
new(Module, Opts, RebarState) ->
    #{module => Module, opts => Opts, state => Module:init(Opts, RebarState)}.

%% @doc Format a file.
%%      Apply formatting rules to a file containing erlang code.
-spec format_file(file:filename_all(), t()) -> result().
format_file(File, #{opts := Opts, module := Module, state := State} = Formatter) ->
    case apply_per_file_opts(File, Opts) of
        ignore ->
            ignore(File, Formatter),
            unchanged;
        FileOpts ->
            Module:format_file(File, State, FileOpts)
    end.

%% @doc Process an ignored file.
%%      If output dir is not the current one we need to copy the files that we
%%      are not formatting to it
-spec ignore(file:filename_all(), t()) -> ok.
ignore(File, #{opts := #{output_dir := OutputDir}}) when not is_atom(OutputDir) ->
    OutFile =
        filename:join(
            filename:absname(OutputDir), File),
    ok = filelib:ensure_dir(OutFile),
    {ok, _} = file:copy(File, OutFile),
    ok;
ignore(_, _) ->
    ok.

%% @doc The action that the formatter will perform.
-spec action(t()) -> verify | format.
action(#{opts := Opts}) ->
    maps:get(action, Opts, format).

%% @doc We need to use quick_parse_file/1 here because the returned format
%%      is much more manageable than the one returned by parse_file/1
apply_per_file_opts(File, Opts) ->
    {ok, AST} = epp_dodger:quick_parse_file(File),
    FileOpts = [Opt || {attribute, _, format, Opt} <- AST],
    case lists:member(ignore, FileOpts) of
        true ->
            ignore;
        false ->
            MergeF = fun(Map, Acc) -> maps:merge(Acc, Map) end,
            lists:foldl(MergeF, Opts, FileOpts)
    end.
