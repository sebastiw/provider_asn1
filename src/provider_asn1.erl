-module('provider_asn1').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'asn').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 asn"},      % How to use the plugin
            % list of options understood by the plugin
            {opts, [{clean, $c, "clean", {boolean, false}, "Clean ASN.1 generated files"},
                    {verbose, $v, "verbose", {boolean, false}, "Be Verbose."}]},
            {short_desc, "Compile ASN.1 with Rebar3"},
            {desc, "Compile ASN.1 with Rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(clean, Args) of
        true ->
            do_clean(State);
        _ ->
            lists:foreach(fun (App) -> process_app(State, App) end, rebar_state:project_apps(State))
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

process_app(State, App) ->
    AppPath = rebar_app_info:dir(App),
    ASNPath = filename:join(AppPath, "asn1"),
    GenPath = filename:join(AppPath, "asngen"),
    IncludePath = filename:join(AppPath, "include"),
    SrcPath = filename:join(AppPath, "src"),

    Asns = find_asn_files(ASNPath),
    verbose_out(State, "    Asns: ~p", [Asns]),
    lists:foreach(fun(AsnFile) -> generate_asn(State, GenPath, AsnFile) end, Asns),

    verbose_out(State, "ERL files: ~p", [filelib:wildcard("*.erl", GenPath)]),
    make_dir(SrcPath),
    lists:foreach(fun(ErlFile) ->
                          F = filename:join(GenPath, ErlFile),
                          Dest = filename:join(SrcPath, ErlFile),
                          verbose_out(State, "Moving: ~p", [F]),
                          verbose_out(State, "~p", [file:copy(F, Dest)]) end,
                  filelib:wildcard("*.erl", GenPath)),

    verbose_out(State, "DB files: ~p", [filelib:wildcard("*.asn1db", GenPath)]),
    lists:foreach(fun(DBFile) ->
                          F = filename:join(GenPath, DBFile),
                          Dest = filename:join(SrcPath, DBFile),
                          verbose_out(State, "Moving: ~p", [F]),
                          verbose_out(State, "~p", [file:copy(F, Dest)]) end,
                  filelib:wildcard("*.asn1db", GenPath)),

    verbose_out(State, "HEADER files: ~p", [filelib:wildcard("*.hrl", GenPath)]),
    make_dir(IncludePath),
    lists:foreach(fun(DBFile) ->
                          F = filename:join(GenPath, DBFile),
                          Dest = filename:join(IncludePath, DBFile),
                          verbose_out(State, "Moving: ~p", [F]),
                          verbose_out(State, "~p", [file:copy(F, Dest)]) end,
                  filelib:wildcard("*.hrl", GenPath)),

    verbose_out(State, "BEAM files: ~p", [filelib:wildcard("*.beam", GenPath)]),
    lists:foreach(fun(BeamFile) ->
                          F = filename:join(GenPath, BeamFile),
                          verbose_out(State, "Deleting: ~p", [F]),
                          verbose_out(State, "~p", [file:delete(F)]) end,
                  filelib:wildcard("*.beam", GenPath)),
    ok.

find_asn_files(Path) ->
    [filename:join(Path, F) || F <- filelib:wildcard("*.asn1", Path)].

generate_asn(State, Path, AsnFile) ->
    rebar_api:info("Generating ASN.1 files.", []),
    {Args, _} = rebar_state:command_parsed_args(State),
    CompileArgs =
        case proplists:get_value(verbose, Args) of
            true -> [ber, verbose, {outdir, Path}];
            _ -> [ber, {outdir, Path}]
        end,
    asn1ct:compile(AsnFile, CompileArgs).


do_clean(_State) ->
    ok.

verbose_out(State, FormatString, Args)->
    {CommArgs, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(verbose, CommArgs) of
        true ->
            rebar_api:info(FormatString, Args);
        _ ->
            ok
    end.
