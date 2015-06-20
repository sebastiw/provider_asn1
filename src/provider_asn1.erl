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
    Apps = rebar_state:project_apps(State),
    AllApps =
        case lists:member(rebar_state:dir(State), Apps) of
            true -> Apps;
            false -> [rebar_state:dir(State) | Apps]
        end,

    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(clean, Args) of
        true ->
            lists:foreach(fun (App) -> do_clean(State, App) end, AllApps);
        _ ->
            lists:foreach(fun (App) -> process_app(State, rebar_app_info:dir(App)) end, AllApps)
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

process_app(State, AppPath) ->
    ASNPath = filename:join(AppPath, "asn1"),
    GenPath = filename:join(AppPath, "asngen"),
    IncludePath = filename:join(AppPath, "include"),
    SrcPath = filename:join(AppPath, "src"),

    Asns = find_asn_files(ASNPath),
    verbose_out(State, "    Asns: ~p", [Asns]),
    verbose_out(State, "Making ~p ~p~n", [GenPath, file:make_dir(GenPath)]),
    lists:foreach(fun(AsnFile) -> generate_asn(State, GenPath, AsnFile) end, Asns),

    verbose_out(State, "ERL files: ~p", [filelib:wildcard("*.erl", GenPath)]),
    move_files(State, GenPath, SrcPath, "*.erl"),

    verbose_out(State, "DB files: ~p", [filelib:wildcard("*.asn1db", GenPath)]),
    move_files(State, GenPath, SrcPath, "*.asn1db"),

    verbose_out(State, "HEADER files: ~p", [filelib:wildcard("*.hrl", GenPath)]),
    move_files(State, GenPath, IncludePath, "*.hrl"),

    verbose_out(State, "BEAM files: ~p", [filelib:wildcard("*.beam", GenPath)]),
    delete_files(State, GenPath, "*.beam"),

    ok.

move_files(State, From, To, Pattern) ->
    verbose_out(State, "Making ~p ~p~n", [To, file:make_dir(To)]),
    lists:foreach(fun(File) -> move_file(State, From, File, To) end,
                  filelib:wildcard(Pattern, From)).

move_file(State, SrcPath, File, DestPath) ->
    F = filename:join(SrcPath, File),
    Dest = filename:join(DestPath, File),
    verbose_out(State, "Moving: ~p", [F]),
    verbose_out(State, "~p", [file:copy(F, Dest)]).

delete_files(State, In, Pattern) ->
    lists:foreach(fun(File) ->
                          delete_file(State, In, File)
                  end,
                  filelib:wildcard(Pattern, In)).

delete_file(State, In, File) ->
    F = filename:join(In, File),
    verbose_out(State, "Deleting: ~p", [F]),
    verbose_out(State, "~p", [file:delete(F)]).

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


do_clean(State, App) ->
    rebar_api:info("Cleaning ASN.1 generated files.", []),

    AppPath = rebar_app_info:dir(App),
    GenPath = filename:join(AppPath, "asngen"),
    IncludePath = filename:join(AppPath, "include"),
    SrcPath = filename:join(AppPath, "src"),

    ErlFiles = filelib:wildcard("*.erl", GenPath),
    verbose_out(State, "Erl files: ~p", [ErlFiles]),
    lists:foreach(fun(File) ->
                          delete_file(State, SrcPath, File)
                  end, ErlFiles),

    HrlFiles = filelib:wildcard("*.hrl", GenPath),
    verbose_out(State, "Hrl files: ~p", [HrlFiles]),
    lists:foreach(fun(File) ->
                          delete_file(State, IncludePath, File)
                  end, HrlFiles),

    DBFiles = filelib:wildcard("*.asn1db", GenPath),
    verbose_out(State, "DB files: ~p", [DBFiles]),
    lists:foreach(fun(File) ->
                          delete_file(State, SrcPath, File)
                  end, DBFiles),

    delete_files(State, GenPath, "*"),

    ok.

verbose_out(State, FormatString, Args)->
    {CommArgs, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(verbose, CommArgs) of
        true ->
            rebar_api:info(FormatString, Args);
        _ ->
            ok
    end.
