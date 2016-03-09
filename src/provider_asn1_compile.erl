-module('provider_asn1_compile').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-import(provider_asn1_util, 
        [verbose_out/3,
         move_files/4,
         move_file/4,
         delete_files/3,
         delete_file/3]).

-define(PROVIDER, 'compile').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},               % The 'user friendly' name of the task
            {module, ?MODULE},               % The module implementation of the task
            {namespace, asn},                % Compile resides in the asn namespace.
            {bare, true},                    % The task can be run by the user, always true
            {deps, ?DEPS},                   % The list of dependencies
            {example, "rebar3 asn compile"}, % How to use the plugin
            % list of options understood by the plugin
            {opts, [{verbose, $v, "verbose", {boolean, false}, "Be Verbose."}]},
            {short_desc, "Compile ASN.1 with Rebar3"},
            {desc, "Compile ASN.1 with Rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = lists:map(fun (App) -> rebar_app_info:dir(App) end,
                     rebar_state:project_apps(State)),
    AllApps =
        case lists:member(rebar_state:dir(State), Apps) of
            true -> Apps;
            false -> [rebar_state:dir(State) | Apps]
        end,
    
    lists:foreach(fun (App) -> process_app(State, App) end, AllApps),
    {ok, State}.

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

format_error(Reason) ->
    provider_asn1_util:format_error(Reason).

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
