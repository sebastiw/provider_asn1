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
            {example, "rebar3 asn"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Compile ASN.1 with Rebar3"},
            {desc, "Compile ASN.1 with Rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    lists:foreach(fun process_app/1, rebar_state:project_apps(State)),
%    io:format("Project apps: ~p~n", [rebar_state:project_apps(State)]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

process_app(App) ->
    AppPath = rebar_app_info:dir(App),
    ASNPath = filename:join(AppPath, "asn1"),
    IncludePath = filename:join(AppPath, "include"),
    SrcPath = filename:join(AppPath, "src"),

    Asns = find_asn_files(ASNPath),
    io:format("    Asns: ~p~n", [Asns]), 
    lists:foreach(fun(AsnFile) -> generate_asn(AppPath, AsnFile) end, Asns),

    io:format("ERL files: ~p~n", [filelib:wildcard("*.erl", IncludePath)]),
    lists:foreach(fun(ErlFile) -> 
                          F = filename:join(IncludePath, ErlFile),
                          io:format("Moving: ~p~n", [F]),
                          io:format("~p~n", [file:rename(F, SrcPath)]) end, 
                  filelib:wildcard("*.erl", IncludePath)),
    
    io:format("DB files: ~p~n", [filelib:wildcard("*.asn1db", IncludePath)]),
    lists:foreach(fun(DBFile) -> 
                          F = filename:join(IncludePath, DBFile),
                          io:format("Moving: ~p~n", [F]),
                          io:format("~p~n", [file:rename(F, SrcPath)]) end, 
                  filelib:wildcard("*.asn1db", IncludePath)),

    io:format("BEAM files: ~p~n", [filelib:wildcard("*.beam", IncludePath)]),
    lists:foreach(fun(BeamFile) -> 
                          F = filename:join(IncludePath, BeamFile),
                          io:format("Moving: ~p~n", [F]),
                          io:format("~p~n", [file:delete(F)]) end, 
                  filelib:wildcard("*.beam", IncludePath)),
    ok.

find_asn_files(Path) ->
    [filename:join(Path, F) || F <- filelib:wildcard("*.asn1", Path)].

generate_asn(Path, AsnFile) ->
    io:format("Generating ASN.1 stuff.~n"),
    asn1ct:compile(AsnFile, [ber, verbose, {outdir, filename:join(Path, "include")}]).
    
