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
    Path = filename:join(rebar_app_info:dir(App), "asn1"),
    Asns = find_asn_files(Path),
    io:format("    Asns: ~p~n", [Asns]), 
    lists:foreach(fun(AsnFile) -> generate_asn(Path, AsnFile) end, Asns),
    ok.

find_asn_files(Path) ->
    [filename:join(Path, F) || F <- filelib:wildcard("*.asn1", Path)].

generate_asn(Path, AsnFile) ->
    io:format("Generating ASN.1 stuff.~n"),
    asn1ct:compile(AsnFile, [ber, verbose, {outdir, filename:join(Path, "include")}]).
