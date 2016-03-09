-module(provider_asn1_util).

-export([verbose_out/3,
         format_error/1,
         move_files/4,
         move_file/4,
         delete_files/3,
         delete_file/3]).

verbose_out(State, FormatString, Args)->
    {CommArgs, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(verbose, CommArgs) of
        true ->
            rebar_api:info(FormatString, Args);
        _ ->
            ok
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

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
