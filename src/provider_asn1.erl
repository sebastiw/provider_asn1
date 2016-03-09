-module('provider_asn1').

-export([init/1]).

init(State) ->
    lists:foldl(fun provider_init/2, {ok, State}, [provider_asn_compile
                                                  ,provider_asn_clean]).

provider_init(Module, {ok, State}) ->
    Module:init(State).
