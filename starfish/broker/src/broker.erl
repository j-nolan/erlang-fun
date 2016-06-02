-module(broker).

%% broker: broker library's entry point.

-export([start/0]).


%% API

start() ->
    init_wait().

%% Internals
init_wait() ->
    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, 
                                        {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = loop(Sock, []),
    ok = gen_tcp:close(Sock),
    Bin.

loop(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            loop(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

%% End of Module.
