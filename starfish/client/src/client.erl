-module(client).

%% client: client library's entry point.

-export([start/0]).


%% API

start() ->
    client().

%% Internals

client() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678, 
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).

%% End of Module.
