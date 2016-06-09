%%
%% Sequential echo server.
%%
-module(broker).

%% API
-export([start_link/0, stop/1, loop/2]).

-include("broker.hrl").


%% ===================================================================
%% API.
%% ===================================================================

%% @doc Start the echo server.
-spec start_link() -> pid().
start_link() ->
    %spawn_link(fun() -> server(-1) end). % <= Try this with the dialyzer!
    spawn_link(fun() -> server(?PORT) end).

%% Stop the echo server.
-spec stop(Pid) -> stop when
    Pid :: pid().
stop(Pid) ->
    Pid ! stop.


%% ===================================================================
%% Private functions.
%% ===================================================================

-spec server(Port) -> true when
    Port :: inet:port_number().
server(Port) ->
    % Since we don't close the listen socket and this server is sequential,
    % subsequent clients will be allowed to connect until the kernel listen
    % backlog fills up, which is fine.
    {ok, LSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true},
        {active, false}]), % WARNING No backpressure!
    % Design point: Why spawning another process instead of just calling
    % `accept/1` directly? We spawn another process to allow the main one to
    % be always reactive to Erlang messages, despite the fact that
    % `gen_tcp:accept/1` is blocking in a unnatural way for Erlang (outside a
    % `receive` block).
    Acceptor = spawn_link(fun() -> accept(LSocket) end),
    receive
        stop ->
            ?LOG("Server: stopping~n"),
            % Terminate the acceptor also if it is blocked.
            exit(Acceptor, stop);
        Any ->
            ?LOG("Server: unexpected msg ~p~n", [Any]),
            exit({server_unexpected_msg, Any})
    end.

-spec accept(LSocket) -> no_return() when
    LSocket :: inet:socket().
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket), % WARNING Blocking!
    Pid = spawn(?MODULE, loop, [Socket, <<>>]),
    ?LOG("Pid: ~p~n", [Pid]),
    accept(LSocket).

-spec loop(Socket, Bin) -> ok when
	Bin :: erlang:binary(),
    Socket :: inet:socket().
loop(Socket, Rest) ->
	case Rest of
		% Todo: Variable size instead of 5
		<<Any:5/binary, "\r\n\r\n", Next/binary>> ->
			?LOG("~p got a complete message...~p~n", [self(), Any]),
			gen_tcp:send(Socket, Any),
			loop(Socket, Next);			
		_ ->
			?LOG("~p is waiting for next message...~n", [self()]),
			case gen_tcp:recv(Socket, 0) of
				{ok, Bin} ->
					?LOG("~p got tcp ~p~n", [self(), Bin]),
					loop(Socket, <<Rest/binary,Bin/binary>>);
		        Any ->
					?LOG("~p got unexpected ~p ~n", [self(), Any]),
		            exit({loop_unexpected_msg, Any})
			end
	end.

