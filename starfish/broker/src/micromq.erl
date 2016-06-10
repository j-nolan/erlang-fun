%%
%% Sequential echo server.
%%
-module(micromq).

%% API
-export([start_link/0, start_link_pid/0]).
-export([stop/0, stop/1]).
-export([handleSubscribe/2, handlePublish/3]). % temporary
% NOT API, for spawn.
-export([loop/2]).

-include("micromq.hrl").


%% ===================================================================
%% API.
%% ===================================================================

%% @doc Start the server and register it.
-spec start_link() -> ok. %% TODO ADD: | {error, Reason}
start_link() ->
	% Create the tables that contain:
	% - clients_by_topic: a bag of {Topic, ClientID}. Lists all ClientID
	%   matching a given topic
	% Todo: delete tables when server stops
	ets:new(clients_by_topic, [bag, named_table]),
	
	Pid = start_link_pid(),
	register(listener, Pid).

%% @doc Start the server and return its PID.
-spec start_link_pid() -> pid().
start_link_pid() ->
	spawn_link(fun() -> server(?PORT) end).

%% Stop the registered server.
-spec stop() -> ok.
stop() ->
	stop(whereis(listener)).

%% Stop the server with PID known.
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

% ACCEPTOR OF NEW CONNECTION 
% infinite loop that accepts new clients
-spec accept(LSocket) -> no_return() when
	LSocket :: inet:socket().
accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket), % WARNING Blocking!
	Pid = spawn(?MODULE, loop, [Socket, <<>>]),
	?LOG("Pid: ~p~n", [Pid]),
	accept(LSocket).

% CLIENT WORKER
% infinite loop that listen to clients message.
-spec loop(Socket, Bin) -> ok when
	Bin :: binary(),
	Socket :: inet:socket().
loop(Socket, Rest) ->
	Double = binary:compile_pattern([<<"\r\n\r\n">>, <<"\r\r">>, <<"\n\n">>]),
	%%Simple = binary:compile_pattern([<<"\r\n">>, <<"\r">>, <<"\n">>]),
	
	case binary:match(Rest, Double) of
		{Start, Len} ->
			Length = Start + Len,
			Current = binary:part(Rest, {0, Length}),
			Remainder = binary:part(Rest, {Length, byte_size(Rest) - Length}),
			?LOG("~p got a complete message...~p~n", [self(), Current]),
			Reply = Current,
			% TODO: use this.
			%Reply = parse(Current),
			gen_tcp:send(Socket, Reply),
			loop(Socket, Remainder);
		nomatch ->
			?LOG("~p is waiting for next chunk...~n", [self()]),
			case gen_tcp:recv(Socket, 0) of
				{ok, Bin} ->
					?LOG("~p got a TCP chunk ~p~n", [self(), Bin]),
					loop(Socket, <<Rest/binary,Bin/binary>>);
				Any ->
					?LOG("~p got unexpected TCP chunk: ~p ~n", [self(), Any]),
					exit({loop_unexpected_msg, Any})
			end
	end.

-spec parse(Request) -> Reply when
	Request :: binary(),
	Reply :: binary().
parse(Request) -> 
	ok. % Reply

-spec handleStatus(ClientID) -> Reply when
	ClientID :: integer(),
	Reply :: binary().
handleStatus(ClientID) -> 
	ok. % status d client

-spec handleSubscribe(ClientID, Topics) -> Reply when
	ClientID :: integer(),
	Topics :: list(),
	Reply :: binary().
handleSubscribe(ClientID, Topics) ->
	% Insert ClientID in all topics he subscribes to
	ets:insert(clients_by_topic, [ {Topic, ClientID} || Topic <- Topics]),
	<<"ok">>.

-spec handlePublish(ClientID, Topic, Message) -> Reply when
	ClientID :: integer(),
	Topic :: binary(),
	Message :: binary(),
	Reply :: binary().

handlePublish(ClientID, Topic, Message) ->
	% Retrieve list of clients from that topic
	Clients = ets:lookup(clients_by_topic, Topic),
	io:format("~p~n", [Clients]),
	<<"ok">>.
