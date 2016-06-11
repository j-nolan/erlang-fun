%%
%% Sequential echo server.
%%
-module(micromq).

%% API
-export([start_link/0, start_link/1, stop/0]).
-export([handleSubscribe/2, handlePublish/3]). % temporary
% NOT API, for spawn.
-export([loop/2]).

-include("micromq.hrl").


%% ===================================================================
%% API.
%% ===================================================================

%% @doc Start the server with default options, register it and initialize ressources.
-spec start_link() -> ok | {error, Reason} when
	Reason :: system_limit | inet:posix().
start_link() ->
	create_options(),
	start_link_internal().

%% @doc stop the registered server and liberate ressources. Does nothing if server is already down.
-spec stop() -> ok.
stop() ->
	% Search for the server 
	Pid = whereis(server),
	case Pid of
		undefined -> 
			?LOG("STOP Controller: no server running.~n"),
			ok;
		_ -> 
			% Liberate the ressources used by the server.
			unregister(server),
			ets:delete(clients_by_topic),
			ets:delete(options),
			Pid ! stop,
			?LOG("STOP Controller: server stopped and data structure liberated.~n")
	end.

%% @doc Start the server with custum options, register it and initialize ressources.
-type option() ::
	{address, Address::binary()} |
	{port, Port::integer()} |
	{max_topics, N::integer()} |
	{max_clients, N::integer()} |
	{verbosity, V::boolean()}.
-spec start_link(Options) -> ok | {error, Reason} when 
	Options :: [option()],
	Reason :: system_limit | inet:posix().
start_link(Options) ->
	create_options(),
	?LOG("Controller: parsing launch options.~n"),
	handle_options(Options),
	start_link_internal().

%% ===================================================================
%% Private functions.
%% ===================================================================

% Creates a default set of options with the macros (only if no server runninh yet).
-spec create_options() -> ok.
create_options() ->
	% check if the server is already running
	Server = whereis(server),
	case Server of
		% Server not running -> proceed.
		undefined ->
			ets:new(options, [set, named_table]),
			ets:insert(options, {address, ?ADDRESS}),
			ets:insert(options, {port, ?PORT}),
			ets:insert(options, {max_topics, ?MAX_TOPICS}),
			ets:insert(options, {max_clients, ?MAX_CLIENTS}),
			ets:insert(options, {verbosity, ?VERBOSITY});
		_ -> % server already running -> abort.
			ok
	end,
	ok.

% Override default options with the given options (the last one takes priority).
-spec handle_options(Options) -> ok when 
	Options :: [option()].
handle_options(Options) ->
	case Options of
		[] -> 
			ok;
		[H|T] ->
			% H is a tuple {options, Value}
			ets:insert(options, H),
			handle_options(T)
	end.

-spec start_link_internal() -> ok | {error, Reason} when
	Reason :: system_limit | inet:posix().
start_link_internal() ->
	?LOG("START Controller: trying to start the server.~n"),
	[{port,Port}|_] = ets:lookup(options, port),
	[{address,Address}|_] = ets:lookup(options, address),
	% getaddr(Host, Family) -> {ok, Address} | {error, posix()}
	{ok, RealAddress} = inet:getaddr(Address, inet),
	?LOG("Starting on: ~p (~p), Port: ~p.~n", [Address, RealAddress, Port]),
	Listen = gen_tcp:listen(Port, [binary, {reuseaddr, true},
		{active, false}, {ifaddr, RealAddress}]), % , {ifaddr, Address}
	case Listen of 
		{error, Reason} ->
			?LOG("START Controller: coudln't start the server. Do you already have a running instance? Try to stop it first~n"),
			{error, Reason};
		{ok, LSocket} ->
			% Create the tables that contain:
			% - clients_by_topic: a bag of {Topic, ClientID}. Lists all ClientID
			%   matching a given topic
			ets:new(clients_by_topic, [bag, named_table]),
			Pid = spawn_link(fun() -> server(LSocket) end),
			gen_tcp:controlling_process(LSocket, Pid),
			register(server, Pid),
			?LOG("Controller: server started and data structure initialized.~n"),
			ok
	end.

-spec server(LSocket) -> ok when
	LSocket:: inet:socket().
server(LSocket) ->

	% Since we don't close the listen socket and this server is sequential,
	% subsequent clients will be allowed to connect until the kernel listen
	% backlog fills up, which is fine.

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
	
	case binary:match(Rest, Double) of
		{Start, Len} ->
			Length = Start + Len,
			Current = binary:part(Rest, {0, Start}), % discard the double return separation.
			Remainder = binary:part(Rest, {Length, byte_size(Rest) - Length}),
			?LOG("~p got a complete message...~p~n", [self(), Current]),
			Reply = parse(Current),
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

% Parses any binary request received, determines the correct type of request based on prefix,
% makes use of dedicated parser for certain request, does the appropriate call and returns the reply as a binary.
% Returns a '400 Bad Request' binary when the format is not met or the protocol violated.
-spec parse(Request) -> Reply when
	Request :: binary(),
	Reply :: binary().
parse(Request) -> 
	case Request of
		<<"topic:", Payload/binary>> ->
			parse_publish(Payload);			
		<<"subscribe:", Payload/binary>> ->
			parse_subscribe(Payload);
		<<"status">> ->
			%TODO: handleStatus(Client_ID),
			<<"Got: status\n">>;
		<<"help">> ->
			<<"Welcome! I am a simple MQTT server.\n
			Allowed commands are: 'topic:', 'subscribe:', 'status' and 'help'\n
			Simple line-return does nothing and allows multi-line commands and multi-line text body\n
			Double line-return commits your command to the server\n
			\n
			If your command is not supported or malformed, you'll get a '400 Bad Request' message,
			with some extra information if the server is started with VERBOSE option true.
			Warning: all commands headers are case-sensitive!\n
			\n
			Publish:\n
			$> topic: <topic name>\n
			$> body: <msg body>\n
			This command MUST contain at least 2 lines, the first for 'topic:', the second for 'body:'\n
			<topic name> MUST not contain commas or line-return, but MAY contains spaces.\n
			<msg body> MAY be any text and contain single line-return and commas\n
			\n
			Subscribe: \n
			$> subscribe: <topic1> [ , <topic2>, ... ]\n
			Topics MUST be a on single line.
			<topicX> MUST not contain commas or line-return, but MAY contains spaces.\n">>;
		_ -> 
			bad_request(<<"Command not found: '", Request/binary, "'.">>)
	end.

% Parse a publish request payload, does the appropriate call and returns the reply.
-spec parse_publish(Payload) -> binary() when
	Payload :: binary().
parse_publish(Payload) ->
	Simple = binary:compile_pattern([<<"\r\n">>, <<"\r">>, <<"\n">>]),
	% Check the double-line
	case binary:match(Payload, Simple) of
		nomatch ->
			bad_request(<<"Publish command MUST contain at least 2 lines.\n
				$> topic: <topic name>\n
				$> body: <msg body>">>);
		{Start, Len} ->
			% Extract the first line
			Topic = binary:part(Payload, 0, Start),
			% Checks the first line is a single item (no commas), by splitting globally and check the length is 0.
			Topics = binary:split(Payload, <<",">>, [trim_all, global]),
			case length(Topics) of
				1 ->
					% Extract the second line
					Discard = Start+Len, % length of topic data + return line.
					Bodydata = binary:part(Payload, Discard, size(Payload) - Discard),
					% Second line must start with body: and extract the rest as a message.
					case binary:match(Bodydata, <<"body:">>) of
						{0, L} ->
							Body = binary:part(Bodydata, L, size(Bodydata) - L),
							%TODO: handlePublish(Client_ID, Topic, Body),
							<<"Got: topic: ", Topic/binary, ", body:", Body/binary>>;
						_ ->
							bad_request(<<"2nd line of publish command does not comply to body format:
								$> body: <msg body>
								where <msg body> MAY be any text and contain single line-return and commas">>)
					end;
				_ ->
					bad_request(<<"1st line of publish command does not to topic format
						$> topic: <topic name>
						where <topic name> MUST not contain commas or line-return, but MAY contains spaces.">>)
			end
		
	end.

% Parse a subscribe request payload, does the appropriate call and returns the reply.
-spec parse_subscribe(Payload) -> binary() when
	Payload :: binary().
parse_subscribe(Payload) ->
	Simple = binary:compile_pattern([<<"\r\n">>, <<"\r">>, <<"\n">>]),
	% Check the single line.
	case binary:match(Payload, Simple) of
		{_, _} ->
			bad_request(<<"subscribe command items MUST be on a single line.
				$> subscribe: <topic1> [ , <topic2>, ... ]">>);
		nomatch ->
			% Split the whole payload and trim all leading and trailing spaces
			Topics = binary:split(Payload, <<",">>, [trim_all, global]),
			%TODO: handleSubscribe(Client_ID, Topics),
			<<"Got: subscribe: ", Payload/binary>>
	end.

% Returns a generic 400 Bad Request.
-spec bad_request() -> binary().
bad_request() ->
	<<"400 Bad Request\n">>.

% Returns a message specific 400 Bad Request.
-spec bad_request(Message) -> binary() when
	Message :: binary().
bad_request(Message) ->
	BR = bad_request(),
	[{verbosity, V}|_] = ets:lookup(options, verbosity),
	case V of
		false -> BR;
		_ -> 
			<<BR/binary, "Enter 'help' for indications.\nMessage : ", Message/binary, "\n">>
	end.

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
