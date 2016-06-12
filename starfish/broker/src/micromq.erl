%%
%% Parralel micro-MQTT server.
%%
-module(micromq).

%% API
-export([start_link/0, start_link/1, stop/0]).
-include("micromq.hrl").

%% TESTS ONLY
-ifdef(TEST).
-export([
	parse/2,
	parse_publish/2,
	parse_subscribe/2,
	bad_request/0,
	bad_request/1,
	handleStatus/1,
	handleSubscribe/2,
	handlePublish/3,
	distribute/2
	]).
-endif.


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
			log("STOP Controller: no server running.~n"),
			ok;
		_ -> 
			% Liberate the ressources used by the server.
			unregister(server),
			Pid ! stop,
			ets:delete(clients_by_topic),
			ets:delete(clients_records),
			ets:delete(options),
			log("STOP Controller: server stopped and data structure liberated.~n")
	end.

%% @doc Start the server with custum options, register it and initialize ressources.
-type option() ::
	{address, Address::binary()} |
	{port, Port::integer()} |
	{max_topics, N::integer()} |
	{max_clients, N::integer()} |
	{verbosity, V::atom()}.
-spec start_link(Options) -> ok | {error, Reason} when 
	Options :: [option()],
	Reason :: system_limit | inet:posix().
start_link(Options) ->
	create_options(),
	log("Controller: parsing launch options.~n"),
	handle_options(Options),
	start_link_internal().

%% ===================================================================
%% Private functions.
%% ===================================================================

%% Logs utilities, to easily disable/enable logs (to be used instead of io:format()!)
%% ===================================================================
-spec log(Fmt) -> ok when
	Fmt :: io:format().
log(Fmt) ->
	log(Fmt, []).

-spec log(Fmt, Args) -> ok when
	Fmt :: io:format(),
	Args :: [term()]. 
log(Fmt, Args)->
	case ?LOGS of
		true ->
			io:format(Fmt, Args); % print the logs
		_ ->
			ok % keep quiet
	end.

%% Start-up option helpers (create option set, handle custum option)
%% ===================================================================

% Creates a default set of options with the macros (only if no server runninh yet).
-spec create_options() -> ok.
create_options() ->
	% check if the server is already running
	Server = whereis(server),
	case Server of
		% Server not running -> proceed.
		undefined ->
			ets:new(options, [set, named_table, protected]), %explicitely protected
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

%% Internal start-up function and initializer of internal structures
%% ===================================================================
-spec start_link_internal() -> ok | {error, Reason} when
	Reason :: system_limit | inet:posix().
start_link_internal() ->
	log("START Controller: trying to start the server.~n"),
	[{port,Port}|_] = ets:lookup(options, port),
	[{address,Address}|_] = ets:lookup(options, address),
	% getaddr(Host, Family) -> {ok, Address} | {error, posix()}
	{ok, RealAddress} = inet:getaddr(Address, inet),
	log("Starting on: ~p (~p), Port: ~p.~n", [Address, RealAddress, Port]),
	Listen = gen_tcp:listen(Port, [binary, {reuseaddr, true},
		{active, false}, {ifaddr, RealAddress}]), % , {ifaddr, Address}
	case Listen of 
		{error, Reason} ->
			log("START Controller: coudln't start the server. Do you already have a running instance? Try to stop it first~n"),
			{error, Reason};
		{ok, LSocket} ->
			% Tables are public because all processes may add data!
			% Create the tables that contain:
			% - clients_by_topic: a bag of {Topic, ClientID}. Lists all ClientID
			%   matching a given topic
			ets:new(clients_by_topic, [bag, named_table, public]),
			% Clients Records: a set of {ClientID, Socket, [TopicSubscribed], [TopicPublished], NbMsgReceived, NbMsgSent}
			ets:new(clients_records, [set, named_table, public]),

			Pid = spawn_link(fun() -> server(LSocket) end),
			gen_tcp:controlling_process(LSocket, Pid),
			register(server, Pid),
			log("Controller: server started and data structure initialized.~n"),
			ok
	end.

%% Server code: top-supervisor process (spawn the Acceptor and handles tear-down)
%% ===================================================================
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
	Acceptor = spawn_link(fun() -> accept(LSocket, 1) end),
	receive
		stop ->
			log("Server: stopping~n"),
			% Terminate the acceptor also if it is blocked.
			exit(Acceptor, stop);
		Any ->
			log("Server: unexpected msg ~p~n", [Any]),
			exit({server_unexpected_msg, Any})
	end.

%% ACCEPTOR of New Connections : infinite loop that accepts new clients
%% ===================================================================
-spec accept(LSocket, ClientID) -> no_return() when
	LSocket :: inet:socket(),
	ClientID :: integer().
accept(LSocket, ClientID) ->
	{ok, Socket} = gen_tcp:accept(LSocket), % WARNING Blocking!
	% {ClientID, Socket, [TopicSubscribed], [TopicPublished], NbMsgReceived, NbMsgSent}
	ets:insert(clients_records, {ClientID, Socket, [], [], 0, 0}),
	log("~p (Acceptor) got a new client with ID: ~p~n", [self(), ClientID]),
	%Pid = spawn(?MODULE, worker, [Socket, ClientID]),
	Pid = spawn_link(fun() -> worker(Socket, ClientID) end),
	log("Pid: ~p~n", [Pid]),
	accept(LSocket, ClientID + 1).

%% CLIENT WORKER : Initiate client and calls the looper.
%% ===================================================================
-spec worker(Socket, ClientID) -> ok when
	Socket :: inet:socket(),
	ClientID :: integer().
worker(Socket, ClientID) ->
	log("~p (Worker) started for client with ID: ~p~n", [self(), ClientID]),
	Cid = integer_to_binary(ClientID),
	gen_tcp:send(Socket, <<"client_id: ", Cid/binary, "\n\n">>),
	Double = binary:compile_pattern([<<"\r\n\r\n">>, <<"\r\r">>, <<"\n\n">>]),
	loop(Socket, Double, ClientID, <<>>).

%% CLIENT LOOPER : infinite loop that listen to clients message.
%% ===================================================================
-spec loop(Socket, Double, ClientID, Rest) -> ok when
	Socket :: inet:socket(), 
	Double :: binary(),
	ClientID :: integer(),
	Rest :: binary().
loop(Socket, Double, ClientID, Rest) ->
	case binary:match(Rest, Double) of
		{Start, Len} ->
			Length = Start + Len,
			Current = binary:part(Rest, {0, Start}), % discard the double return separation.
			Remainder = binary:part(Rest, {Length, byte_size(Rest) - Length}),
			log("~p (Client Looper ~p) got a complete message: ~p~n", [self(), ClientID, Current]),
			Reply = parse(Current, ClientID),
			gen_tcp:send(Socket, Reply),
			loop(Socket, Double, ClientID, Remainder);
		nomatch ->
			log("~p (Client Looper ~p) is waiting for next TCP chunk...~n", [self(), ClientID]),
			case gen_tcp:recv(Socket, 0) of
				{ok, Bin} ->
					log("~p (Client Looper ~p) got a TCP chunk: ~p~n", [self(), ClientID, Bin]),
					loop(Socket, Double, ClientID, <<Rest/binary,Bin/binary>>);
				{error,closed} ->
					% Clean up all ressources associated to the client.
					% Try-catch because this {error,closed} may happen in 2 situations:
					% 1. client disconnect: it will be successful (and server won't have client data anymore)
					% 2. server is tear down: it will fail (but we dont care, server tear down will erase everything)
					try 
						Record = ets:lookup(clients_records, ClientID),
						log("~p (Client Looper ~p) is closing connection... ~n", [self(), ClientID]),
						% {ClientID, Socket, [TopicSubscribed], [TopicPublished], NbMsgReceived, NbMsgSent}
						[{ClientID, _, TopicSubscribed, _, _, _}|_] = Record,
						[ets:delete_object(clients_by_topic, {Topic, ClientID}) || Topic <- TopicSubscribed],
						ets:delete(clients_records, ClientID),
						log("~p (Client Looper ~p) closed connection (ressources liberated). ~n", [self(), ClientID])
					catch
						error:_ -> ok
					end,
					ok;
				{error,Error} ->
					log("~p (Client Looper ~p) got unexpected TCP error: ~p ~n", [self(), ClientID, Error]),
					exit({loop_unexpected_msg, Error});
 				Any ->
					log("~p (Client Looper ~p) got unexpected TCP chunk: ~p ~n", [self(), ClientID, Any]),
					exit({loop_unexpected_msg, Any})
			end
	end.

%% PARSING METHODS
%% ===================================================================

% Parses any binary request received, determines the correct type of request based on prefix,
% makes use of dedicated parser for certain request, does the appropriate call and returns the reply as a binary.
% Returns a '400 Bad Request' binary when the format is not met or the protocol violated.
-spec parse(Request, ClientID) -> Reply when
	Request :: binary(),
	Reply :: binary(),
	ClientID :: integer().
parse(Request, ClientID) -> 
	case Request of
		<<"topic:", Payload/binary>> ->
			log("~p (Client Looper ~p) got publish command, payload: ~p. ~n", [self(), ClientID, Payload]),
			parse_publish(Payload, ClientID);			
		<<"subscribe:", Payload/binary>> ->
			log("~p (Client Looper ~p) got subscribe command, payload: ~p. ~n", [self(), ClientID, Payload]),
			parse_subscribe(Payload, ClientID);
		<<"status">> ->
			log("~p (Client Looper ~p) got status command. ~n", [self(), ClientID]),
			handleStatus(ClientID);
		<<"help">> ->
			log("~p (Client Looper ~p) got help command. ~n", [self(), ClientID]),
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
			log("~p (Client Looper ~p) got unknown command: ~p. ~n", [self(), ClientID, Request]),
			bad_request(<<"Command not found: '", Request/binary, "'.">>)
	end.

% Parse a publish request payload, does the appropriate call and returns the reply.
-spec parse_publish(Payload, ClientID) -> binary() when
	Payload :: binary(),
	ClientID :: integer().
parse_publish(Payload, ClientID) ->
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
			Topics = binary:split(Topic, <<",">>, [trim_all, global]),
			case length(Topics) of
				1 ->
					% Extract the second line
					Discard = Start+Len, % length of topic data + return line.
					Bodydata = binary:part(Payload, Discard, size(Payload) - Discard),
					% Second line must start with body: and extract the rest as a message.
					case binary:match(Bodydata, <<"body:">>) of
						{0, L} ->
							Body = binary:part(Bodydata, L, size(Bodydata) - L),
							log("~p (Client Looper ~p) got valid publish command, Topic: ~p, Body: ~p. ~n", [self(), ClientID, Topic, Body]),
							[TopicTrimmed|_] = Topics,
							handlePublish(ClientID, TopicTrimmed, Body);
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
-spec parse_subscribe(Payload, ClientID) -> binary() when
	Payload :: binary(),
	ClientID :: integer().
parse_subscribe(Payload, ClientID) ->
	Simple = binary:compile_pattern([<<"\r\n">>, <<"\r">>, <<"\n">>]),
	% Check the single line.
	case binary:match(Payload, Simple) of
		{_, _} ->
			bad_request(<<"subscribe command items MUST be on a single line.
				$> subscribe: <topic1> [ , <topic2>, ... ]">>);
		nomatch ->
			log("~p (Client Looper ~p) got valid subscribe command, payload: ~p. ~n", [self(), ClientID, Payload]),
			handleSubscribe(ClientID, Payload)
	end.

%% Bad request Helpers, according to parameters (Verbosity)
%% ===================================================================
% Returns a generic 400 Bad Request.
-spec bad_request() -> binary().
bad_request() ->
	<<"400 Bad Request\n">>.

% Returns a message specific 400 Bad Request.
-spec bad_request(Message) -> binary() when
	Message :: binary().
bad_request(Message) ->
	log("~p got bad_request: ~p. ~n", [self(), Message]),
	BR = bad_request(),
	[{verbosity, V}|_] = ets:lookup(options, verbosity),
	case V of
		none ->
			ok; % keeps quiet, discard bad request silently
		minimal ->
			<<BR/binary, "\n">>; % generic message (always same)
		_ ->  %verbose, logs
			<<BR/binary, "Enter 'help' for indications.\nMessage : ", Message/binary, "\n\n">>
	end.

%% HANDLER METHODS : 1 handler for each type of commands
%% ===================================================================

% Handle the status command and returns its status reply as binary.
-spec handleStatus(ClientID) -> Reply when
	ClientID :: integer(),
	Reply :: binary().
handleStatus(ClientID) -> 
	% {ClientID, Socket, [TopicSubscribed], [TopicPublished], NbMsgReceived, NbMsgSent}
	ClientRecord = ets:lookup(clients_records, ClientID),
	[{ClientID, _ , TopicSubscribed, TopicPublished, NbMsgReceived, NbMsgSent}] = ClientRecord,
	log("~p (Client Status ~p) got: rcv: ~p, sent: ~p. ~n", [self(), ClientID, NbMsgReceived, NbMsgSent]),
	Cid = integer_to_binary(ClientID),
	NbR = integer_to_binary(NbMsgReceived),
	NbS = integer_to_binary(NbMsgSent),
	TopS = list_to_binary(TopicSubscribed),
	TopP = list_to_binary(TopicPublished),
	<<"status for client ", Cid/binary,
	":\nsubscribed topics: ", TopS/binary,
	"\npublished topics: ", TopP/binary,
	"\nreceived messages: ", NbR/binary,
	"\nsent messages: ", NbS/binary, "\n\n">>. 

% Handle the subscribe command, subscribe the client to the topics and returns its confirmation reply as binary.
-spec handleSubscribe(ClientID, Topics) -> Reply when
	ClientID :: integer(),
	Topics :: list(),
	Reply :: binary().
handleSubscribe(ClientID, Topics) ->
	% Split the whole payload and trim all leading and trailing spaces
	TopicsList = binary:split(Topics, <<",">>, [trim_all, global]),
	% Insert ClientID in all topics he subscribes to
	ets:insert(clients_by_topic, [ {Topic, ClientID} || Topic <- TopicsList]),
	ClientRecord = ets:lookup(clients_records, ClientID),
	[{ClientID, Socket , TopicSubscribed, TopicPublished, NbMsgReceived, NbMsgSent}] = ClientRecord,
	NewTopicSubscribed = lists:append(TopicsList, TopicSubscribed),
	ets:insert(clients_records, {ClientID, Socket, NewTopicSubscribed, TopicPublished, NbMsgReceived, NbMsgSent}),
	<<"subscribed: ", Topics/binary, "\n\n">>.

% Handle the publush command, forward the message to all subscribed clients, and returns its confirmation reply as binary.
-spec handlePublish(ClientID, Topic, Message) -> Reply when
	ClientID :: integer(),
	Topic :: binary(),
	Message :: binary(),
	Reply :: binary().
handlePublish(ClientID, Topic, Message) ->
	Cid = integer_to_binary(ClientID),
	Data = <<"from: ", Cid/binary,
	"\ntopic: ", Topic/binary,
	"\nbody:", Message/binary, "\n\n">>,
	% Retrieve list of clients from that topic
	Clients = ets:lookup(clients_by_topic, Topic),
	io:format("~p~n", [Clients]),
	log("~p (Client Publish ~p) sent message to subscribers: ~p.~n", [self(), ClientID, [Clients]]),

	[distribute(ClientIDDest, Data) || {_, ClientIDDest} <- Clients],
	log("~p (Client Publish ~p) sent message to subscribers of ~p. ~n", [self(), ClientID, Topic]),

	% Distribute to wildcard clients.
	ClientsStar = ets:lookup(clients_by_topic, <<"*">>),
	[distribute(ClientIDDest, Data) || {_, ClientIDDest} <- ClientsStar],
	
	% Update record of sender: [TopicPublished] prefixed by the new topic, NbMsgSent (increment)
	ClientRecord = ets:lookup(clients_records, ClientID),
	[{ClientID, Socket, TopicSubscribed, TopicPublished, NbMsgReceived, NbMsgSent}] = ClientRecord,
	ets:insert(clients_records, {ClientID, Socket, TopicSubscribed, [Topic|TopicPublished], NbMsgReceived, NbMsgSent+1}),
	
	<<"accepted: ", Topic/binary, "\n\n">>.

% Retreive the client record for its Socket (to send the data) and its NbMsgReceived (to increment and update record)
-spec distribute(ClientIDRecipient, Data) -> ok when
	ClientIDRecipient :: integer(),
	Data :: binary().
distribute(ClientIDRecipient, Data) ->
	ClientRecord = ets:lookup(clients_records, ClientIDRecipient),
	[{ClientIDRecipient, Socket, TopicSubscribed, TopicPublished, NbMsgReceived, NbMsgSent}|_] = ClientRecord,
	gen_tcp:send(Socket, Data),
	ets:insert(clients_records, {ClientIDRecipient, Socket, TopicSubscribed, TopicPublished, NbMsgReceived+1, NbMsgSent}),
	ok.
