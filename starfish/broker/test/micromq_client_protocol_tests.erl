% MQTT Unit Tests
-module(micromq_client_protocol_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/micromq.hrl").

%% ===================================================================
%% Client Protocol Testing
%% ===================================================================

%% ===================================================================
%% WARNING: NEVER PUT A TEST/ASSERT BETWEEN START & STOP SERVER
%% If the test fails, the server will never stop, and all subsequent tests
%% trying to start the server will fail as well.
%%
%% ALWAYS use the test construction, like this (ends with test_!)
% example_test_() ->
% 	{setup, 
% 	fun start/0, 
% 	fun stop/1, 
% 	fun tests_array/1}.
%% WHERE
%% - start is the setup function
%% - stop is the teardwon function
%% - stop and tests_array take a single input (the output of start)
%% - test_array returns an array of tests (use _ prefix for asserts!) 
% tests_array(_) ->
%	[?_assertEqual(ok, ok)].
%% ===================================================================

%% ===================================================================
%% IN THIS MODULE, 
%% - the start function MUST start the server with default {Address, Port}
%%   and MUST return a list [] of connected sockets
%% - the stop function MUST stop the server
%%   and MUST close the sockets in the list [] of connected sockets
%% - the tests function MAY use the list [] of connected sockets
%% ===================================================================

%% ===== UTILITIES for serveral tests =====
% Start the server with default params and returns an empty list of sockets
start() -> 
	micromq:start_link(),
	[].

% Start the server with default params and returns a list of a single socket
% with greetings consumed
start1() -> 
	micromq:start_link(),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	receive_line(Socket),
	[Socket].

% Start the server with default params and returns a list of two sockets
% with greetings consumed
start2() -> 
	micromq:start_link(),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Socket1} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	{ok, Socket2} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	receive_line(Socket1),
	receive_line(Socket2),
	[Socket1, Socket2].

% Start the server with default params and returns a list of a single socket
% with greetings NOT consumed
start1_raw() -> 
	micromq:start_link(),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	[Socket].

% Start the server with default params and returns a list of a single socket
% with greetings NOT consumed
start2_raw() -> 
	micromq:start_link(),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Socket1} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	{ok, Socket2} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	[Socket1, Socket2].

% Close the sockets (always successful), stop the server and returns ok.
stop(Sockets) ->
	[gen_tcp:close(Socket) || Socket <- Sockets],
	timer:sleep(5), % Give a chance to the server to close down.
	micromq:stop().

% THIS FUNCTION IS AS GIVEN BY PROFESSOR in micromq_sample_tests.erl + timeout
% % A "line" is a chunk of bytes separated by a newline character (compare with
% % the UUT sending the string "hello\n").
% % This function is resistent to "short-reads"
receive_line(Socket) -> receive_line(Socket, []).
receive_line(Socket, SoFar) ->
	receive
		{tcp, Socket, Bin} ->
			case binary:split(Bin, [<<"\n">>]) of
				% "\n" found
				[Token, Rest] ->
					{ok, list_to_binary(lists:reverse([Token|SoFar])), Rest};
				% "\n" not found (yet)
				[Bin] ->
					receive_line(Socket, [Bin|SoFar])
			end;
		{tcp_closed, Socket} ->
			{error, unexpected_close}
	after 100 -> % Timeout added, in case of broken test, does not get stuck.
		{error, timeout}
	end.

% Tests start here (by a dumb test to proove the setup-start-tests-stop workflow works)
% ===================================================================
dumb_tests(_) ->
	[?_assertEqual(ok, ok)].

dumb_test_() ->
	{setup, 
	fun start/0, 
	fun stop/1, 
	fun dumb_tests/1}.

%% Test ID's of single and double connections.
%% ===================================================================

tests_clientID(Sockets) ->
	[Socket|_] = Sockets,
	{ok, Bin, _} = receive_line(Socket),
	[?_assertEqual(<<"client_id: 1">>, Bin)].

clientID_test_() ->
	{setup, 
	fun start1_raw/0, 
	fun stop/1, 
	fun tests_clientID/1}.

tests_clientIDs(Sockets) ->
	[Socket1|[Socket2|_]] = Sockets,
	{ok, Bin1, _} = receive_line(Socket1),
	{ok, Bin2, _} = receive_line(Socket2),
	[?_assertEqual(<<"client_id: 1">>, Bin1), ?_assertEqual(<<"client_id: 2">>, Bin2)].

clientIDs_test_() ->
	{setup, 
	fun start2_raw/0, 
	fun stop/1, 
	fun tests_clientIDs/1}.

%% Test status
%% ===================================================================
tests_status(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"status\n\n">>),
	{ok, L1, R} = receive_line(Socket),
	[L2 | [L3 | [L4 | [L5|_]]]] = binary:split(R, [<<"\n">>], [global]), 
	[?_assertEqual(<<"status for client 1:">>, L1),
	?_assertEqual(<<"subscribed topics: ">>, L2),
	?_assertEqual(<<"published topics: ">>, L3),
	?_assertEqual(<<"received messages: 0">>, L4),
	?_assertEqual(<<"sent messages: 0">>, L5)].

status_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_status/1}.

%% Test subscribe (simple, wrong, and check it updates status)
%% ===================================================================
tests_subscribe(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"subscribe: hello\n\n">>),
	{ok, Reply, _} = receive_line(Socket),
	[?_assertEqual(<<"subscribed: hello">>, Reply)]. 

subscribe_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_subscribe/1}.

tests_wrong_subscribe(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"subscribe: hello\nanything\n\n">>),
	%On none-verbose mode, server discard malformed message and sends nothing (and recieve will cause timeout!)
	% a timeout is then considered a success in that case, as we expect an error.
	{Error, Reason} = receive_line(Socket),
	[?_assertEqual(error, Error), ?_assertEqual(timeout, Reason)].

subscribe_wrong_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_subscribe/1}.

tests_subscribe_status(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"subscribe: hello\n\n">>),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Reply, _} = receive_line(Socket),
	
	gen_tcp:send(Socket, <<"status\n\n">>),
	{ok, L1, R} = receive_line(Socket),
	[L2 | [L3 | [L4 | [L5|_]]]] = binary:split(R, [<<"\n">>], [global]), 
	[?_assertEqual(<<"subscribed: hello">>, Reply), 
	?_assertEqual(<<"status for client 1:">>, L1),
	?_assertEqual(<<"subscribed topics: hello">>, L2), 
	?_assertEqual(<<"published topics: ">>, L3), 
	?_assertEqual(<<"received messages: 0">>, L4),
	?_assertEqual(<<"sent messages: 0">>, L5)].

subscribe_status_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_subscribe_status/1}.

%% Test publish (simple, wrong, and check it updates status)
%% ===================================================================
tests_publish(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"topic:   hello1   \nbody: message\n\n">>),
	{ok, Reply, _} = receive_line(Socket),
	[?_assertEqual(<<"accepted: hello1">>, Reply)]. 

publish_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_publish/1}.

tests_wrong_publish_generic(Sockets, Message) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, Message),
	{Error, Reason} = receive_line(Socket),
	[?_assertEqual(error, Error), ?_assertEqual(timeout, Reason)].

tests_wrong_publish1(Sockets) ->
	tests_wrong_publish_generic(Sockets, <<"topic: hello\n\n">>).
tests_wrong_publish2(Sockets) ->
	tests_wrong_publish_generic(Sockets, <<"topic: hello,hello2\nbody:message\n\n">>).
tests_wrong_publish3(Sockets) ->
	tests_wrong_publish_generic(Sockets, <<"topic: hello\nnobody:message\n\n">>).
	% [Socket|_] = Sockets,
	% gen_tcp:send(Socket, <<"topic: hello\n\n">>),
	% {Error1, Reason1} = receive_line(Socket),
	% [?_assertEqual(error, Error1), ?_assertEqual(timeout, Reason1)].

	% gen_tcp:send(Socket, <<"topic: hello,hello2\nbody:message\n">>),
	% {Error2, Reason2} = receive_line(Socket),
	% gen_tcp:send(Socket, <<"topic: hello\nnobody:message\n\n">>),
	% {Error3, Reason3} = receive_line(Socket),
	% %[?_assertEqual(error, Error3), ?_assertEqual(timeout, Reason3)].
	% %On none-verbose mode, server discard malformed message and sends nothing (and recieve will cause timeout!)
	% % a timeout is then considered a success in that case, as we expect an error.
	% [?_assertEqual(error, Error1), ?_assertEqual(timeout, Reason1),
	% ?_assertEqual(error, Error2), ?_assertEqual(timeout, Reason2),
	% ?_assertEqual(error, Error3), ?_assertEqual(timeout, Reason3)].

publish_wrong1_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_publish1/1}.

publish_wrong2_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_publish2/1}.

publish_wrong3_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_publish3/1}.

tests_publish_status(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"topic: hello\r\nbody: message\n\n">>),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Reply, _} = receive_line(Socket),
	
	gen_tcp:send(Socket, <<"status\n\n">>),
	{ok, L1, R} = receive_line(Socket),
	[L2 | [L3 | [L4 | [L5|_]]]] = binary:split(R, [<<"\n">>], [global]), 
	[?_assertEqual(<<"accepted: hello">>, Reply), 
	?_assertEqual(<<"status for client 1:">>, L1),
	?_assertEqual(<<"subscribed topics: ">>, L2),
	?_assertEqual(<<"published topics: hello">>, L3), 
	?_assertEqual(<<"received messages: 0">>, L4),
	?_assertEqual(<<"sent messages: 1">>, L5)].

publish_status_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_publish_status/1}.

%% Test help & wrong command
%% ===================================================================
tests_help(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"help\n\n">>),
	{Ok, _, _} = receive_line(Socket),
	[?_assertEqual(ok, Ok)].

help_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_help/1}.

tests_wrong_command(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"anything\n\n">>),
	{Error, Reason} = receive_line(Socket),
	[?_assertEqual(error, Error), ?_assertEqual(timeout, Reason)].

wrong_command_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_command/1}.

%% Test subscribe on S1, publish a message on S2, same message is recieved on S1, updated status of reciever.
%% ===================================================================
tests_subscribe_publish(Sockets) ->
	[S1|[S2|_]] = Sockets,
	
	% subscribe on S1, publish of S2,
	gen_tcp:send(S1, <<"subscribe: hello\n\n">>),
	{ok, _, _} = receive_line(S1),
	timer:sleep(5), % Give a chance to the server to subscribe
	gen_tcp:send(S2, <<"topic: hello\nbody: message\n\n">>),
	{ok, _, _} = receive_line(S2),
	timer:sleep(5), % Give a chance to the server to publish.

	%recieve on S1
	{ok, R1, Rest} = receive_line(S1), 
	[R2 | [R3 |_]] = binary:split(Rest, [<<"\n">>], [global]), 

	gen_tcp:send(S1, <<"status\n\n">>),
	{ok, L1, R} = receive_line(S1),
	[L2 | [L3 | [L4 | [L5|_]]]] = binary:split(R, [<<"\n">>], [global]), 

	[?_assertEqual(<<"from: 2">>, R1),
	?_assertEqual(<<"topic: hello">>, R2),  
	?_assertEqual(<<"body: message">>, R3),
	?_assertEqual(<<"status for client 1:">>, L1),
	?_assertEqual(<<"subscribed topics: hello">>, L2), 
	?_assertEqual(<<"published topics: ">>, L3),
	?_assertEqual(<<"received messages: 1">>, L4),
	?_assertEqual(<<"sent messages: 0">>, L5)].

subscribe_publish_test_() ->
	{setup, 
	fun start2/0, 
	fun stop/1, 
	fun tests_subscribe_publish/1}.

%% Test subscribe on S1 with *, publish a message on S2, same message is recieved on S1
%% ===================================================================
tests_subscribe_star_publish(Sockets) ->
	[S1|[S2|_]] = Sockets,
	
	% subscribe on S1, publish of S2,
	gen_tcp:send(S1, <<"subscribe:*\n\n">>),
	{ok, _, _} = receive_line(S1),
	timer:sleep(5), % Give a chance to the server to subscribe
	gen_tcp:send(S2, <<"topic: hello\nbody: message\n\n">>),
	{ok, _, _} = receive_line(S2),
	timer:sleep(5), % Give a chance to the server to publish.

	%recieve on S1
	{ok, R1, Rest} = receive_line(S1), 
	[R2 | [R3 |_]] = binary:split(Rest, [<<"\n">>], [global]), 

	gen_tcp:send(S1, <<"status\n\n">>),
	{ok, L1, R} = receive_line(S1),
	[L2 | [L3 | [L4 | [L5|_]]]] = binary:split(R, [<<"\n">>], [global]), 

	[?_assertEqual(<<"from: 2">>, R1),
	?_assertEqual(<<"topic: hello">>, R2),  
	?_assertEqual(<<"body: message">>, R3),
	?_assertEqual(<<"status for client 1:">>, L1),
	?_assertEqual(<<"subscribed topics: *">>, L2), 
	?_assertEqual(<<"published topics: ">>, L3),
	?_assertEqual(<<"received messages: 1">>, L4),
	?_assertEqual(<<"sent messages: 0">>, L5)].

subscribe_star_publish_test_() ->
	{setup, 
	fun start2/0, 
	fun stop/1, 
	fun tests_subscribe_star_publish/1}.

%% Test bad request.
%% ===================================================================

% Start the server with custom params and returns a list of a single socket
% with greetings consumed
start1_verbose() -> 
	micromq:start_link([{verbosity, verbose}]),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	receive_line(Socket),
	[Socket].

start1_minimal() -> 
	micromq:start_link([{verbosity, minimal}]),
	timer:sleep(5), % Give a chance to the server to listen.
	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary]),
	receive_line(Socket),
	[Socket].

tests_badrequest(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"badrequest\n\n">>),
	{ok, Msg, _} = receive_line(Socket), 
	[?_assertEqual(<<"400 Bad Request">>, Msg)].

% Does not test the exact message, just test it gets first the generic one.
bad_request_verbose_test_() ->
	{setup, 
	fun start1_verbose/0, 
	fun stop/1, 
	fun tests_badrequest/1}.

bad_request_minimal_test_() ->
	{setup, 
	fun start1_minimal/0, 
	fun stop/1, 
	fun tests_badrequest/1}.

%% Test disconnect server
%% ===================================================================
disconnect_tests(Sockets) ->
	[Socket|_] = Sockets,
	micromq:stop(),
	timer:sleep(5), % Give a chance to the server to shut down.
	Result = gen_tcp:send(Socket, <<"status\n\n">>),
	[?_assertEqual({error,closed}, Result)].

disconnect_test_() ->
	{setup, 
	fun start1/0, 
	fun dumb_tests/1, 
	fun disconnect_tests/1}.


