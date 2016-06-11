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
start1() -> 
	micromq:start_link(),
	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [{active , false}]),
	[Socket].

% Start the server with default params and returns a list of a single socket
start2() -> 
	micromq:start_link(),
	{ok, Socket1} = gen_tcp:connect(?ADDRESS, ?PORT, [{active , false}]),
	{ok, Socket2} = gen_tcp:connect(?ADDRESS, ?PORT, [{active , false}]),
	[Socket1, Socket2].

% Close the sockets (always successful), stop the server and returns ok.
stop(Sockets) ->
	[gen_tcp:close(Socket) || Socket <- Sockets],
	micromq:stop().

dumb_tests(_) ->
	[?_assertEqual(ok, ok)].

dumb_test_() ->
	{setup, 
	fun start/0, 
	fun stop/1, 
	fun dumb_tests/1}.

tests_clientID(Sockets) ->
	[Socket|_] = Sockets,
	{ok, Bin} = gen_tcp:recv(Socket, 0),
	[?_assertEqual("client_id: 1\n\n", Bin)].

clientID_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_clientID/1}.

%% Warning: all subsequent tests test nothing in return
%% ===================================================================
tests_status(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"status\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

status_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_status/1}.

tests_subscribe(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"subscribe: hello\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

subscribe_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_subscribe/1}.

tests_wrong_subscribe(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"subscribe: hello\nanything\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

subscribe_wrong_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_subscribe/1}.

tests_publish(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"topic: hello\nbody: message\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

publish_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_publish/1}.

tests_wrong_publish(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"topic: hello\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

publish_wrong_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_publish/1}.

tests_help(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"help\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

help_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_help/1}.

tests_wrong_command(Sockets) ->
	[Socket|_] = Sockets,
	gen_tcp:send(Socket, <<"anything\n\n">>),
	{Reply, _} = gen_tcp:recv(Socket, 0),
	[?_assertEqual(ok, Reply)].

wrong_command_test_() ->
	{setup, 
	fun start1/0, 
	fun stop/1, 
	fun tests_wrong_command/1}.

tests_subscribe_publish(Sockets) ->
	[S1|[S2|_]] = Sockets,
	gen_tcp:send(S1, <<"subscribe:hello\n\n">>),
	gen_tcp:send(S2, <<"topic:hello\nbody: message\n\n">>),
	{Reply, _} = gen_tcp:recv(S1, 0),
	[?_assertEqual(ok, Reply)].

subscribe_publish_test_() ->
	{setup, 
	fun start2/0, 
	fun stop/1, 
	fun tests_subscribe_publish/1}.
