% MQTT Unit Tests
-module(micromq_client_connect_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/micromq.hrl").

%% ===================================================================
%% Client-server connection testing
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
%	[?_assertEqual(ok, ok)]
%% ===================================================================

%% ===================================================================
%% IN THIS MODULE, 
%% - the start function MUST start the server and return a {Address, Port} tuple.
%% - the stop function MUST stop the server (and MAY use the {Address, Port} recieved)
%% - the tests function MAY use the {Address, Port} recieved)
%% ===================================================================

% Start & stop server, check client cannot connect anymore after teardown.
client_connect_NOT_after_server_down_test() ->
	micromq:start_link(),
	micromq:stop(),
	{Result, _} = gen_tcp:connect(?ADDRESS, ?PORT, [], 100),
	?assertEqual(error, Result).

% check client cannot connect yet, before a start&stop server.
client_connect_NOT_before_server_up_test() ->
	{Result, _} = gen_tcp:connect(?ADDRESS, ?PORT, [], 100),
	?assertEqual(error, Result),
	micromq:start_link(),
	micromq:stop().

%% ===== UTILITIES for serveral tests =====
% Start the server with default params and returns the default {Address, Port} tuple. 
start() -> 
	micromq:start_link(),
	{?ADDRESS, ?PORT}.

% Stop the server and returns ok.
stop(_) ->
	micromq:stop().

% Returns a test set trying to connect to this {Address, Port} tuple. 
% Also prooves its unavailable on 1 other port and 1 other address.
test_connect({Address, Port})->
	{Success1, _} = gen_tcp:connect(Address, Port, []),
	{Failure1, _} = gen_tcp:connect(Address, Port+1, [], 100),
	case Address of
		{I1, I2, I3, I4} ->
			CustAddr = {I1, I2, I3, I4+1};
		_ -> % including localhost
			CustAddr = {127,0,0,2}
	end,
	{Failure2, _} = gen_tcp:connect(CustAddr, Port, [], 100),
	[?_assertEqual(ok, Success1), ?_assertEqual(error, Failure1), ?_assertEqual(error, Failure2)].

%% ===== TESTS =====
% Start server with default Port/Address and connect client with default Port/Address
client_connect_simple_test_() ->
	{setup, 
	fun start/0, 
	fun stop/1, 
	fun test_connect/1}.

% Start server with custom Port/Address (supposely equal to the defaults) and connect client with default Port/Address
start_cust1() ->
	micromq:start_link([{address, localhost}, {port, 5017}]),
	{?ADDRESS, ?PORT}.
client_connect_default_test_() ->
	{setup, 
	fun start_cust1/0, 
	fun stop/1, 
	fun test_connect/1}.

% Start server with custom Port/Address and connect client with same custom Port/Address
start_cust2() ->
	Address = {127, 0, 0, 1},
	Port = 5016,
	micromq:start_link([{address, Address}, {port, Port}]),
	{Address, Port}.
client_connect_custom_test_() ->
	{setup, 
	fun start_cust2/0, 
	fun stop/1, 
	fun test_connect/1}.

% Start server with custom Port and checks it is unavailable on the default port.
test_connect_cust({Address, _}) ->
	{Failure, _} = gen_tcp:connect(Address, ?PORT, [], 100),
	[?_assertEqual(error, Failure)].
client_connect_custom_port_test_() ->
	{setup, 
	fun start_cust2/0, 
	fun stop/1, 
	fun test_connect_cust/1}.
