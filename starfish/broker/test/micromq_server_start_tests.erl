% MQTT Unit Tests
-module(micromq_server_start_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/micromq.hrl").

%% ===================================================================
%% Server Start & Stop testing
%% ===================================================================

%% ===================================================================
%% WARNING: NEVER PUT A TEST/ASSERT BETWEEN START & STOP SERVER
%% 
%% However, in this test module, we are precisely testing the start and stop functions.
%% If you get failures in this module, something is going really wrong, 
%% and you should fix it before fixing anything else !
%% (all subsequent test might have strange behaviors)
%% ===================================================================

% Set up and tear down server
server_start_and_stop_test() ->
	?assertEqual(ok, micromq:start_link()),
	?assertEqual(ok, micromq:stop()).

% Set up and tear down server twice
server_start_and_stop_addr_in_use_test() ->
	?assertEqual(ok, micromq:start_link()),
	% Starting twice cause an 'eaddrinuse' error.
	?assertEqual({error,eaddrinuse}, micromq:start_link()),
	?assertEqual(ok, micromq:stop()),
	% Stopping twice does nothing.
	?assertEqual(ok, micromq:stop()).

% Set up with several custum params and tear down server.
server_start_and_stop_server_params_test() ->
	?assertEqual(ok, micromq:start_link([{port, 1337}])),
	?assertEqual(ok, micromq:stop()),
	?assertEqual(ok, micromq:start_link([{address, localhost}, {port, 1337}])),
	?assertEqual(ok, micromq:stop()),
	% Two way of writing localhost or 127.0.0.1
	?assertEqual(ok, micromq:start_link([{address, localhost}])),
	?assertEqual(ok, micromq:stop()),
	?assertEqual(ok, micromq:start_link([{address, {127, 0, 0, 1}}])),
	?assertEqual(ok, micromq:stop()),
	% All options are allowed
	?assertEqual(ok, micromq:start_link([{max_clients, 10}, {max_topics, 10}, {address, localhost}, {port, 1337}])),
	?assertEqual(ok, micromq:stop()),
	% The same option is allowed twice
	?assertEqual(ok, micromq:start_link([{max_clients, 10}, {max_clients, 100}])),
	?assertEqual(ok, micromq:stop()).
