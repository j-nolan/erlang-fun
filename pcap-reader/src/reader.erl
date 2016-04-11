-module(reader).

%% API
-export([
  filereader/0,
  filereader/1
]).

% Reads the given pcap file and returns its binary.
filereader() ->
  filereader("../test_files/ping_0_full.pcap").

% Reads any (test) file and returns its binary.
filereader(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Binary.
