-module(drumchallenge).

%% drum-challenge: drum-challenge library's entry point.

-export([
  file_reader/0, % TODO: move to test
  read_splice_header/1
]).

% TODO: read generic file, transform this to test with actual content.
file_reader() ->
  {ok, Binary} = file:read_file("../test/data/drum_pattern_1.splice"),
  read_splice_header(Binary).

%% API
read_splice_header(Bin) ->
  case Bin of
    <<"SPLICE", PayloadLength:64/integer, Version:32/binary, Tempo:32/little-float, Content/binary>> -> % :PayloadLength

      io:format("Saved with HW Version: ~s \n", [string_prettyprint_new(Version)]),

      % Print the tempo without the decimal part
      io:format("Tempo: ~s~n", [erlang:float_to_list(Tempo, [{decimals, 0}])]),
      read_splice_content(Content)
  end.

%% Internals

% Read a line of instrument measure, recursively on the next line.
read_splice_content(Bin) ->
  case Bin of
    <<_:0>> -> ok;
    <<TrackNo:8/integer, Length:32/integer, Instrument:Length/binary, Measure:16/binary, Rest/binary>> ->
      io:format("(~p) ~ts \t", [TrackNo, Instrument]),
      prettyprint(Measure),
      io:format("~n", []),
      read_splice_content(Rest)
  end.

% Pretty print of a measure
prettyprint(Bin) ->
  case Bin of
    <<_:0>> ->
      io:format("|", []);
    <<Group:4/binary, Rest/binary>> ->
      io:format("|", []),
      prettyprint_bit(Group),
      prettyprint(Rest)
  end.

% Pretty print of a group within a measure
prettyprint_bit(Bin) ->
  case Bin of
    <<_:0>> -> ok;
    <<0, Rest/binary>> ->
      io:format("-", []),
      prettyprint_bit(Rest);
    <<1, Rest/binary>> ->
      io:format("x", []),
      prettyprint_bit(Rest)
  end.

% Pretty print of string...
string_prettyprint_new(Bin) ->
  case Bin of
    <<_:0>> -> [];
    <<0, _/binary>> -> [];
    <<Any, Rest/binary>> ->
      [Any | string_prettyprint_new(Rest)]
  end.

%% End of Module.
