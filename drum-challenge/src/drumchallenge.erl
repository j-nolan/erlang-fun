-module(drumchallenge).

%% drum-challenge: drum-challenge library's entry point.

-export([
  read_splice_header/1
]).


%% API
read_splice_header(Bin) ->
  case Bin of
    <<"SPLICE", PayloadLength:64/integer, Version:32/binary, Tempo:32/little-float, Content/binary>> -> % :PayloadLength

      % TODO: problem with string printing without terminating zeros (^@)
      VersionUTF = binary_to_list(Version),
      io:format("Saved with HW Version: ~s \n", [Version]),
      io:format("Saved with HW Version: ~s \n", [VersionUTF]),
      io:format("Saved with HW Version: ", []),
      string_prettyprint_new(Version),
      io:format("\nSaved with HW Version: ", []),
      string_prettyprint(VersionUTF),


      % Print the tempo without the decimal part
      io:format("\nTempo: ~s~n", [erlang:float_to_list(Tempo, [{decimals, 0}])]),
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
% TODO: trials only, to FIX

string_prettyprint([]) -> ok;
string_prettyprint([0 | _]) -> ok;
string_prettyprint([H | T]) ->
  io:format("~w,", [H]),
  string_prettyprint(T).

string_prettyprint_new(Bin) ->
  case Bin of
    <<_:0>> -> ok;
    <<0, _/binary>> -> ok;
    <<Any, Rest/binary>> ->
      %A = unicode:characters_to_binary(Any),
      %A = unicode:characters_to_list(Any, utf8),
      io:format("~p,", [Any]),
      string_prettyprint_new(Rest)
  end.

%% End of Module.
