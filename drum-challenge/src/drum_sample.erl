-module(drum_sample).

%% Authors: Melanie Huck, James Nolan, Valentin Minder

%% WARNING: a \t tabulation is considered between instrument and measure on each track.
%% (instead of a undefined number of spaces)
%% Tests and test files have been changed to be compliant with this approach.
%% All given tests successfully run.

%% API
-export([
  decode_file/1,
  render_file/1
]).

% for testing
-ifdef(TEST).
-export([
  binary_to_string/1, % utils
  parse_header/1,
  parse_tracks/1,
  parse_measure/1,
  render/3,
  render_tracks/2
]).
-endif.

%% ============================================================================
%% API.
%% ============================================================================

% Decode a file.
% Input: file name of .splice file
% Output: {ok, VersionName, Tempo, Tracks}
% with Tracks as {TrackNo, Instrument, MeasureStruct}
% with MeasureStruct as an array of array of bytes, grouped 4 by 4
decode_file(File) ->
  try
    {ok, Bin} = file:read_file(File),
    {ok, VersionName, Tempo, TracksBinary} = parse_header(Bin),
    {ok, Tracks} = parse_tracks(TracksBinary),
    {ok, VersionName, Tempo, Tracks}
  catch
    error:{badmatch, Error} -> Error
  end.

% Decode and renders a file.
% Input: file name of .splice file
% Output: {ok, Render} where Render is a List (string) representing the file.
render_file(File) ->
  try
    {ok, VersionName, Tempo, Tracks} = decode_file(File),
    {ok, render(VersionName, Tempo, Tracks)}
  catch
    error:{badmatch, Error} -> Error
  end.

%% ============================================================================
%% Internals - PARSING
%% ============================================================================

% Parse a binary file and extract headers, leaving the content as binary.
% Returns error parse_header if magic is incorrect or not enough data provided.
% Any subsequent data is unused.
% Input: a binary of .slice file
% Ouput: a tuple {ok, VersionName, Tempo, TracksBinary}
parse_header(Bin) ->
  case Bin of
    % Verify the magic and reads ONLY the right length of payload, anything else is discarded
    <<"SPLICE", PayloadLength:64/integer, Payload:PayloadLength/binary, _/binary>> ->
      case Payload of
        <<Version:32/binary, Tempo:32/little-float, TracksBinary/binary>> ->
          VersionName = binary_to_string(Version),
          {ok, VersionName, Tempo, TracksBinary};
        Any -> {error, parse_header, Any}
      end;
    Any -> {error, parse_header, Any}
  end.

% Recursively parse tracks binary and extract their headers and their content, the measure.
% Input: a binary of tracks
% Output: an array of tracks, each containing {TrackNo, Instrument, MeasureStruct}
% MeasureStruct as defined in parse_measure(Bin).
parse_tracks(Bin) ->
  case Bin of
    <<_:0>> -> {ok, []};
    <<TrackNo:8/integer, Length:32/integer, Instrument:Length/binary, Measure:16/binary, Rest/binary>> ->
      MeasureVal = parse_measure(Measure),
      InstrumentVal = binary_to_list(Instrument),
      {ok, Tail} = parse_tracks(Rest),
      {ok, [{TrackNo, InstrumentVal, MeasureVal} | Tail]};
    Any -> Any
  end.

% Parse a binary Measure and returns its structure as an array of array
% Byte can only have values 0 or 1, otherwise an error is returned.
% Input: binary representation of a Measure
% Example: <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1>>
% Output: an array of array of bytes, grouped 4 by 4
% Example: [[0, 0, 0, 1], [0, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1]]
parse_measure(Bin) ->
  Measure = parse_measure_no_validation(Bin),
  ValidationResult = validate_measure(Measure),
  case ValidationResult of
    ok -> Measure;
    Any -> Any
  end.

% Parse without validation of bytes
parse_measure_no_validation(Bin) ->
  case Bin of
    <<_:0>> -> [];
    <<Group:4/binary, Rest/binary>> ->
      [binary_to_list(Group) | parse_measure_no_validation(Rest)]
  end.

% Validation of bytes of the Measure
% If failing the whole group is returned.
validate_measure([]) -> ok;
validate_measure([H | T]) ->
  GroupValidationResult = validate_group_within_measure(H),
  case GroupValidationResult of
    ok -> validate_measure(T);
    bad_value -> {parse_measure, bad_value, list_to_binary(H)}
  end.

% Validation of group with measure : each byte is either 0 or 1.
validate_group_within_measure([]) -> ok;
validate_group_within_measure([H | T]) when H == 0; H == 1 -> validate_group_within_measure(T);
validate_group_within_measure(_) -> bad_value.

%% ============================================================================
%% Internals - RENDERING
%% ============================================================================

% Renders the header of the structure and calls the rendering of tracks
% Input: a Tuple of Version, Tempo, and array of Tracks
% Example: "foo bar 42", 120.0, [Tracks]
% Output: a List, rendering of headers (each on its line), rendering of all tracks
% Example: "Saved with HW Version: foo bar 42\nTempo: 120\nTracksRendering"
render(Version, Tempo, Tracks) ->
  FloatToPrint = float_format(Tempo, 4),
  "Saved with HW Version: " ++ Version ++ "\nTempo: " ++ FloatToPrint ++ "\n" ++ render_tracks(Tracks, 0).

% Takes a Float and returns a List, formatted as follow:
%  - if the Float with Precision decimals evaluates to an Int, the Int representation (eg: Float 12.0 -> "12")
%  - otherwise, the Float representation with Precision digits (eg: Float 12.432, Precision 1 -> "12.4")
float_format(Float, Precision) ->
  % Integer representation (eg: 12)
  IntList = float_to_list(Float, [{decimals, 0}]),
  % Float representation with Precision decimals (eg: 12.4)
  FloatList = float_to_list(Float, [{decimals, Precision}, compact]),
  % They are turned back to Integer and Float, respectively
  IntRepr = list_to_integer(IntList),
  FloatRepr = list_to_float(FloatList),
  case Float of
    % If they evaluate the same (==) -> the Integer representation is returned
    _ when IntRepr == FloatRepr -> IntList;
    % Otherwise, the Float representation is returned
    _ -> FloatList
  end.

% Recursively pretty render of tracks, for each calls the rendering of its Measure
% Input: An array of Tracks, A integer
% Example: [{0, "kick", Measure}]
% Ouput: a List (string) rendering all Tracks, each Track rendered on its own line.
% Example: "(0) kick\tMeasureRender\n"
render_tracks([], _) -> "";
render_tracks([H | T], N) ->
  {No, Instr, Measure} = H,
  NumberToPrint = integer_to_list(No),
  "(" ++ NumberToPrint ++ ") " ++ Instr ++ "\t" ++ render_measure(Measure) ++ "\n" ++ render_tracks(T, N + 1).

%% WARNING: a \t tabulation is considered between instrument and measure on each track.
%% (instead of a undefined number of spaces)
%% Tests and test files have been changed to be compliant with this approach.
%% All given tests successfully run.


% Pretty render of a measure
% Input: an array of arrays of byte, called a Measure
% Example: [[0, 0, 1, 0], [0, 0, 1, 0], [1, 0, 1, 0], [0, 0, 1, 0]]
% Output: a List (string), where global array elements separated by |, 0 is -, 1 is x
% Example: |--x-|--x-|x-x-|--x-|
render_measure([]) -> "|";
render_measure([H | T]) ->
  "|" ++ render_group(H) ++ render_measure(T).

% Pretty render of a group within a measure
% Input: an array of byte, called a Group
% Example: [1, 0, 1, 0]
% Output: a List (string), where 0 is -, 1 is x
% Example: x-x-
render_group([]) -> "";
render_group([H | T]) ->
  case H of
    0 -> "-" ++ render_group(T);
    1 -> "x" ++ render_group(T)
  end.

% Takes a Bin and returns the list represented, without trailing zeroes.
binary_to_string(Bin) ->
  remove_trailing_zeroes(binary_to_list(Bin)).

% Takes a list, return the list limited up to the first 0 found, not included.
remove_trailing_zeroes(L) -> lists:reverse(remove_trailing_zeroes(L, [])).
% Tail-recursive implementation with final reverse
remove_trailing_zeroes([], Acc) -> Acc;
remove_trailing_zeroes([0|_], Acc) -> Acc;
remove_trailing_zeroes([H|T], Acc) -> remove_trailing_zeroes(T, [H|Acc]).