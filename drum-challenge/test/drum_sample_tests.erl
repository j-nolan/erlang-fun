-module(drum_sample_tests).

-include_lib("eunit/include/eunit.hrl").


%% ============================================================================
%% Test private functions.
%% To export a private function only for testing, do the following in the
%% source module:
%%
%%   -ifdef(TEST).
%%   -export([private_function/1]).
%%   -endif.
%% ============================================================================

binary_to_string_removes_trailing_zeroes_test() ->
    ?assertEqual("ciao", drum_sample:binary_to_string(<<"ciao", 0, 0, 0>>)).

parse_header_test_() -> [
    {
        "good header, no tracks, ok",
        fun() ->
            % dump in 1-byte decimal format obtained with "od -t uC <file>"
            Bin = <<
                083, 080, 076, 073, 067, 069, 000, 000, 000, 000,
                000, 000, 000, 036, 048, 046, 056, 048, 056, 045,
                097, 108, 112, 104, 097, 000, 000, 000, 000, 000,
                000, 000, 000, 000, 000, 000, 000, 000, 000, 000,
                000, 000, 000, 000, 000, 000, 000, 000, 240, 066>>,
            ?assertEqual({ok, "0.808-alpha", 120.0, <<>>},
                drum_sample:parse_header(Bin))
        end},
    {
        "empty header, fail",
        ?_assertEqual({error, parse_header, <<>>},
            drum_sample:parse_header(<<>>))},
    {
        "header with bad magic, fail",
        fun() ->
            Bin = <<
                082, 080, 076, 073, 067, 069, 000, 000, 000, 000,
                000, 000, 000, 036, 048, 046, 056, 048, 056, 045,
                097, 108, 112, 104, 097, 000, 000, 000, 000, 000,
                000, 000, 000, 000, 000, 000, 000, 000, 000, 000,
                000, 000, 000, 000, 000, 000, 000, 000, 240, 066>>,
            ?assertEqual({error, parse_header, Bin},
                drum_sample:parse_header(Bin))
        end}
].

parse_tracks_test_() -> [
    {
        "empty tracks, ok",
        ?_assertEqual({ok, []}, drum_sample:parse_tracks(<<>>))},
    {
        "sample track #1, ok",
        fun() ->
            % (0) kick     |x---|x---|x---|x---|
            TrackN = 0,
            Instrument = "kick",
            Measure = [[1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0]],
            Bin = <<
                0, 0, 0, 0, 4, 107, 105, 99, 107, 1,
                0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                0, 1, 0, 0, 0>>,
            ?assertEqual({ok, [{TrackN, Instrument, Measure}]},
                drum_sample:parse_tracks(Bin))
        end},
    {
        "sample track #2, ok",
        fun() ->
            % (1) snare    |----|x---|----|x---|
            TrackN = 1,
            Instrument = "snare",
            Measure = [[0, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [1, 0, 0, 0]],
            Bin = <<
                1, 0, 0, 0, 5, 115, 110, 97, 114, 101,
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                0, 0, 1, 0, 0, 0>>,
            ?assertEqual({ok, [{TrackN, Instrument, Measure}]},
                drum_sample:parse_tracks(Bin))
        end}
].

parse_measure_test_() -> [
    {
        "good measure #1",
        ?_assertEqual([[0, 0, 0, 1], [0, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1]],
            drum_sample:parse_measure(
                <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1>>))},
    {
        "good measure #2",
        ?_assertEqual([[1, 1, 1, 1], [0, 0, 0, 0], [1, 1, 1, 1], [1, 1, 1, 1]],
            drum_sample:parse_measure(
                <<1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1>>))},
    {
        "bad measure, bad value",
        ?_assertEqual({parse_measure, bad_value, <<0, 2, 0, 0>>},
            drum_sample:parse_measure(
                <<1, 1, 1, 1, 0, 2, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1>>))}
].

render_test_() -> [
    {
        "render, empty tracks, ok",
        fun() ->
            Version = "foo bar 42",
            Tempo = 120.0,
            Tracks = [],
            Expected = "Saved with HW Version: " ++ Version ++ "\nTempo: 120\n",
            ?assertEqual(Expected,
                lists:flatten(drum_sample:render(Version, Tempo, Tracks)))
        end},
    {
        "render one track, ok",
        fun() ->
            Tracks = [
                {0, "kick",
                    [[1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0]]}
            ],
            Expected = "(0) kick\t|x---|x---|x---|x---|\n",
            ?assertEqual(Expected,
                lists:flatten(drum_sample:render_tracks(Tracks, 0)))
        end},
    {
        "render tracks, ok",
        fun() ->
            Tracks = [
                {0, "kick",
                    [[1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0]]},
                {1, "snare",
                    [[0, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [1, 0, 0, 0]]},
                {2, "clap",
                    [[0, 0, 0, 0], [1, 0, 1, 0], [0, 0, 0, 0], [0, 0, 0, 0]]},
                {3, "hh-open",
                    [[0, 0, 1, 0], [0, 0, 1, 0], [1, 0, 1, 0], [0, 0, 1, 0]]},
                {4, "hh-close",
                    [[1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [1, 0, 0, 1]]},
                {5, "cowbell",
                    [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 0]]}
            ],
            Expected =
                "(0) kick\t|x---|x---|x---|x---|\n" ++
                "(1) snare\t|----|x---|----|x---|\n" ++
                "(2) clap\t|----|x-x-|----|----|\n" ++
                "(3) hh-open\t|--x-|--x-|x-x-|--x-|\n" ++
                "(4) hh-close\t|x---|x---|----|x--x|\n" ++
                "(5) cowbell\t|----|----|--x-|----|\n",
            ?assertEqual(Expected,
                lists:flatten(drum_sample:render_tracks(Tracks, 0)))
        end}

].


%% ============================================================================
%% Test public functions.
%% ============================================================================

non_existing_file_test() ->
    ?assertEqual({error, enoent}, drum_sample:decode_file("nonexistent")),
    ?assertEqual({error, enoent}, drum_sample:render_file("nonexistent")).

decode_file_test_() -> [
    {
        "decode file ok",
        fun() ->
            Version = "0.808-alpha",
            Tempo = 120.0,
            Tracks = [
                {0, "kick",
                    [[1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0]]},
                {1, "snare",
                    [[0, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [1, 0, 0, 0]]},
                {2, "clap",
                    [[0, 0, 0, 0], [1, 0, 1, 0], [0, 0, 0, 0], [0, 0, 0, 0]]},
                {3, "hh-open",
                    [[0, 0, 1, 0], [0, 0, 1, 0], [1, 0, 1, 0], [0, 0, 1, 0]]},
                {4, "hh-close",
                    [[1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [1, 0, 0, 1]]},
                {5, "cowbell",
                    [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 0]]}
            ],
            ?assertEqual({ok, Version, Tempo, Tracks},
                drum_sample:decode_file(data_dir("drum_pattern_1.splice")))
        end},
    {
        "decode file fail",
        ?_assertMatch({error, parse_header, _},
            drum_sample:decode_file(data_dir("drum_pattern_1.txt")))
    }
].

pattern1_test() ->
    {ok, Expected} = file:read_file(data_dir("drum_pattern_1.txt")),
    {ok, Actual} = drum_sample:render_file(data_dir("drum_pattern_1.splice")),
    ?assertEqual(binary_to_list(Expected), lists:flatten(Actual)).

pattern2_test() ->
    {ok, Expected} = file:read_file(data_dir("drum_pattern_2.txt")),
    {ok, Actual} = drum_sample:render_file(data_dir("drum_pattern_2.splice")),
    ?assertEqual(binary_to_list(Expected), lists:flatten(Actual)).

pattern3_test() ->
    {ok, Expected} = file:read_file(data_dir("drum_pattern_3.txt")),
    {ok, Actual} = drum_sample:render_file(data_dir("drum_pattern_3.splice")),
    ?assertEqual(binary_to_list(Expected), lists:flatten(Actual)).

pattern4_test() ->
    {ok, Expected} = file:read_file(data_dir("drum_pattern_4.txt")),
    {ok, Actual} = drum_sample:render_file(data_dir("drum_pattern_4.splice")),
    ?assertEqual(binary_to_list(Expected), lists:flatten(Actual)).

pattern5_test() ->
    {ok, Expected} = file:read_file(data_dir("drum_pattern_5.txt")),
    {ok, Actual} = drum_sample:render_file(data_dir("drum_pattern_5.splice")),
    ?assertEqual(binary_to_list(Expected), lists:flatten(Actual)).


misbeat_failing_test() ->
    {ok, Expected} = file:read_file(data_dir("drum_pattern_5_corrupted.txt")),
    {ok, Actual} = drum_sample:render_file(data_dir("drum_pattern_5.splice")),
    ?assertNotEqual(binary_to_list(Expected), lists:flatten(Actual)).

%% Helper functions.

data_dir(File) ->
    % TODO: problem with library location to locate tests file
    {ok, Dir} = file:get_cwd(), % returns .../drum-challenge/.eunit
    Dir ++ "/../test/data/" ++ File.
    % this doesn't work...
    % code:lib_dir('drum-challenge', test) ++ "/data/" ++ File.
