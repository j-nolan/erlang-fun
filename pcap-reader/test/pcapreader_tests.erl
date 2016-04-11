-module(pcapreader_tests).

-include_lib("eunit/include/eunit.hrl").

dumb_true_test() ->
  ?assert(true).

dumb_fail_test() ->
  ?assertNot(false).

%% TESTS read_global_header
read_global_header_real_file_test() ->
  B = reader:filereader(),
  ?assertMatch({ok, _, _}, pcapreader:read_global_header(B)).

read_global_header_not192_test() ->
  A = <<12:32>>,
  B = binary:list_to_bin([A, A, A, A, A]),
  ?assertError(bad_global_pcap_header, pcapreader:read_global_header(B)).

%% TESTS pcap_all_reader
read_all_real_file_test() ->
  B = reader:filereader(),
  ?assertEqual(ok, pcapreader:pcap_all_reader(B)).

%% TESTS read_pcap_header
read_pcap_header_real_file_test() ->
  <<_:192, Rest/binary>> = reader:filereader(),
  ?assertEqual(ok, pcapreader:read_pcap_header(Rest)).

read_pcap_header_real_file_no_header_test() ->
  Rest = reader:filereader("../test_files/ping_1_nopcapheader.pcap"),
  ?assertEqual(ok, pcapreader:read_pcap_header(Rest)).

read_pcap_header_real_file_single_packet1_test() ->
  Rest = reader:filereader("../test_files/ping_1_single_PCAP_packet1.pcap"),
  ?assertEqual(ok, pcapreader:read_pcap_header(Rest)).

read_pcap_header_real_file_single_packet2_test() ->
  Rest = reader:filereader("../test_files/ping_1_single_PCAP_packet2.pcap"),
  ?assertEqual(ok, pcapreader:read_pcap_header(Rest)).

%% TESTS read_IP_datagram
read_pcap_content_real_file_IP_datagram1_test() ->
  Rest = reader:filereader("../test_files/ping_2_single_IP_datagram1.pcap"),
  ?assertEqual(ok, pcapreader:read_IP_datagram(Rest)).

read_pcap_content_real_file_IP_datagram2_test() ->
  Rest = reader:filereader("../test_files/ping_2_single_IP_datagram2.pcap"),
  ?assertEqual(ok, pcapreader:read_IP_datagram(Rest)).

%% TESTS read_payload
read_payload_real_file_ICMP_packet1_test() ->
  Rest = reader:filereader("../test_files/ping_3_single_ICMP_packet1.pcap"),
  ?assertEqual(ok, pcapreader:read_payload(Rest)).

read_payload_real_file_ICMP_packet2_test() ->
  Rest = reader:filereader("../test_files/ping_3_single_ICMP_packet2.pcap"),
  ?assertEqual(ok, pcapreader:read_payload(Rest)).
