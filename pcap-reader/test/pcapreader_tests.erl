-module(pcapreader_tests).

-include_lib("eunit/include/eunit.hrl").

dumb_true_test() ->
  ?assert(true).

dumb_fail_test() ->
  ?assertNot(false).

%% TESTS read_global_header
read_global_header_real_file_test() ->
  Map = #{magic_number => 3569595041,
    network => 0,
    sigfigs => 0,
    snaplen => 262144,
    thiszone => 0,
    version_major => 2,
    version_minor => 4},
  B = reader:filereader(),
  ?assertMatch({ok, Map, _}, pcapreader:read_global_header(B)).

read_global_header_not192_test() ->
  A = <<12:32>>,
  B = binary:list_to_bin([A, A, A, A, A]),
  ?assertError(bad_global_pcap_header, pcapreader:read_global_header(B)).

%% TESTS pcap_all_reader
read_all_real_file_test() ->
  Map = #{magic_number => 3569595041,
    network => 0,
    sigfigs => 0,
    snaplen => 262144,
    thiszone => 0,
    version_major => 2,
    version_minor => 4},
  B = reader:filereader(),
  ?assertMatch({Map, _}, pcapreader:pcap_all_reader(B)).

%% TESTS read_pcap_header
read_pcap_header_real_file_test() ->
  <<_:192, Rest/binary>> = reader:filereader(),
  Map1 = #{incl_len => 88,orig_len => 88,ts_sec => 1458507088,ts_usec => 879754},
  Map2 = #{incl_len => 88,orig_len => 88,ts_sec => 1458507088,ts_usec => 879790},
  ?assertMatch([{Map1, _}, {Map2, _}], pcapreader:read_pcap_header(Rest)).

read_pcap_header_real_file_no_header_test() ->
  Rest = reader:filereader("../test_files/ping_1_nopcapheader.pcap"),
  Map1 = #{incl_len => 88,orig_len => 88,ts_sec => 1458507088,ts_usec => 879754},
  Map2 = #{incl_len => 88,orig_len => 88,ts_sec => 1458507088,ts_usec => 879790},
  ?assertMatch([{Map1, _}, {Map2, _}], pcapreader:read_pcap_header(Rest)).

read_pcap_header_real_file_single_packet1_test() ->
  Rest = reader:filereader("../test_files/ping_1_single_PCAP_packet1.pcap"),
  Map = #{incl_len => 88,orig_len => 88,ts_sec => 1458507088,ts_usec => 879754},
  ?assertMatch([{Map, _}], pcapreader:read_pcap_header(Rest)).

read_pcap_header_real_file_single_packet2_test() ->
  Rest = reader:filereader("../test_files/ping_1_single_PCAP_packet2.pcap"),
  Map = #{incl_len => 88,orig_len => 88,ts_sec => 1458507088,ts_usec => 879790},
  ?assertMatch([{Map, _}], pcapreader:read_pcap_header(Rest)).

%% TESTS read_IP_datagram
read_pcap_content_real_file_IP_datagram1_test() ->
  Map = #{destinationIP => 2130706433,
    ds => 0,
    flags => 0,
    fragOffset => 0,
    headerChecksum => 0,
    identification => 22866,
    ihl => 5,
    ipVersion => 4,
    length => 84,
    protocol=>1,
    sourceIP=>2130706433, % localhost 127.0.0.1
    ttl=>64},
  Rest = reader:filereader("../test_files/ping_2_single_IP_datagram1.pcap"),
  ?assertMatch({Map, _}, pcapreader:read_IP_datagram(Rest)).

read_pcap_content_real_file_IP_datagram2_test() ->
  Map = #{destinationIP => 2130706433,
    ds => 0,
    flags => 0,
    fragOffset => 0,
    headerChecksum => 0,
    identification => 8322,
    ihl => 5,
    ipVersion => 4,
    length => 84,
    protocol=>1,
    sourceIP=>2130706433, % localhost 127.0.0.1
    ttl=>64},
  Rest = reader:filereader("../test_files/ping_2_single_IP_datagram2.pcap"),
  ?assertMatch({Map, _}, pcapreader:read_IP_datagram(Rest)).

%% TESTS read_payload
read_payload_real_file_ICMP_packet1_test() ->
  Map = #{checkSum => 36353,code => 0,identifier => 44619,seqNum => 0,type => 8},
  Rest = reader:filereader("../test_files/ping_3_single_ICMP_packet1.pcap"),
  ?assertMatch({Map, _}, pcapreader:read_payload(Rest)).

read_payload_real_file_ICMP_packet2_test() ->
  Map = #{checkSum => 38401,code => 0,identifier => 44619,seqNum => 0,type => 0},
  Rest = reader:filereader("../test_files/ping_3_single_ICMP_packet2.pcap"),
  ?assertMatch({Map, _}, pcapreader:read_payload(Rest)).
