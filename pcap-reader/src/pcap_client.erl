-module(pcap_client).

%% API
-export([
  pcap_read/0,
  pcap_read_fully/0
]).

pcap_read () ->
  {ok, Binary} = file:read_file("../test_files/ping_0_full.pcap"),
  {ok, PcapHeader, Content} = pcapreader:read_global_header(Binary),
  read(pcapreader:read_pcap_header(Content), 1).

pcap_read_fully() ->
  {ok, Binary} = file:read_file("../test_files/ping_0_full.pcap"),
  {ok, PcapHeader, Content} = pcapreader:read_global_header(Binary),
  read_fully(pcapreader:read_pcap_header(Content), 1).

% Internals

read([], _) -> ok;
read([H|T], N) ->
  {_, {IP_header, {ICMP_h, _}}} = H, %
  Protocol = protocol_IP(maps:get(protocol, IP_header)),
  Type = type_ICMP(maps:get(type, ICMP_h)),
  io:format("~p ~.16B-> ~.16B ~s ~s id=Ox~.16b, seq=~p, ttl=~p~n", [
    N,
    maps:get(sourceIP, IP_header),
    maps:get(destinationIP, IP_header),
    Protocol,
    Type,
    maps:get(identifier, ICMP_h),
    maps:get(seqNum, ICMP_h),
    maps:get(ttl, IP_header)
  ]),
  read(T, N+1).

read_fully([], _) -> ok;
read_fully([H|T], N) ->
  {PCAP_headers, {IP_header, {ICMP_h, ICMP_data}}} = H,

  io:format("---- PCAP header #~p ----~n", [N]),
  io:format("Time (sec) : ~p~n", [maps:get(ts_sec, PCAP_headers)]),
  io:format("Time (usec): ~p~n", [maps:get(ts_usec, PCAP_headers)]),
  io:format("Length (captured): ~p~n", [maps:get(incl_len, PCAP_headers)]),
  io:format("Length (origin)  : ~p~n", [maps:get(orig_len, PCAP_headers)]),

  io:format("---- PCAP packet #~p ----~n", [N]),
  io:format("Null/Loopback~n", []),
  io:format("    Family: IP(2)~n", []),

  io:format("---- IP header #~p ----~n", [N]),
  io:format("Internet Protocol Version ~p, Src: Ox~.16B, Dst: Ox~.16B~n", [ %TODO: ~p ip version
    maps:get(ipVersion, IP_header),
    maps:get(sourceIP, IP_header),
    maps:get(destinationIP, IP_header)
  ]),
  io:format(" Total Length: ~p~n", [maps:get(length, IP_header)]),
  io:format(" Identification: Ox~.16b(~p)~n", [
    maps:get(identification, IP_header),
    maps:get(identification, IP_header)
  ]),
  io:format(" Fragment offset: ~p~n", [maps:get(fragOffset, IP_header)]),
  io:format(" Time to live: ~p~n", [maps:get(ttl, IP_header)]),
  Protocol = protocol_IP(maps:get(protocol, IP_header)),
  io:format(" Protocol: ~p(~p)~n", [Protocol, maps:get(protocol, IP_header)]),
  io:format(" Header checksum: Ox~.16b~n", [maps:get(headerChecksum, IP_header)]),
  io:format(" Source: Ox~.16B~n", [maps:get(sourceIP, IP_header)]),
  io:format(" Destination: 0x~.16B~n", [maps:get(destinationIP, IP_header)]),

  io:format("---- ICMP header #~p ----~n", [N]),
  io:format("Internet Control Message Protocol~n", []),
  Type = type_ICMP(maps:get(type, ICMP_h)),
  io:format(" Type: ~p (~s)~n", [maps:get(type, ICMP_h), Type]),
  io:format(" Code: ~p~n", [maps:get(code, ICMP_h)]),
  io:format(" Identifier: ~p~n", [maps:get(identifier, ICMP_h)]),
  io:format(" Sequence Number: ~p~n", [maps:get(seqNum, ICMP_h)]),

  io:format("---- ICMP data #~p ----~n", [N]),
  % TODO: print the data.
  %io:format("~.16b~n", [ICMP_data]),
  read_fully(T, N+1).

% Translate function for IP protocol.
protocol_IP(1) -> "ICMP";
protocol_IP(N) -> N.

% Translate function for ICMP Types
type_ICMP(8) -> "Echo request";
type_ICMP(0) -> "Echo reply  ";
type_ICMP(N) -> N.

% to sepate IP into 4 not working
print_IP(IP) ->
  case IP of
    <<Other/binary>> -> Other;
    <<_:0>> -> [];
    <<A:8, Rest/binary>> -> [A | print_IP(Rest)];
    <<Other/binary>> -> Other
  end.
