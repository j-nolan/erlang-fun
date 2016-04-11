-module(pcapreader).

%% pcap-reader: pcap-reader library's entry point.

-export([
  pcap_all_reader/1,
  read_global_header/1,
  read_pcap_header/1,
  read_pcap_content/1,
  read_IP_datagram/1,
  read_payload/1
]).

%% API
% Read PCAP global header and extract
% - global headers
% - content as a list of PCAP packet (decoded, nested structure)
% Safe method (has a try-catch)
pcap_all_reader(Bin) ->
  try
    {ok, Pcap_global_headers, All_pcaps} = read_global_header(Bin),
    {Pcap_global_headers, read_pcap_header(All_pcaps)}
  catch
    error:{badmatch, Error} -> Error
  end.

% Read PCAP global header and extract headers and content (binary)
% Not safe (will crash)
read_global_header(Bin) ->
  case Bin of
    % depending on magic, all fields are /little instead of default /big
    <<Magic_number:32, Version_major:16/little, Version_minor:16/little,
      Thiszone:32/little, Sigfigs:32/little, Snaplen:32/little, Network:32/little,
      All_pcaps/binary>> ->
      % Check magic number 0xd4c3b2a1 (swapped)
      3569595041 = Magic_number,

      % Extract values as a map keys-values
      Values = [Magic_number, Version_major, Version_minor,
        Thiszone, Sigfigs, Snaplen, Network],
      Keys = [magic_number, version_major, version_minor,
        thiszone, sigfigs, snaplen, network],
      Headers = maps:from_list(lists:zip(Keys, Values)),
      {ok, Headers, All_pcaps};
    Bin -> error(bad_global_pcap_header, Bin)
  end.

% Read PCAP packets and extract headers and content, for each packet, and returns a list.
read_pcap_header(Bin) ->
  case Bin of
    <<_:0/binary>> -> [];
    % all PCAP fields are /little because of magic (hard-coded for now)
    <<Ts_sec:32/little, Ts_usec:32/little, Incl_len:32/little, Orig_len:32/little, Rest/binary>> ->
      % Extract values as a map keys-values
      Values = [Ts_sec, Ts_usec, Incl_len, Orig_len],
      Keys = [ts_sec, ts_usec, incl_len, orig_len],
      Pcap_header = maps:from_list(lists:zip(Keys, Values)),
      case Rest of
        <<Pcap_content:Incl_len/binary, Next_packet/binary>> ->
          % directly decode the current packet data, inside nested structure.
          Current = {Pcap_header, read_pcap_content(Pcap_content)},
          [Current | read_pcap_header(Next_packet)];
        Rest -> error({bad_pcap_content, Incl_len, Bin})
      end;
    Bin -> error({bad_pcap_header, Bin}) %% length!
  end.

% Gets rid of a 32-bit marker at start of PCAP packet(maybe for type TCP/IP Family ?)
read_pcap_content(Pcap_content) ->
  case Pcap_content of
    <<Any:32, Datagram/binary>> -> read_IP_datagram(Datagram)
  end.

% Read IP datagram and extract headers and content.
-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).
read_IP_datagram(Datagram) ->
  %PSize = byte_size(Datagram),
  case Datagram of
    <<?IP_VERSION:4, IHL:4, DS:8, Length:16,
      Identification:16, Flags:3, FragOffset:13,
      TTL:8, Protocol:8, HeaderChecksum:16,
      SourceIP:32,
      DestinationIP:32,
      Rest/binary>> when IHL >= ?IP_MIN_HDR_LEN -> %, 4 * IHL =< PSize ->
      OptsLen = 4 * (IHL - ?IP_MIN_HDR_LEN),
      <<Opts:OptsLen/binary, Payload/binary>> = Rest,

      % Extract values as a map keys-values
      Values = [?IP_VERSION, IHL, DS, Length,
        Identification, Flags, FragOffset,
        TTL, Protocol, HeaderChecksum,
        SourceIP, DestinationIP],
      Keys = [ipVersion, ihl, ds, length,
        identification, flags, fragOffset,
        ttl, protocol, headerChecksum,
        sourceIP, destinationIP],
      IP_header = maps:from_list(lists:zip(Keys, Values)),

      % reads auto the content in case of ICMP
      case Protocol of
        1 -> {IP_header, read_payload(Payload)}; % ICMP Protocol
        _ -> {error, {unknown_protocol, Protocol}}
      end;
    Bin -> {error, {bad_ip_header, Bin}}
  end.

% Read icmp packet and extract headers and content.
read_payload(Payload) ->
  case Payload of
    <<Type:8, Code:8, CheckSum:16, Data/binary>> ->
      Values = [Type, Code, CheckSum],
      Keys = [type, code, checkSum],
      case Type of
        0 ->
          <<Identifier:16, SeqNum:16, ICMP_data/binary>> = Data,
          Values_in = [Identifier, SeqNum | Values],
          Keys_in = [identifier, seqNum | Keys],
          ICMP_header = maps:from_list(lists:zip(Keys_in, Values_in)),
          {ICMP_header, ICMP_data};
        8 ->
          <<Identifier:16, SeqNum:16, ICMP_data/binary>> = Data,
          Values_in = [Identifier, SeqNum | Values],
          Keys_in = [identifier, seqNum | Keys],
          ICMP_header = maps:from_list(lists:zip(Keys_in, Values_in)),
          {ICMP_header, ICMP_data};
        Any -> {error, {unknown_icmp_type, Any}}
      end

  end.

%% Internals

%% End of Module.
