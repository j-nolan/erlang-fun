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

pcap_all_reader(Bin) ->
  try
    {ok, Pcap_global_headers, All_pcaps} = read_global_header(Bin),
    read_pcap_header(All_pcaps)
  catch
    error:{badmatch, Error} -> Error
  end.

read_global_header(Bin) ->
  case Bin of
    <<Headers:192, All_pcaps/binary>> -> {ok, Headers, All_pcaps};
    %% Advanced version, not used right now.
    % TODO: depending on magic, all fields are /little instead of default /big
    <<Magic_number:32, Version_major:16, Version_minor:16,
      Thiszone:32, Sigfigs:32, Snaplen:32, Network:32,
      All_pcaps/binary>> ->
      Headers = maps:new(),
      % TODO: does nothing for now.
      %Headers:put(magic_number, Magic_number),
      {ok, Headers, All_pcaps};
    Bin -> error(bad_global_pcap_header, Bin)
  end.

read_pcap_header(Bin) ->
  case Bin of
    <<Any:0/binary>> -> ok;
    % TODO: all PCAP fields are /little because of magic (hard-coded for now)
    <<Ts_sec:32/little, Ts_usec:32/little, Incl_len:32/little, Orig_len:32/little, Rest/binary>> ->
      case Rest of
        <<Pcap_content:Incl_len/binary, Next_packet/binary>> ->
          % TODO: do something with current packet and next packet
          read_pcap_content(Pcap_content),
          read_pcap_header(Next_packet);
        Rest -> error({bad_pcap_content, Incl_len, Bin})
      end;
    Bin -> error({bad_pcap_header, Bin}) %% length!
  end.

% Gets rid of a 32-bit marker at start of PCAP packet(maybe for type TCP/IP ?)
read_pcap_content(Pcap_content) ->
  case Pcap_content of
    <<Any:32, Datagram/binary>> -> read_IP_datagram(Datagram)
  end.

-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).
read_IP_datagram(Datagram) ->
  %PSize = byte_size(Pcap_content),
  case Datagram of
    <<?IP_VERSION:4, IHL:4, DS:8, Length:16,
      Identification:16, Flags:3, FragOffset:13,
      TTL:8, Protocol:8, HeaderChecksum:16,
      SourceIP:32,
      DestinationIP:32,
      Rest/binary>> when IHL >= ?IP_MIN_HDR_LEN -> %, 4 * IHL =< PSize ->
      OptsLen = 4 * (IHL - ?IP_MIN_HDR_LEN),
      <<Opts:OptsLen/binary, Payload/binary>> = Rest,
      % TODO: reads auto the content ? without knowing type ICMP ?
      read_payload(Rest);
    Bin -> {error, {bad_ip_header, Bin}}
  end.

% Read icmp payload
read_payload(Payload) ->
  case Payload of
    <<Type:8, Code:8, CheckSum:16, Data/binary>> ->
      case Type of
        0 ->
          <<Identifier:16, SeqNum:16, Dataa/binary>> = Data,
          ok;
        8 ->
          <<Identifier:16, SeqNum:16, Dataa/binary>> = Data,
          ok;
        Any -> {error, {unknown_icmp_type, Any}} %Dataa = Data
      end
  end.

%% Internals

%% End of Module.
