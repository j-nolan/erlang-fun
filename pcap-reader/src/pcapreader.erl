-module(pcapreader).

%% pcap-reader: pcap-reader library's entry point.

-export([my_func/0]).


%% API

my_func() ->
    ok().

pcap_all_reader(Bin) ->
    try
        {ok, Pcap_global_headers, All_pcaps} = read_global_header(Bin),
        read_pcap_header(All_pcaps)
    catch
        error:{badmatch, Error} -> Error
    end.

read_global_header(Bin) ->
    case Bin of
        <<Headers:192/binary, All_pcaps/binary>> -> {ok, Headers, All_pcaps};
        Bin -> {error, {bad_global_pcap_header, Bin}}
    end.

read_pcap_header(Bin) ->
    case Bin of
        <<Any:0/binary>> -> ok;
        <<Ts_sec:32/integer, Ts_usec:32/integer, Incl_len:32/integer, Orig_len:32/integer, Pcap_content:Incl_len/binary, Next_packet/binary>> ->
            read_pcap_content(Pcap_content),
            read_pcap_header(Next_packet);
        Bin -> {error, {bad_pcap_header, Bin}} %% length!
    end.

read_pcap_content(Pcap_content) -> ok. %% TODO

%% Internals

ok() ->
    ok.

%% End of Module.
