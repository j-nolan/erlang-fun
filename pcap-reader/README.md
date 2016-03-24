# Pcap reader
## API methods
- Read `pcap` files
  * **Input**: a binary representation of a `pcap` file
  * **Output**: the global headers as a list of key-values
  * **Output**: a binary representation of the `pcaps` packets

- Split a list of `pcap` packets
  * **Input**: a binary representation of a list of `pcap` packets
  * **Output**: A list of tuples, one for each `pcap` packet. Each tuple contains a list of the packet's header as key-values, and the binary representation of the packet 

- Read an `IP` packet
  * **Input**: a binary representation of a a `pcap` packet
  * **Output**: IP headers a a list of key-values
  * **Output**: a binary representation of the `IP` packet's content

- Read an `ICMP` packet
  * **Input**: a binary representation of a an `ICMP` packet
  * **Output**: `ICMP` headers a a list of key-values
  * **Output**: a binary representation of the `ICMP` packet's content

## Ressources
- [The `pcap` file format](https://wiki.wireshark.org/Development/LibpcapFileFormat)
- [The `TCP/IP` protocol](http://www.networksorcery.com/enp/protocol/ip.htm)
- [The `ICMP` protcol](http://www.networksorcery.com/enp/protocol/icmp.htm)