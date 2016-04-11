# Pcap reader
## API methods
- Read `pcap` files
  * **Input**: a binary representation of a `pcap` file
  * **Output**: the global header as a map of key-values
  * **Output**: a binary representation of the `pcaps` packets

- Split a list of `pcap` packets
  * **Input**: a binary representation of a list of `pcap` packets
  * **Output**: A list of tuples, one for each `pcap` packet. Each tuple contains a map of the packet's header as key-values, and the decoded packet 
  * *Note* : This method could be split in two smaller methods: one that split the binary in packets and one that read the parses the packets. To be discussed
  
- Read an `IP` datagram
  * **Input**: a binary representation of a a `pcap` packet
  * **Output**: IP headers as a map of key-values
  * **Output**: a binary representation of the `IP` packet's content

- Read an `ICMP` packet
  * **Input**: a binary representation of a an `ICMP` packet
  * **Output**: `ICMP` headers as a map of key-values
  * **Output**: a binary representation of the `ICMP` packet's content

## Call hierarchy
We had to make a choice on how functions interact. Although each of them is assigned a specific task, the encapsulating nature of network layers inevitably make them depend on one another.
For example, we had two options to implement the IP datagram decoder:
1. The function decoding the `IP` datagram calls the function to decode the underlying `ICMP` packet.
2. The function returns the binary payload and lets the user call the `ICMP` decoder if necessary.

We chose to combine those approaches. The function `read_global_header` returns a binary representation of the encapsulated data. All other function return a decoded version of their payload.

## Visibility of function
Some functions are exported and others aren't. It depends on whether they make sense being used by the user of the API.

## Checksum
The `ICMP` and `IP` header checksums are not computed.

## Ressources
- [The `pcap` file format](https://wiki.wireshark.org/Development/LibpcapFileFormat)
- [The `TCP/IP` protocol](http://www.networksorcery.com/enp/protocol/ip.htm)
- [The `ICMP` protcol](http://www.networksorcery.com/enp/protocol/icmp.htm)