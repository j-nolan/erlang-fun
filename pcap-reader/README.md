# Pcap reader
## API methods
- Read `pcap` files
  * **Input**: a binary representation of a `pcap` file
  * **Output**: the global headers as a list of key-values
  * **Output**: a binary representation of the `pcaps` packets

- Split `pcap` packets
  * **Input**: a binary representation of a `pcap` packet
  * **Output**: A list of tuple (one for each packet). First element of each tuple contains the headers as a list of key-values of the packet, second contains the binary representation of the data of the packet

- A method that reads an IP packet and returns:
  * IP headers a a list of key-values
  * The content as binary

- A method that reads an ICMP packet and returns:
  * ICMP headers as list of key-values
   * The content as binary