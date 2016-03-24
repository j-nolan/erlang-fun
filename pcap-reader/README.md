# Pcap reader
## API
### Methods
A method that reads PCAP files and returns:
- Global headers as a list of key-value
- a binary containing all the pcaps packets

A method that reads a binary blob of pcap packets and returns:
- A list of tuple. First element of each tuple contains the headers as a list of key-values of the packet, second contains the data as binary

A method that reads an IP packet and returns:
- IP headers a a list of key-values
- The content as binary

A method that reads an ICMP packet and returns:
- ICMP headers as list of key-values
- The content as binary