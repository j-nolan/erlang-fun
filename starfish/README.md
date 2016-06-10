# Usage
## Setup
This programs has two dependencies:
- [`reloader`](https://bitbucket.org/marco_m/reloader) watches compiled sources and automatically reloads them in the erlang shell whenever they are changed
- [`meck`](https://github.com/eproxus/meck.git) is a mocking library for Erlang. (Note: do we really need it?)

To setup those dependencies, type from the server's directory:
- `rebar get-deps` (retrieve dependencies)
- `rebar update-deps` (update dependencies)

## Simple compilation
With this method, you need to repeat steps 2 and 3 every time you edit the sources.
1. Launch your terminal in the server's directory
2. Compile sources: `rebar compile`
3. Run program: `erl -pa ebin`

## Automatic reloading
With this method, whenever the sources are recompiled, the code is reloaded in the erlang shell using the `reloader` dependency.
1. Launch your terminal in the server's directory
2. Compile sources: `rebar compile` 
3. Run program with the reloader (2 seconds refresh): `erl -pa ebin -pa deps/*/ebin -run reloader start 2`

# API
## `start_link`
Spawns the server. The server in its turn spawn an acceptor process that will listen for new tcp connexions. Whenever a new client connects, the acceptor process spawns a worker process.

## `stop`
Retrieves the PID of the listener process and sends him a `stop` message.

# Technical considerations
## Message framing
According to the protocol, messages from clients are framed by double line-breaks.

The challenge is that `tcp` sends chunks of text that may contain:
- A partial frame
- One or more complete frame(s), often followed by a partial frame

This is how we parse messages frames with tcp chunks:
1. Read tcp chunk
2. Find first occurrence of a double line break
3. If an occurrence was found, extract frame until the line-break and remove it from the chunk
4. If not, read another tcp chunk and concatenate it to the previous tcp chunk
5. repeat from `2.` with the updated tcp chunk

## Bad protocol
The server should be designed not to crash when the client sends a message that doesn't follow the protocol's format. The specification does not tell how to respond to those messages.

We chose to discare message that don't match the protocol. Other approaches include sending back an error message to the client, to let him know what happened. This could be specified in future versions of the protocol.