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

# Server
## Data structures
- `client_records` : `Map(client_id => client_record)`
- `client_record` : `{client_id, Socket, List[topic_subscribed], List[topic_published], nb_msg_received, nb_msg_sent}`
- `topics` : `Map(topic => List[client_id])`
- `sockets` : `Map(Socket => client_id)`

The `List[topic_subscribed]` in the `client_record` contient l'information inverse de `topic`. Cette redondance est toutefois utile dans le cas où l'on veut accéder à la liste des sujets auxquels s'est inscrit un client. Si nous n'avions pas `List[topic_subscribed]`, il faudrait parcourir tous les topics et tous les clients de chaque topic pour retrouver cette information.

De même, la liste `sockets` est utilisée pour accéder au `client_id` associé à un socket en O(1), au lieu de devoir parcourir `client_records` en O(n).

## Network use cases
### Ouverture d'une socket (client se "connecte")
- Générer un `client_id`
- Générer un `client_record`
  * `{client_id, Socket, List[], List[], 0, 0}`
- Ajouter le `client_id` à `sockets`
- Ajouter le record à `client_records`

### Fermeture d'une socket (client se "déconnecte")
- Chercher le `client_id` dans `sockets`
- Récupérer le `client_record` associé
- Pour chaque topic de `List[topic_subscribed]`, supprimer `client_id` de la liste correspondante dans `topics`
- Supprimer `client_id` de la Map `client_records`
- Supprimer `Socket` de `sockets`

## Scénarios protocolaires
### Client se subscribe
- Chercher le `client_id` dans `sockets`
  * **Note** : le `client_id` existe forcément car il s'est connecté auparavant
- Parser la liste des topics
- Pour chaque topic
  * ajouter *si nécessaire* le `client_id` à la liste correspondante dans `topics`. Si le topic n'y est pas présent, créer d'abord une liste vide et y ajouter `client_id`
  * ajouter *si nécessaire* le topic dans `List[topic_subscribed]` du `client_record` associé au `client_id`

### Client publie
- Chercher le `client_id` dans `sockets`
- Récupérer le `client_record` associé
  * Incrémenter `nb_msg_sent`
  * Mettre à jour *si nécessaire* `List[topic_published]`
- Chercher la liste de `clients_id` correspondant au `topic` dans `topics`
- Pour chaque `client_id`, Récupérer le `client_record` associé
  * Envoyer le message sur la socket du client
  * Incrémenter `nb_msg_received`


### Client asks for status
- Retrieve `client_id` in `sockets`
- Retrieve corresponding `client_record`
- Format and send the `client_record`'s properties

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