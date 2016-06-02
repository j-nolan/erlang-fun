# Serveur
## Structures de données
- `client_records` : `Map(client_id => client_record)`
- `client_record` : `{client_id, Socket, List[topic_subscribed], List[topic_published], nb_msg_received, nb_msg_sent}`
- `topics` : `Map(topic => List[client_id])`
- `sockets` : `Map(Socket => client_id)`

`List[topic_subscribed]` dans le `client_record` contient l'information inverse de `topic`. Cette redondance est toutefois utile dans le cas où l'on veut accéder à la liste des sujets auxquels s'est inscrit un client. Si nous n'avions pas `List[topic_subscribed]`, il faudrait parcourir tous les topics et tous les clients de chaque topic pour retrouver cette information.

De même, la liste `sockets` est utilisée pour accéder au `client_id` associé à un socket en O(1), au lieu de devoir parcourir `client_records` en O(n).

## Scénarios réseau
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


### Client demande son statut
- Chercher le `client_id` dans `sockets`
- Récupérer le `client_record` associé
- Formater et envoyer les propriétés du `client_record`
