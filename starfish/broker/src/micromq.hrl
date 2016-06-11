% localhost or {0..255, 0..255, 0..255, 0..255}, eg {127, 0, 0, 1}
-define(ADDRESS, localhost).
% 0..65535
-define(PORT, 5017).
-define(MAX_TOPICS, 10000).
-define(MAX_CLIENTS, 10000).
% values: none (=quiet), minimal (generic 400 bad request), verbose (full 400 bad request with comments)
-define(VERBOSITY, none).
-define(LOGS, true).