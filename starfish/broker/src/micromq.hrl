% localhost or {0..255, 0..255, 0..255, 0..255}, eg {127, 0, 0, 1}
-define(ADDRESS, localhost).
% 0..65535
-define(PORT, 5017).
-define(MAX_TOPICS, 10000).
-define(MAX_CLIENTS, 10000).
% values: none, minimal, verbose, logs
-define(VERBOSITY, true).

-define(LOG(Fmt), io:format(Fmt)).
-define(LOG(Fmt, Args), io:format(Fmt, Args)).