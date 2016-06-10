-define(ADDRESS, localhost).
-define(PORT, 5017).
-define(MAX_TOPICS, 10000).
-define(MAX_CLIENTS, 10000).
-define(VERBOSITY, true).

-define(LOG(Fmt), io:format(Fmt)).
-define(LOG(Fmt, Args), io:format(Fmt, Args)).