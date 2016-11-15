% keeps track of the constant context stuff,
% this includes the current query, and the listener and repo PIDs
-record(context, {query, listener, repo}).
