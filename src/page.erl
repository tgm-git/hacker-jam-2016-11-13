-module(page).
-compile(export_all).

%% @doc checks if the page contains the query.
eval(_Query, _PageData) ->
% use re module, to regex through the page data
% returns a tuple a number of hits and containing all hyperlinks
    {0, []}.
