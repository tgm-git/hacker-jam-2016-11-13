-module(spider).
-compile(export_all).

init() ->
    ok.

search(_Pid, _Query, 0) -> ok;
search(_Pid, _Query, _Lifetime) ->
    ok.

%% @doc checks if the page contains the query.
relevant(_Query, _PageData) ->
    false.

%% @doc stores the sites visited so that a page isn't visited twice.
link_repo([]) -> ok;
link_repo(_Data) ->
    receive
        {find, _Site} -> ok;
        {store, _Site} -> ok
    end.

