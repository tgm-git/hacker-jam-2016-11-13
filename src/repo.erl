-module(repo).
-compile(export_all).

%% @doc stores the sites visited so that a page isn't visited twice.
%% recursive calls to itself ensures that it stays alive, 
%% while acting like a mutable store
%% @TODO Create CRUD messages
link_repo(Links) ->
    receive
        {From, {find, URL, Ref}} ->             % synchronous message
            case lists:member(URL, Links) of
                true  -> From ! {hit,  Ref};
                false -> From ! {miss, Ref}
            end,
            link_repo(Links);
        {store, URL} ->                         % asynchronous message
            link_repo([URL | Links]);
        terminate -> ok                         % termination message
    end.

