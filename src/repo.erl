-module(repo).
-compile(export_all).
-behaviour(gen_server).
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
init(_foo).
handle_info(_bar, _baz).
handle_cast(_x, _y).
handle_call(_z, _q, _w).
code_change(_e, _r, _t).
terminate(_sleep, _tight).

