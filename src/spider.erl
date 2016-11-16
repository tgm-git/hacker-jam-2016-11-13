-module(spider).
-compile(export_all).
-include("spider-context.hrl").

% added for convenience so it can be called without making the struct
init(Page, Lifetime, Query, Listener, Repo) ->
    Context = #context{query = Query, listener = Listener, repo = Repo},
    init(Page, Lifetime, Context).

init(Page, Lifetime, Context) ->
    spawn(?MODULE, search, [Page, Lifetime, Context]).

% TODO: Implement this
get_page(_URL) ->
    "<!DOCTYPE html><html><head></head><body></body></html>".

search(_URL, 0, _Context) -> 
    do_nothing;
search(URL, Lifetime, Context = #context{query = Query, listener = Pid, repo = RPid}) ->
    PageData = get_page(URL),
    {Hits, Hyperlinks} = page:eval(Query, PageData),
    if Hits =/= 0 -> Pid ! {page, URL} end,

    SpawnSpider = 
        fun (Link) -> init(Link, Lifetime - 1, Context) end,

    FilteredLinks = check_links(Hyperlinks, Context),

    % spawn a new spider for each link found
    lists:foreach(SpawnSpider, FilteredLinks),
    % spawns a new spider search on each new hyperlink
    ok.

check_links(Urls, Context) -> check_links(Urls, [], Context).

%% @doc sends a message to the link repository and checks if it exists
%% if it doesn't, it'll be added to RelevantURLs, if does it won't.
check_links([], RelevantURLs, _Context)   -> RelevantURLs;
check_links(URLs, RelevantURLs, Context) -> 
    [URLHead | URLTail] = URLs,
    Self = self(),
    %% make_ref is used to ensure the message is sent back
    %% to the correct receiver
    Ref = make_ref(),
    Context#context.repo ! {Self, {find, URLHead, Ref}},
    receive
        {hit, Ref}  -> check_links(URLTail, RelevantURLs)
        {miss, Ref} -> check_links(URLTail, [URLHead | RelevantURLs]);
    after 2000 -> timeout
    end.

%% @doc stores the sites visited so that a page isn't visited twice.
link_repo(Links) ->
    receive
        {From, {find, URL, Ref}} ->             % synchronous message
            case lists:member(URL, Links) of
                true  -> From ! {hit,  Ref};
                false -> From ! {miss, Ref}
            end;
        {store, URL} ->                         % asynchronous message
            link_repo([URL | Links]);
        terminate -> ok                         % termination message
    after 2000 -> timeout
    end.

