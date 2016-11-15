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

    % spawn a new spider for each link found
    lists:foreach(SpawnSpider, Hyperlinks),
    % spawns a new spider search on each new hyperlink
    ok.

% TODO: Implement this
check_links([], RelevantURLs)   -> RelevantURLs;
check_links(URLs, RelevantURLs) -> ok.

%% @doc stores the sites visited so that a page isn't visited twice.
link_repo([]) -> ok;
link_repo(_Data) ->
    receive
        {find, _URL} -> ok;
        {store, _URL} -> ok
    end.

